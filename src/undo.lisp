(in-package #:surgeon)

;;;; -- Reversible Redefinition --

;;; Before a definition is installed, DEFINITION-UNDO-CAPTURE records exactly
;;; what it is about to replace and returns a function that puts it back:
;;; function bindings, a single method, variable values with SBCL's recorded
;;; variable kind, type expanders, and, for classes, either reinstallation of
;;; the previous source or removal of the class and of every function the new
;;; definition added to its package.

(defgeneric definition-undo-capture (operator definition package &key previous-source)
  (:documentation
   "Capture the state that installing DEFINITION in PACKAGE replaces and return an undo function.

OPERATOR is DEFINITION's first element. PREVIOUS-SOURCE, when known, is the
complete source text of the definition being replaced; class redefinitions need
it to undo an existing class. The returned function takes no arguments and
restores the captured state with the involved packages unlocked. Hosts add an
EQL method for each defining operator of their own; the default method signals
DEFINITION-IRREVERSIBLE."))

(defmethod definition-undo-capture (operator definition package &key previous-source)
  "Refuse a definition kind without a reversible installation strategy."
  (declare (ignore operator package previous-source))
  (error 'definition-irreversible
         :definition definition
         :message "The definition kind has no reversible installation strategy."))

(defmethod definition-undo-capture :around (operator definition package &key previous-source)
  "Run the captured undo function with DEFINITION's packages unlocked."
  (declare (ignore operator previous-source))
  (let ((undo (call-next-method)))
    (lambda ()
      (call-with-definition-unlocked definition package undo))))

(defmethod definition-undo-capture ((operator (eql 'defun)) definition package &key previous-source)
  "Restore the replaced function binding, or its absence."
  (declare (ignore package previous-source))
  (undo--function-binding (second definition)))

(defmethod definition-undo-capture ((operator (eql 'defgeneric)) definition package
                                    &key previous-source)
  "Restore the replaced generic function binding, or its absence."
  (declare (ignore package previous-source))
  (undo--function-binding (second definition)))

(defmethod definition-undo-capture ((operator (eql 'defmacro)) definition package &key previous-source)
  "Restore the replaced macro binding, or its absence."
  (declare (ignore package previous-source))
  (undo--function-binding (second definition)))

(defmethod definition-undo-capture ((operator (eql 'define-compiler-macro)) definition package
                                    &key previous-source)
  "Restore the replaced compiler macro, or its absence."
  (declare (ignore package previous-source))
  (let* ((name (second definition))
         (binding (compiler-macro-function name)))
    (lambda ()
      (setf (compiler-macro-function name) binding))))

(defmethod definition-undo-capture ((operator (eql 'defmethod)) definition package
                                    &key previous-source)
  "Remove the installed method, reinstate the method it replaced, and drop a new generic function."
  (declare (ignore previous-source))
  (let ((name (second definition)))
    (multiple-value-bind (generic-function method) (undo--find-method definition package)
      (let ((generic-function-existed-p (typep generic-function 'generic-function)))
        (lambda ()
          (multiple-value-bind (current-generic-function current-method)
              (undo--find-method definition package)
            (when current-method
              (remove-method current-generic-function current-method))
            (when method
              (add-method current-generic-function method))
            (unless generic-function-existed-p
              (when (fboundp name)
                (fmakunbound name)))))))))

(defmethod definition-undo-capture ((operator (eql 'defvar)) definition package &key previous-source)
  "Restore the replaced value and SBCL's recorded variable kind."
  (declare (ignore package previous-source))
  (undo--variable (second definition)))

(defmethod definition-undo-capture ((operator (eql 'defparameter)) definition package
                                    &key previous-source)
  "Restore the replaced value and SBCL's recorded variable kind."
  (declare (ignore package previous-source))
  (undo--variable (second definition)))

(defmethod definition-undo-capture ((operator (eql 'deftype)) definition package &key previous-source)
  "Restore SBCL's recorded type kind, expander and source location, then drop type caches."
  (declare (ignore package previous-source))
  (let* ((name (second definition))
         (snapshots (loop for kind in '(:kind :expander :source-location)
                          collect (cons kind (multiple-value-list (sb-int:info :type kind name))))))
    (lambda ()
      (loop for (kind . snapshot) in snapshots
            do (undo--restore-info name :category ':type :kind kind :snapshot snapshot))
      (sb-kernel:%note-type-defined name))))

(defmethod definition-undo-capture ((operator (eql 'defclass)) definition package
                                    &key previous-source)
  "Reinstall the previous class source, or remove a new class and its added functions."
  (undo--class definition package previous-source))

(defmethod definition-undo-capture ((operator (eql 'defstruct)) definition package
                                    &key previous-source)
  "Reinstall the previous structure source, or remove a new structure and its added functions."
  (undo--class definition package previous-source))

(defmethod definition-undo-capture ((operator (eql 'define-condition)) definition package
                                    &key previous-source)
  "Reinstall the previous condition source, or remove a new condition and its added functions."
  (undo--class definition package previous-source))


;;;; -- Binding Snapshots --

(-> function-binding-snapshot (t) list)
(defun function-binding-snapshot (name)
  "Return a snapshot of function NAME's exact binding, macro or function, or of its absence."
  (let ((macro (and (symbolp name) (macro-function name))))
    (cond
      (macro
       (list ':macro macro))
      ((fboundp name)
       (list ':function (fdefinition name)))
      (t
       (list nil nil)))))

(-> function-binding-restore (t list) null)
(defun function-binding-restore (name snapshot)
  "Restore function NAME to SNAPSHOT, replacing whatever binding it has now."
  (destructuring-bind (kind binding) snapshot
    (unless (and (eq kind ':function)
                 (fboundp name)
                 (not (and (symbolp name) (macro-function name))))
      (when (fboundp name)
        (fmakunbound name)))
    (case kind
      (:macro
       (setf (macro-function name) binding))
      (:function
       (setf (fdefinition name) binding))))
  nil)

(-> value-binding-snapshot (symbol) list)
(defun value-binding-snapshot (symbol)
  "Return a snapshot of SYMBOL's exact global value, or of its absence."
  (let ((bound-p (boundp symbol)))
    (list bound-p (and bound-p (symbol-value symbol)))))

(-> value-binding-restore (symbol list) null)
(defun value-binding-restore (symbol snapshot)
  "Restore SYMBOL's global value to SNAPSHOT."
  (destructuring-bind (bound-p value) snapshot
    (if bound-p
        (setf (symbol-value symbol) value)
        (when (boundp symbol)
          (makunbound symbol))))
  nil)

(-> package-functions-snapshot (package) hash-table)
(defun package-functions-snapshot (package)
  "Return snapshots of the function bindings of every symbol whose home is PACKAGE."
  (let ((snapshot (make-hash-table :test #'eq)))
    (do-symbols (symbol package)
      (when (eq (symbol-package symbol) package)
        (setf (gethash symbol snapshot) (function-binding-snapshot symbol))))
    snapshot))

(-> package-functions-restore (package hash-table) null)
(defun package-functions-restore (package snapshot)
  "Restore PACKAGE's function bindings to SNAPSHOT, unbinding functions it did not hold."
  (do-symbols (symbol package)
    (when (eq (symbol-package symbol) package)
      (multiple-value-bind (state present-p) (gethash symbol snapshot)
        (if present-p
            (function-binding-restore symbol state)
            (when (fboundp symbol)
              (fmakunbound symbol))))))
  nil)


;;;; -- Undo Strategies --

(-> undo--function-binding (t) function)
(defun undo--function-binding (name)
  "Return an undo function restoring NAME's current function binding."
  (let ((snapshot (function-binding-snapshot name)))
    (lambda ()
      (function-binding-restore name snapshot))))

(-> undo--variable (symbol) function)
(defun undo--variable (symbol)
  "Return an undo function restoring SYMBOL's value and recorded variable kind."
  (let ((value (value-binding-snapshot symbol))
        (kind (multiple-value-list (sb-int:info :variable :kind symbol))))
    (lambda ()
      (value-binding-restore symbol value)
      (undo--restore-info symbol :category ':variable :kind ':kind :snapshot kind))))

(-> undo--class (list package (or null string)) function)
(defun undo--class (definition package previous-source)
  "Return an undo function for class-like DEFINITION in PACKAGE.

An existing class can only be restored from PREVIOUS-SOURCE, since a class
object cannot be reinstated in place; without it the installation is refused."
  (let ((name (second definition))
        (functions (package-functions-snapshot package)))
    (when (and (find-class name nil) (null previous-source))
      (error 'definition-irreversible
             :definition definition
             :message "The existing class has no recoverable source, so its redefinition cannot be made reversible."))
    (lambda ()
      (if previous-source
          (let ((*package* package))
            (eval (read-one-form previous-source :read-eval nil :package package)))
          (progn
            (when (find-class name nil)
              (setf (find-class name) nil))
            (package-functions-restore package functions))))))

(-> undo--restore-info (symbol &key (:category keyword) (:kind keyword) (:snapshot list)) null)
(defun undo--restore-info (name &key category kind snapshot)
  "Restore SBCL's global database entry CATEGORY and KIND for NAME from SNAPSHOT."
  (if (second snapshot)
      (setf (sb-int:info category kind name) (first snapshot))
      (sb-int:clear-info category kind name))
  nil)

(-> undo--method-components (list package) (values t list list))
(defun undo--method-components (definition package)
  "Return DEFMETHOD DEFINITION's generic function name, qualifiers and specializer objects."
  (multiple-value-bind (qualifiers lambda-list) (definition--method-parts definition)
    (values
     (second definition)
     qualifiers
     (loop for specializer in (method-specializers lambda-list)
           collect (cond
                     ((eq specializer t)
                      (find-class 't))
                     ((and (consp specializer) (eq (first specializer) 'eql))
                      (closer-mop:intern-eql-specializer
                       (let ((*package* package))
                         (eval (second specializer)))))
                     ((symbolp specializer)
                      (find-class specializer))
                     (t
                      (find-class (let ((*package* package))
                                    (eval specializer)))))))))

(-> undo--find-method (list package) (values t t))
(defun undo--find-method (definition package)
  "Return the generic function DEFINITION extends and its current matching method, if any."
  (multiple-value-bind (name qualifiers specializers) (undo--method-components definition package)
    (let ((generic-function (and (fboundp name) (fdefinition name))))
      (values generic-function
              (and (typep generic-function 'generic-function)
                   (find-method generic-function qualifiers specializers nil))))))
