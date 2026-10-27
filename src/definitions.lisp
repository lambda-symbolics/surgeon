(in-package #:surgeon)

;;;; -- Definition Forms --

(defparameter *standard-definition-operators*
  '(defun defgeneric defmethod defmacro defclass defstruct define-condition
    deftype define-compiler-macro defvar defparameter)
  "The standard top-level defining operators surgeon understands.")

(defgeneric definition-operator-p (operator)
  (:documentation
   "Return true when OPERATOR is a supported top-level defining operator.

Hosts add an EQL method for each defining macro of their own, together with a
DEFINITION-UNDO-CAPTURE method that makes its installation reversible."))

(defmethod definition-operator-p (operator)
  "Accept the standard defining operators."
  (and (member operator *standard-definition-operators* :test #'eq) t))

(-> definition-name-p (t) boolean)
(defun definition-name-p (value)
  "Return true when VALUE is a symbol or a two-element SETF function name."
  (or (symbolp value)
      (and (consp value)
           (eq (first value) 'setf)
           (consp (rest value))
           (null (rest (rest value)))
           (symbolp (second value)))))

(-> definition-form-p (t) boolean)
(defun definition-form-p (form)
  "Return true when FORM is one supported complete top-level definition."
  (and (consp form)
       (symbolp (first form))
       (consp (rest form))
       (definition-operator-p (first form))
       (definition-name-p (second form))))

(-> definition-name-symbol (t) symbol)
(defun definition-name-symbol (name)
  "Return the symbol identifying definition NAME, unwrapping SETF names."
  (if (consp name)
      (second name)
      name))

(-> definition-home-package-name (list) (or null string))
(defun definition-home-package-name (definition)
  "Return the home package name of DEFINITION's target symbol, or NIL when uninterned."
  (let ((home (symbol-package (definition-name-symbol (second definition)))))
    (and home (package-name home))))

(-> call-with-definition-unlocked (list package function) t)
(defun call-with-definition-unlocked (definition package thunk)
  "Call THUNK with reader PACKAGE and DEFINITION's home package unlocked."
  (let ((home (symbol-package (definition-name-symbol (second definition)))))
    (call-with-package-unlocked
     package
     (lambda ()
       (if (and home (not (eq home package)))
           (call-with-package-unlocked home thunk)
           (funcall thunk))))))

(-> method-specializers (list) list)
(defun method-specializers (specialized-lambda-list)
  "Return the required specializers of SPECIALIZED-LAMBDA-LIST without parameter names."
  (loop for parameter in specialized-lambda-list
        until (member parameter lambda-list-keywords :test #'eq)
        collect (if (consp parameter)
                    (second parameter)
                    t)))

(-> definition-signature (list) list)
(defun definition-signature (definition)
  "Return the semantic identity of top-level DEFINITION.

A method is identified by its generic function, qualifiers and required
specializers; every other definition by its operator and name."
  (if (eq (first definition) 'defmethod)
      (multiple-value-bind (qualifiers lambda-list) (definition--method-parts definition)
        (list (first definition)
              (second definition)
              qualifiers
              (method-specializers lambda-list)))
      (list (first definition) (second definition))))

(-> definition-key (list) string)
(defun definition-key (definition)
  "Return a stable readable key for DEFINITION's signature, printed relative to *PACKAGE*."
  (write-to-string (definition-signature definition) :readably t :case :downcase))

(-> definition--method-parts (list) (values list list))
(defun definition--method-parts (definition)
  "Return the qualifiers and specialized lambda list of DEFMETHOD form DEFINITION."
  (let* ((tail            (rest (rest definition)))
         (lambda-position (position-if #'listp tail)))
    (unless lambda-position
      (error 'definition-unsupported :form    definition
                                     :message "The DEFMETHOD form has no lambda list."))
    (values (subseq tail 0 lambda-position)
            (nth lambda-position tail))))
