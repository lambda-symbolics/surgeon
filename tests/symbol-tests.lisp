(in-package #:surgeon)

;;;; -- Symbol and Restart Tests --

(define-test symbol-definition-kinds-and-apropos
  (call-with-scratch-package
   (lambda (package)
     (dolist (source '("(defun widget-make (size) \"Make a widget.\" size)"
                       "(defmacro with-widget ((name) &body body) `(let ((,name 1)) ,@body))"
                       "(defgeneric widget-size (widget))"
                       "(defvar *widget-count* 0)"
                       "(defclass widget () ())"
                       "(define-condition widget-error (error) ())"
                       "(deftype widget-id () 'integer)"
                       "(defun gadget-make () nil)"))
       (scratch-evaluate package source))
     (flet ((kinds (name)
              (symbol-definition-kinds (find-symbol name package))))
       (loop for (name expected) in '(("WIDGET-MAKE" (:function))
                                      ("WITH-WIDGET" (:macro))
                                      ("WIDGET-SIZE" (:generic-function))
                                      ("*WIDGET-COUNT*" (:variable))
                                      ("WIDGET" (:class))
                                      ("WIDGET-ERROR" (:condition))
                                      ("WIDGET-ID" (:type)))
             do (test-equal expected (kinds name) :description name)))
     (test-equal '(:function) (symbol-definition-kinds 'car))
     (test-assert (null (symbol-definition-kinds :keyword)) "keywords carry no definitions")
     (test-equal '("SIZE") (mapcar #'symbol-name
                                   (symbol-lambda-list (find-symbol "WIDGET-MAKE" package))))
     (test-equal '("WIDGET") (mapcar #'symbol-name
                                     (symbol-lambda-list (find-symbol "WIDGET-SIZE" package))))
     (test-equal "Make a widget."
                 (symbol-documentation-line (find-symbol "WIDGET-MAKE" package) '(:function)))
     (test-equal "Make..."
                 (symbol-documentation-line (find-symbol "WIDGET-MAKE" package) '(:function)
                                            :limit 4))
     (intern "WIDGET-UNDEFINED" package)
     (test-equal '("*WIDGET-COUNT*" "WIDGET" "WIDGET-ERROR" "WIDGET-ID" "WIDGET-MAKE"
                   "WIDGET-SIZE" "WITH-WIDGET")
                 (mapcar (lambda (match) (symbol-name (first match)))
                         (package-apropos "widget" :package package))
                 :description "apropos lists defined names only, sorted")
     (test-equal '("WIDGET-MAKE")
                 (mapcar (lambda (match) (symbol-name (first match)))
                         (package-apropos "MAKE wid" :package package :kind ':function)))
     (test-equal '("WIDGET-MAKE")
                 (mapcar #'symbol-name (package-symbol-suggestions "widget-mak" :package package
                                                                                 :limit 1)))
     (test-assert (member "GADGET-MAKE"
                          (mapcar #'symbol-name (package-symbol-suggestions "gadget-make-it"
                                                                            :package package))
                          :test #'string=)
                  "near misses share a token")
     (let ((names (mapcar #'symbol-name
                          (package-symbol-suggestions "make-hash-tabel"
                                                      :package (find-package '#:cl)))))
       (test-equal "MAKE-HASH-TABLE" (first names))
       (test-assert (notany (lambda (name) (< (length name) 3)) names)
                    "one-letter names are never near misses"))
     (test-equal "car" (symbol-label 'car (find-package '#:cl)))
     (test-equal "common-lisp:car" (symbol-label 'car package)))))

(define-test package-and-form-reading
  (call-with-scratch-package
   (lambda (package)
     (test-assert (eq package (package-find (string-downcase (package-name package)))))
     (test-assert (null (package-find "SURGEON-NO-SUCH-PACKAGE")))
     (test-equal '(+ 1 2) (read-one-form "(+ 1 2)" :package package))
     (test-signals form-count-error (read-one-form "1 2" :package package))
     (test-signals form-count-error (read-one-form "  " :package package))
     (test-signals reader-error (read-one-form "#.(+ 1 2)" :read-eval nil :package package))
     (test-assert (eq 'car (resolve-symbol "#'car" :package package)))
     (test-assert (eq 'car (resolve-symbol "'car" :package package)))
     (test-signals surgeon-error (resolve-symbol "(car)" :package package))
     (let ((locked (find-package '#:cl)))
       (call-with-package-unlocked locked
                                   (lambda ()
                                     (test-assert (not (sb-ext:package-locked-p locked)))))
       (test-assert (sb-ext:package-locked-p locked) "the lock is restored")))))

(define-test restart-choice
  (flet ((offer ()
           (restart-case (error "Need a value.")
             (use-value (value)
               :report "Use a value."
               value)
             (abort ()
               :report "Abort."
               :aborted))))
    (test-equal 42 (call-with-restart-choice #'offer
                                             :restart-name "use-value"
                                             :restart-value-function (constantly 42)))
    (let ((condition (test-signals restart-choice-available (call-with-restart-choice #'offer))))
      (test-equal '("USE-VALUE" . "Use a value.")
                  (first (restart-choice-available-choices condition)))
      (test-assert (not (assoc "ABORT" (restart-choice-available-choices condition)
                               :test #'string=))
                   "ABORT is never offered")
      (test-assert (typep (restart-choice-available-condition condition) 'simple-error)))
    (test-signals restart-choice-available
      (call-with-restart-choice #'offer :restart-name "abort"))))
