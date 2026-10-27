(in-package #:surgeon)

;;;; -- Test Registry --

(defvar *test-cases* nil
  "The registered test names and functions, newest first.")

(defmacro define-test (name &body body)
  "Define and register one test named NAME."
  `(progn
     (setf *test-cases* (remove ',name *test-cases* :key #'first))
     (push (list ',name (lambda () ,@body)) *test-cases*)
     ',name))

(defmacro test-assert (form &optional description)
  "Signal a test failure unless FORM returns true."
  `(unless ,form
     (error "Assertion failed: ~A~@[ (~A)~]" ',form ,description)))

(defmacro test-equal (expected form &key (test '#'equal) description)
  "Signal a test failure, naming DESCRIPTION, unless FORM equals EXPECTED under TEST."
  (let ((expected-value (gensym "EXPECTED"))
        (actual-value   (gensym "ACTUAL")))
    `(let ((,expected-value ,expected)
           (,actual-value ,form))
       (unless (funcall ,test ,expected-value ,actual-value)
         (error "Expected ~S, got ~S from ~S~@[ (~A)~]."
                ,expected-value ,actual-value ',form ,description))
       ,actual-value)))

(defmacro test-signals (condition-type &body body)
  "Evaluate BODY and return the signaled CONDITION-TYPE condition."
  (let ((condition (gensym "CONDITION")))
    `(handler-case
         (progn
           ,@body
           (error "Expected condition ~S, but none was signaled." ',condition-type))
       (,condition-type (,condition)
         ,condition))))

(-> call-with-scratch-package (function) t)
(defun call-with-scratch-package (function)
  "Call FUNCTION with a fresh package using COMMON-LISP, deleting it afterwards."
  (let ((package (make-package (format nil "SURGEON-SCRATCH-~D" (random most-positive-fixnum))
                               :use '(#:cl))))
    (unwind-protect (funcall function package)
      (delete-package package))))

(-> scratch-evaluate (package string) t)
(defun scratch-evaluate (package source)
  "Read SOURCE in PACKAGE and evaluate it there."
  (let ((*package* package))
    (eval (read-one-form source :package package))))
