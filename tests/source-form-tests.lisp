(in-package #:surgeon)

;;;; -- Source Form Tests --

(defparameter *source-form-tests-text*
  ";;;; Header
(defun first-function () 1) ; trailing comment

#| a block #| nested |# comment |#
(defmethod speak :before ((x string)) x)
(defmethod speak ((x string)) x)
;; The variable.
(defvar *setting* 1)
"
  "A source file with comments around and between its top-level forms.")

(define-test source-form-spans
  (let ((forms (source-read-forms *source-form-tests-text*)))
    (test-equal 4 (length forms))
    (dolist (source-form forms)
      (test-equal (source-form-form source-form)
                  (read-from-string *source-form-tests-text* t nil
                                    :start (source-form-start source-form)
                                    :end (source-form-end source-form))
                  :description "each span holds exactly its form"))
    (test-equal "(defmethod speak ((x string)) x)"
                (let ((source-form (third forms)))
                  (subseq *source-form-tests-text*
                          (source-form-start source-form) (source-form-end source-form)))))
  (test-signals surgeon-error (source-read-forms "#| unterminated")))

(define-test source-definition-replacement
  (let ((pathname (merge-pathnames (format nil "surgeon-~D.lisp" (random most-positive-fixnum))
                                   (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (stream pathname :direction :output :if-exists :supersede)
             (write-string *source-form-tests-text* stream))
           (multiple-value-bind (updated previous)
               (source-replace-definition pathname "(defmethod speak ((x string)) (list x))")
             (test-equal "(defmethod speak ((x string)) x)" previous)
             (test-assert (search "(defmethod speak :before ((x string)) x)" updated)
                          "a method with other qualifiers is untouched")
             (test-assert (search "; trailing comment" updated) "comments are preserved")
             (test-equal updated (uiop:read-file-string pathname)))
           (test-signals definition-not-found
             (source-find-definition pathname '(defun missing () nil)))
           (test-signals definition-unsupported
             (source-replace-definition pathname "(progn 1)"))
           (test-signals form-count-error
             (source-replace-definition pathname "(defun a ()) (defun b ())")))
      (when (probe-file pathname)
        (delete-file pathname)))))
