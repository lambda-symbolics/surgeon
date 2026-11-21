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
                                    :end   (source-form-end source-form))
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

(define-test source-definition-index-order-and-keys
  (let* ((prefix     (format nil "surgeon-index-~D" (random most-positive-fixnum)))
         (first-path (merge-pathnames (format nil "~A-a.lisp" prefix) (uiop:temporary-directory)))
         (last-path  (merge-pathnames (format nil "~A-z.lisp" prefix) (uiop:temporary-directory)))
         (primary    "(defmethod speak ((x string)) x)"))
    (unwind-protect
         (progn
           (with-open-file (stream first-path :direction :output :if-exists :error)
             (write-string *source-form-tests-text* stream)
             (write-line "(defun (setf speak) (value object) (list value object))" stream))
           (with-open-file (stream last-path :direction :output :if-exists :error)
             (write-line "(defmethod speak ((x integer)) x)" stream))
           (let* ((paths   (list last-path first-path))
                  (index   (source-definition-index paths :package (find-package :surgeon)))
                  (records (gethash 'speak index))
                  (key     (definition-key '(defmethod speak ((x string)) nil))))
             (test-equal (list first-path first-path last-path)
                         (mapcar #'source-definition-pathname records))
             (test-equal primary
                         (source-definition-text
                          (find key records :test #'string= :key #'source-definition-key)))
             (test-equal '(setf speak)
                         (source-definition-name (first (gethash '(setf speak) index))))
             (let ((filtered (source-definition-index paths
                                                      :package (find-package :surgeon)
                                                      :name    'speak)))
               (test-equal 1 (hash-table-count filtered))
               (test-equal (mapcar #'source-definition-text records)
                           (mapcar #'source-definition-text (gethash 'speak filtered)))))
           (with-open-file (stream last-path :direction :output :if-exists :supersede)
             (write-string "(" stream))
           (test-signals end-of-file
             (source-definition-index (list first-path last-path) :name 'speak)))
      (dolist (pathname (list first-path last-path))
        (when (probe-file pathname)
          (delete-file pathname))))))

(define-test source-forms-equal-with-reader
  (flet ((read-safely (source)
           (read-one-form source :read-eval nil)))
    (test-assert (source-forms-equal-p "(list 1)" "(list  ; comment
 1)" #'read-safely))
    (test-assert (source-forms-equal-p "(" "(" #'read-safely))
    (test-equal nil (source-forms-equal-p "(" "( " #'read-safely))
    (test-equal nil (source-forms-equal-p nil "nil" #'read-safely))))

(define-test asdf-source-selection
  (let ((root (asdf:system-source-directory (asdf:find-system :surgeon)))
        (name "surgeon-source-selection-fixture"))
    (unwind-protect
         (progn
           (eval `(asdf:defsystem ,name
                    :pathname ,root
                    :depends-on ("Simple" (:version "Versioned" "1")
                                 (:feature (:and) (:version "Nested" "2"))
                                 (:feature (:or) "Withheld"))
                    :components ((:file "src/source-forms")
                                 (:file "absent-fixture")
                                 (:module "private" :pathname "src/" :if-feature (:or)
                                  :components ((:file "undo"))))))
           (let ((system (asdf:find-system name)))
             (test-equal (list (merge-pathnames "src/source-forms.lisp" root))
                         (asdf-source-pathnames system))
             (test-equal '("simple" "versioned" "nested") (asdf-direct-dependency-names system))))
      (asdf:clear-system name))))
