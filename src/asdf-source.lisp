(in-package #:surgeon)

(-> asdf-source-pathnames (asdf:component) list)
(defun asdf-source-pathnames (component)
  "Return existing Common Lisp source files below COMPONENT in ASDF order.

Prune components whose :IF-FEATURE expression is false before probing files."
  (let ((feature (asdf/component:component-if-feature component)))
    (cond
      ((and feature (not (uiop:featurep feature)))
       nil)
      ((typep component 'asdf:cl-source-file)
       (let ((pathname (asdf:component-pathname component)))
         (when (probe-file pathname)
           (list pathname))))
      ((typep component 'asdf:module)
       (mapcan #'asdf-source-pathnames (asdf:component-children component))))))

(-> asdf-direct-dependency-names (asdf:system) list)
(defun asdf-direct-dependency-names (system)
  "Return SYSTEM's direct dependency names applicable on this host, in declaration order."
  (labels ((dependency-name (specification)
             "Normalize a name, a version wrapper, or an applicable feature wrapper."
             (cond
               ((or (stringp specification) (symbolp specification))
                (string-downcase (string specification)))
               ((and (consp specification) (eq (first specification) ':version))
                (dependency-name (second specification)))
               ((and (consp specification) (eq (first specification) ':feature))
                (and (uiop:featurep (second specification))
                     (dependency-name (third specification)))))))
    (remove nil (mapcar #'dependency-name (asdf:system-depends-on system)))))
