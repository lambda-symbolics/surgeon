(asdf:defsystem #:surgeon
  :description "Introspection and reversible redefinition for live SBCL images."
  :author "Lambda Symbolics OÜ"
  :license "COLL-Attribution"
  :version "0.1.0"
  :serial t
  :depends-on (#:closer-mop
               #:ls-compat/files
               #:sb-introspect
               #:serapeum)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "conditions")
                             (:file "symbols")
                             (:file "restarts")
                             (:file "definitions")
                             (:file "undo")
                             (:file "source-forms"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:surgeon/tests))))

(asdf:defsystem #:surgeon/tests
  :description "Tests for surgeon."
  :depends-on (#:surgeon)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "test-support")
                             (:file "symbol-tests")
                             (:file "definition-tests")
                             (:file "source-form-tests")
                             (:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:surgeon '#:run-tests)))
