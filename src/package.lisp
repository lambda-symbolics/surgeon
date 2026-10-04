(defpackage #:surgeon
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:export
   ;; Conditions
   #:surgeon-error
   #:surgeon-error-message
   #:form-count-error
   #:form-count-error-source
   #:restart-choice-available
   #:restart-choice-available-condition
   #:restart-choice-available-choices
   #:definition-irreversible
   #:definition-irreversible-definition
   #:definition-unsupported
   #:definition-unsupported-form
   ;; Packages, forms and symbols
   #:package-find
   #:call-with-package-unlocked
   #:read-one-form
   #:resolve-symbol
   #:*definition-kinds*
   #:symbol-definition-kinds
   #:symbol-defined-p
   #:symbol-lambda-list
   #:symbol-definition-pathname
   #:symbol-documentation-line
   #:symbol-label
   #:package-present-symbols
   #:package-apropos
   #:package-symbol-suggestions
   ;; Restarts
   #:restart-choices
   #:restart-find
   #:call-with-restart-choice
   ;; Definitions
   #:*standard-definition-operators*
   #:definition-operator-p
   #:definition-name-p
   #:definition-form-p
   #:definition-name-symbol
   #:definition-home-package-name
   #:call-with-definition-unlocked
   #:method-specializers
   #:definition-signature
   #:definition-key
   ;; Reversible redefinition
   #:definition-undo-capture
   #:function-binding-snapshot
   #:function-binding-restore
   #:value-binding-snapshot
   #:value-binding-restore
   #:package-functions-snapshot
   #:package-functions-restore))
