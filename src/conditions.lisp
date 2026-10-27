(in-package #:surgeon)

;;;; -- Conditions --

(define-condition surgeon-error (error)
  ((message
    :initarg       :message
    :reader        surgeon-error-message
    :type          string
    :documentation "A complete sentence explaining the failure."))
  (:report (lambda (condition stream)
             (write-string (surgeon-error-message condition) stream)))
  (:documentation "The parent of every condition surgeon signals."))

(define-condition form-count-error (surgeon-error)
  ((source
    :initarg       :source
    :reader        form-count-error-source
    :type          string
    :documentation "The source text that did not hold exactly one form."))
  (:documentation "Source expected to hold exactly one form held none or several."))

(define-condition restart-choice-available (surgeon-error)
  ((condition
    :initarg       :condition
    :reader        restart-choice-available-condition
    :documentation "The condition whose restarts were not chosen.")
   (choices
    :initarg       :choices
    :reader        restart-choice-available-choices
    :type          list
    :documentation "The (NAME . REPORT) pairs of the restarts a caller may choose."))
  (:documentation
   "An error offered restarts while no requested restart matched one of them.

It is signaled from inside the handler, after the restarts have been
recorded, so a caller can describe them and retry naming one."))

(define-condition definition-irreversible (surgeon-error)
  ((definition
    :initarg       :definition
    :reader        definition-irreversible-definition
    :documentation "The definition whose installation could not be made reversible."))
  (:documentation "No exact undo action can be captured before installing a definition."))

(define-condition definition-unsupported (surgeon-error)
  ((form
    :initarg       :form
    :reader        definition-unsupported-form
    :documentation "The form that is not one supported complete definition."))
  (:documentation "A form is not a definition whose operator surgeon accepts."))

(define-condition definition-not-found (surgeon-error)
  ((pathname
    :initarg       :pathname
    :reader        definition-not-found-pathname
    :type          pathname
    :documentation "The source file searched.")
   (definition
    :initarg       :definition
    :reader        definition-not-found-definition
    :documentation "The definition whose signature no top-level form matched."))
  (:documentation "A source file holds no top-level form matching a definition."))
