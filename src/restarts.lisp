(in-package #:surgeon)

;;;; -- Non-Interactive Restart Selection --

;;; A program that cannot answer a debugger still benefits from restarts: it
;;; runs an operation once to learn which restarts an error offers, then runs it
;;; again naming the restart to invoke while the signaling frame is still live.

(-> restart-choices (condition) list)
(defun restart-choices (condition)
  "Return (NAME . REPORT) pairs for CONDITION's named restarts other than ABORT.

ABORT is excluded because invoking it unwinds the caller's own loop instead of
correcting the failed operation."
  (loop for restart in (compute-restarts condition)
        for name = (restart-name restart)
        when (and name (not (eq name 'abort)))
          collect (cons (symbol-name name) (princ-to-string restart))))

(-> restart-find (condition string) (or null restart))
(defun restart-find (condition name)
  "Return CONDITION's first restart other than ABORT whose name is NAME, ignoring case."
  (find-if (lambda (restart)
             (let ((restart-name (restart-name restart)))
               (and restart-name
                    (not (eq restart-name 'abort))
                    (string-equal (symbol-name restart-name) name))))
           (compute-restarts condition)))

(-> call-with-restart-choice
    (function &key (:restart-name (or null string)) (:restart-value-function (or null function)))
    t)
(defun call-with-restart-choice (thunk &key restart-name restart-value-function)
  "Call THUNK, invoking the chosen restart of an error it signals.

With RESTART-NAME, an error offering a matching restart invokes it while the
signaling operation is live, passing the value RESTART-VALUE-FUNCTION returns
when that function is given. Any other error that offers restarts signals
RESTART-CHOICE-AVAILABLE from inside the handler, carrying the original
condition and its RESTART-CHOICES. Errors without restarts pass through."
  (handler-bind
      ((error
         (lambda (condition)
           (unless (typep condition 'restart-choice-available)
             (when (and restart-name (plusp (length restart-name)))
               (let ((restart (restart-find condition restart-name)))
                 (when restart
                   (if restart-value-function
                       (invoke-restart restart (funcall restart-value-function))
                       (invoke-restart restart)))))
             (let ((choices (restart-choices condition)))
               (when choices
                 (error 'restart-choice-available
                        :message (princ-to-string condition)
                        :condition condition
                        :choices choices)))))))
    (funcall thunk)))
