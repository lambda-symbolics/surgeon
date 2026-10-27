(in-package #:surgeon)

;;;; -- Packages and Forms --

(-> package-find (t) (or null package))
(defun package-find (name)
  "Return the package designated by NAME, trying an upcased string name too.

A lowercase name such as \"cl-user\" therefore finds its package."
  (or (find-package name)
      (and (stringp name) (find-package (string-upcase name)))))

(-> call-with-package-unlocked (package function) t)
(defun call-with-package-unlocked (package thunk)
  "Call THUNK with PACKAGE temporarily unlocked, restoring its lock afterward."
  (let ((locked-p (sb-ext:package-locked-p package)))
    (unwind-protect
         (progn
           (when locked-p
             (sb-ext:unlock-package package))
           (funcall thunk))
      (when (and locked-p (find-package (package-name package)))
        (sb-ext:lock-package package)))))

(-> read-one-form (string &key (:read-eval boolean) (:package package)) t)
(defun read-one-form (source &key (read-eval t) (package *package*))
  "Read exactly one Common Lisp form from SOURCE relative to PACKAGE.

PACKAGE stays unlocked while reading so that the reader may intern new symbols
in it. A source without a form, or ending inside one, signals END-OF-FILE as
READ-FROM-STRING does, so callers can tell incomplete input apart; trailing
forms signal FORM-COUNT-ERROR, and reader errors pass through."
  (call-with-package-unlocked
   package
   (lambda ()
     (let ((*read-eval* read-eval)
           (*package*   package)
           (end-marker  (cons nil nil)))
       (multiple-value-bind (form position)
           (read-from-string source t nil :preserve-whitespace t)
         (unless (eq (read-from-string source nil end-marker :start position) end-marker)
           (error 'form-count-error
                  :source  source
                  :message (format nil "Expected exactly one Common Lisp form, but another ~
                                        follows the first, which ends at character ~D: ~S. ~
                                        Check the parentheses, or wrap several forms in PROGN."
                                   position (form--trailing-excerpt source position))))
         form)))))

(-> form--trailing-excerpt (string integer) string)
(defun form--trailing-excerpt (source position)
  "Return the start of the text after POSITION in SOURCE, at most 80 characters."
  (let ((rest (string-left-trim '(#\Space #\Tab #\Newline #\Return)
                                (subseq source position))))
    (if (> (length rest) 80)
        (concatenate 'string (subseq rest 0 80) "...")
        rest)))

(-> resolve-symbol (string &key (:package package)) symbol)
(defun resolve-symbol (name &key (package *package*))
  "Read symbol NAME relative to PACKAGE without read-time evaluation.

A quoted or function-quoted symbol, as in 'name or #'name, resolves to the
symbol itself. Anything else signals SURGEON-ERROR."
  (let* ((form  (read-one-form name :read-eval nil :package package))
         (value (if (and (consp form)
                         (member (first form) '(quote function))
                         (consp (rest form))
                         (null (rest (rest form)))
                         (symbolp (second form)))
                    (second form)
                    form)))
    (unless (symbolp value)
      (error 'surgeon-error :message (format nil "~S does not name a symbol." name)))
    value))


;;;; -- Definition Kinds --

(defparameter *definition-kinds*
  '(:function :macro :generic-function :class :condition :variable :type)
  "The definition kinds SYMBOL-DEFINITION-KINDS reports.")

(-> symbol-definition-kinds (symbol) list)
(defun symbol-definition-kinds (symbol)
  "Return the definition kinds SYMBOL carries in the running image, in *DEFINITION-KINDS* order.

Keywords and the constants T and NIL never count as variables or types, so a
plain interned name without a definition yields NIL. Types are looked up in
SBCL's type database rather than probed, so no undefined-type note is recorded."
  (let ((kinds '()))
    (cond
      ((macro-function symbol)
       (push ':macro kinds))
      ((special-operator-p symbol)
       (push ':function kinds))
      ((and (fboundp symbol)
            (typep (fdefinition symbol) 'generic-function))
       (push ':generic-function kinds))
      ((fboundp symbol)
       (push ':function kinds)))
    (when (and (boundp symbol)
               (not (keywordp symbol))
               (not (member symbol '(t nil))))
      (push ':variable kinds))
    (let ((class (find-class symbol nil)))
      (cond
        (class
         (push (if (subtypep class 'condition) ':condition ':class) kinds))
        ((and (not (member symbol '(t nil)))
              (not (keywordp symbol))
              (member (sb-int:info :type :kind symbol) '(:defined :primitive :instance)))
         (push ':type kinds))))
    (nreverse kinds)))

(-> symbol-defined-p (symbol) boolean)
(defun symbol-defined-p (symbol)
  "Return true when SYMBOL carries at least one definition kind."
  (not (null (symbol-definition-kinds symbol))))

(-> symbol-lambda-list (symbol) t)
(defun symbol-lambda-list (symbol)
  "Return SYMBOL's function lambda list when introspection can recover it, else NIL.

Generic functions answer through the MOP. Compiled functions keep no lambda
expression, so SBCL's recorded lambda list is consulted before the expression."
  (when (and (fboundp symbol)
             (not (special-operator-p symbol)))
    (handler-case
        (let ((function (symbol-function symbol)))
          (if (typep function 'generic-function)
              (closer-mop:generic-function-lambda-list function)
              (let ((recorded (sb-introspect:function-lambda-list function)))
                (if (listp recorded)
                    recorded
                    (let ((expression (function-lambda-expression function)))
                      (and (consp expression)
                           (eq (first expression) 'lambda)
                           (second expression)))))))
      (error ()
        nil))))

(-> symbol-definition-pathname (symbol &optional list) (or null pathname))
(defun symbol-definition-pathname (symbol &optional (kinds (symbol-definition-kinds symbol)))
  "Return the first source file SBCL recorded for SYMBOL under one of KINDS."
  (dolist (kind kinds nil)
    (dolist (source (sb-introspect:find-definition-sources-by-name symbol kind))
      (let ((pathname (sb-introspect:definition-source-pathname source)))
        (when pathname
          (return-from symbol-definition-pathname (pathname pathname)))))))

(-> symbol-documentation-line (symbol list &key (:limit (integer 1))) (or null string))
(defun symbol-documentation-line (symbol kinds &key (limit 110))
  "Return the first documentation line of SYMBOL's primary definition among KINDS.

The line is trimmed and cut to LIMIT characters with a trailing ellipsis."
  (let ((documentation
          (or (and (intersection kinds '(:function :macro :generic-function))
                   (documentation symbol 'function))
              (and (member ':variable kinds)
                   (documentation symbol 'variable))
              (and (intersection kinds '(:class :condition :type))
                   (documentation symbol 'type)))))
    (when (and (stringp documentation) (plusp (length documentation)))
      (let ((line (string-trim '(#\Space #\Tab)
                               (subseq documentation
                                       0 (or (position #\Newline documentation)
                                             (length documentation))))))
        (if (> (length line) limit)
            (concatenate 'string (subseq line 0 limit) "...")
            line)))))

(-> symbol-label (symbol package) string)
(defun symbol-label (symbol package)
  "Return SYMBOL's lowercase name, qualified when its home is not PACKAGE."
  (let ((home (symbol-package symbol)))
    (if (or (null home) (eq home package))
        (string-downcase (symbol-name symbol))
        (format nil "~(~A:~A~)" (package-name home) (symbol-name symbol)))))


;;;; -- Apropos and Near Misses --

(-> package-present-symbols (package) list)
(defun package-present-symbols (package)
  "Return the symbols present in PACKAGE, imported ones included, inherited ones excluded."
  (let ((symbols '()))
    (do-symbols (symbol package)
      (multiple-value-bind (found status) (find-symbol (symbol-name symbol) package)
        (when (and (eq found symbol)
                   (not (eq status ':inherited)))
          (push symbol symbols))))
    (remove-duplicates symbols :test #'eq)))

(-> package-apropos (string &key (:package package) (:kind (or null keyword))) list)
(defun package-apropos (query &key (package *package*) kind)
  "Return (SYMBOL . KINDS) pairs for the defined symbols present in PACKAGE matching QUERY.

Every whitespace-separated term of QUERY must occur in the symbol name without
regard to case. KIND keeps only symbols carrying that definition kind. Symbols
without any definition are never returned, so names interned by misspelled
lookups stay invisible. Matches are sorted by name."
  (let ((terms   (apropos--terms query))
        (matches '()))
    (dolist (symbol (package-present-symbols package))
      (let ((name (string-downcase (symbol-name symbol))))
        (when (every (lambda (term) (search term name)) terms)
          (let ((kinds (symbol-definition-kinds symbol)))
            (when (and kinds (or (null kind) (member kind kinds)))
              (push (cons symbol kinds) matches))))))
    (sort matches #'string< :key (lambda (match) (symbol-name (first match))))))

(-> package-symbol-suggestions
    (string &key (:package package) (:limit (integer 1)) (:token-minimum (integer 1)))
    list)
(defun package-symbol-suggestions (name &key (package *package*) (limit 3) (token-minimum 3))
  "Return up to LIMIT defined symbols of PACKAGE whose names resemble the guessed NAME.

Candidates share at least one separator-delimited token of TOKEN-MINIMUM or
more characters with NAME, or contain it, or are contained by it when the
shorter of the two has TOKEN-MINIMUM characters; stronger overlaps rank first
and shorter names break ties."
  (let* ((guess  (string-downcase (string-trim "'#:" name)))
         (tokens (apropos--name-tokens guess token-minimum))
         (scored '()))
    (dolist (symbol (package-present-symbols package))
      (let* ((candidate (string-downcase (symbol-name symbol)))
             (score     (apropos--suggestion-score candidate tokens guess token-minimum)))
        (when (and (plusp score)
                   (string/= candidate guess)
                   (symbol-defined-p symbol))
          (push (cons score symbol) scored))))
    (setf scored (sort scored
                       (lambda (left right)
                         (or (> (first left) (first right))
                             (and (= (first left) (first right))
                                  (< (length (symbol-name (rest left)))
                                     (length (symbol-name (rest right)))))))))
    (loop for (nil . symbol) in scored
          repeat limit
          collect symbol)))

(-> apropos--terms (string) list)
(defun apropos--terms (query)
  "Return the lowercase whitespace-separated search terms of QUERY."
  (let ((terms '())
        (start nil))
    (loop for index from 0 to (length query)
          for character = (and (< index (length query)) (char query index))
          do (cond
               ((and character
                     (not (member character '(#\Space #\Tab #\Newline #\Return))))
                (unless start
                  (setf start index)))
               (start
                (push (string-downcase (subseq query start index)) terms)
                (setf start nil))))
    (nreverse terms)))

(-> apropos--name-tokens (string (integer 1)) list)
(defun apropos--name-tokens (name minimum)
  "Return the distinct separator-delimited tokens of NAME with at least MINIMUM characters."
  (let ((tokens '())
        (start  0))
    (loop for index from 0 to (length name)
          when (or (= index (length name))
                   (member (char name index) '(#\- #\: #\. #\/ #\Space)))
            do (when (>= (- index start) minimum)
                 (push (subseq name start index) tokens))
               (setf start (1+ index)))
    (remove-duplicates (nreverse tokens) :test #'string=)))

(-> apropos--suggestion-score (string list string (integer 1)) integer)
(defun apropos--suggestion-score (candidate tokens guess minimum)
  "Return how strongly lowercase CANDIDATE resembles GUESS through its TOKENS.

Containment of one name in the other adds a point only when the shorter name
has at least MINIMUM characters, so one-letter names never qualify."
  (+ (count-if (lambda (token) (search token candidate)) tokens)
     (if (and (>= (min (length candidate) (length guess)) minimum)
              (or (search guess candidate) (search candidate guess)))
         1
         0)))
