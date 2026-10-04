(in-package #:surgeon)

;;;; -- Top-Level Source Forms --

(defclass source-form ()
  ((form
    :initarg :form
    :reader source-form-form
    :documentation "The parsed top-level form.")
   (start
    :initarg :start
    :reader source-form-start
    :type (integer 0)
    :documentation "The character offset at which the form begins.")
   (end
    :initarg :end
    :reader source-form-end
    :type (integer 0)
    :documentation "The character offset immediately after the form."))
  (:documentation "One parsed top-level form and its exact source span."))

(-> source-read-forms (string &key (:package package)) list)
(defun source-read-forms (source &key (package *package*))
  "Return every top-level form of SOURCE as a SOURCE-FORM, read in PACKAGE.

Whitespace, line comments and nested block comments between forms are
skipped, so each span covers exactly one form. Read-time evaluation is off."
  (let ((stream (make-string-input-stream source))
        (position 0)
        (forms nil)
        (*read-eval* nil)
        (*package* package))
    (loop
      (setf position (source--next-form-start source position))
      (when (>= position (length source))
        (return (nreverse forms)))
      (file-position stream position)
      (let ((form (read-preserving-whitespace stream t nil))
            (end (file-position stream)))
        (push (make-instance 'source-form :form form :start position :end end) forms)
        (setf position end)))))

(-> source-definition-match-p (source-form list) boolean)
(defun source-definition-match-p (source-form definition)
  "Return true when SOURCE-FORM is a definition with DEFINITION's signature."
  (let ((form (source-form-form source-form)))
    (and (definition-form-p form)
         (equal (definition-signature form) (definition-signature definition)))))

(-> source-find-definition (pathname list &key (:package package)) (values source-form string))
(defun source-find-definition (pathname definition &key (package *package*))
  "Return the top-level form of PATHNAME matching DEFINITION, and the file's text.

The file is read in PACKAGE. A file without a match signals DEFINITION-NOT-FOUND."
  (let* ((source (uiop:read-file-string pathname))
         (match (find-if (lambda (source-form)
                           (source-definition-match-p source-form definition))
                         (source-read-forms source :package package))))
    (unless match
      (error 'definition-not-found
             :pathname pathname
             :definition definition
             :message (format nil "No matching definition exists in ~A." pathname)))
    (values match source)))

(-> source-replace-definition (pathname string &key (:package package)) (values string string))
(defun source-replace-definition (pathname definition-source &key (package *package*))
  "Replace the definition in PATHNAME matching DEFINITION-SOURCE with that source text.

DEFINITION-SOURCE must hold exactly one supported definition; anything else
signals DEFINITION-UNSUPPORTED. Everything outside the replaced span, comments
included, is preserved byte for byte, and the file is replaced atomically.
Return the updated file text and the source text of the replaced definition."
  (let ((definition (read-one-form definition-source :read-eval nil :package package)))
    (unless (definition-form-p definition)
      (error 'definition-unsupported
             :form definition
             :message "The source is not a supported complete definition."))
    (multiple-value-bind (match source) (source-find-definition pathname definition
                                                                :package package)
      (let ((previous (subseq source (source-form-start match) (source-form-end match)))
            (updated (concatenate 'string
                                  (subseq source 0 (source-form-start match))
                                  definition-source
                                  (subseq source (source-form-end match)))))
        (ls-compat.files:publish-file pathname updated)
        (values updated previous)))))

(-> source--skip-block-comment (string (integer 0)) (integer 0))
(defun source--skip-block-comment (source start)
  "Return the first offset after the nested block comment opening at START."
  (let ((position (+ start 2))
        (depth 1)
        (length (length source)))
    (loop while (and (< position length) (plusp depth))
          do (cond
               ((and (< (1+ position) length)
                     (char= (char source position) #\#)
                     (char= (char source (1+ position)) #\|))
                (incf depth)
                (incf position 2))
               ((and (< (1+ position) length)
                     (char= (char source position) #\|)
                     (char= (char source (1+ position)) #\#))
                (decf depth)
                (incf position 2))
               (t
                (incf position))))
    (when (plusp depth)
      (error 'surgeon-error :message "Unterminated block comment in source."))
    position))

(-> source--next-form-start (string (integer 0)) (integer 0))
(defun source--next-form-start (source start)
  "Return the offset of the next form in SOURCE at or after START, skipping comments."
  (let ((position start)
        (length (length source)))
    (loop
      (loop while (and (< position length)
                       (find (char source position)
                             '(#\Space #\Tab #\Newline #\Return #\Page)))
            do (incf position))
      (cond
        ((and (< position length) (char= (char source position) #\;))
         (let ((newline (position #\Newline source :start position)))
           (setf position (if newline (1+ newline) length))))
        ((and (< (1+ position) length)
              (char= (char source position) #\#)
              (char= (char source (1+ position)) #\|))
         (setf position (source--skip-block-comment source position)))
        (t
         (return position))))))
