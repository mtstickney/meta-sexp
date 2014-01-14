;;; Copyright (c) 2007, Volkan YAZICI <yazicivo@ttnet.net.tr>
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;; - Redistributions of source code must retain the above copyright
;;;   notice, this list of conditions and the following disclaimer.
;;; - Redistributions in binary form must reproduce the above
;;;   copyright notice, this list of conditions and the following
;;;   disclaimer in the documentation and/or other materials provided
;;;   with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
;;; TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
;;; THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

(in-package :meta-sexp)


;;; Parser Context Structure & Routines

(defclass parser-context ()
  ()
  (:documentation "Base for all parser context classes."))

(defgeneric create-parser-context (input &rest args)
  (:documentation "Create a new parser context from INPUT and ARGS."))

(defgeneric peek-atom (context)
  (:documentation "Return the current item from CONTEXT without consuming it. Returns (VALUES atom eof-p), where atom is nil if eof-p is t, and eof-p is t if there is no current data to be read."))

(defgeneric read-atom (context)
  (:documentation "Consume and return the current item from CONTEXT. The default method simply calls PEEK-CHAR; specializing classes can provide an :after method to increment the cursor.")
  (:method ((context parser-context))
    (peek-atom context)))

(defgeneric checkpoint (context)
  ;; TODO: "state" or "position"? (there might be both positional and
  ;; icase-related state -- do we save and restore both, or just one?)
  (:documentation "Store the current state of CONTEXT for later restoration."))

(defgeneric checkpointed-p (context)
  (:documentation "Return t if CONTEXT has a saved checkpoint, nil otherwise."))

(defgeneric commit (context)
  (:documentation "Commit (remove) the last saved checkpoint."))

(defgeneric rollback (context)
  (:documentation "Restore the context state saved in the last checkpoint. An error is signaled if there is no saved checkpoint."))

(defgeneric begin-nocase (context)
  (:documentation "Begin a region where matches are done case-insensitively."))

(defgeneric begin-case (context)
  (:documentation "Begin a region where matches are done case-sensitively (the default). This is used to restore case-sensitivity within a case-insensitive region."))

(defgeneric end-case-region (context)
  (:documentation "End the most recent case-insensitive or case-sensitive region. If there is no such region, the default of case-sensitivity is restored."))

(defgeneric case-sensitive-p (context)
  (:documentation "Returns t if CONTEXT is matching case-sensitively, nil otherwise.")
  (:method ((context parser-context))
    (not (case-insensitive-p context))))

(defgeneric case-insensitive-p (context)
  (:documentation "Returns t if CONTEXT is matching case-insensitively, nil otherwise."))

(defgeneric cursor (context)
  (:documentation "Return an object indicating the current match location in the context."))

(defgeneric context-subseq (context start &optional end)
  (:documentation "Return a new context for the subsequence of this context bounded by the cursor objects START and (optionally) END. As with SUBSEQ, END is an exclusive bound, and if END is nil, the end of the sequence is used."))

(defgeneric context-data (context)
  (:documentation "Return the data covered by CONTEXT, in whatever format is appropriate for the context."))

(defgeneric next-cursor (context cursor)
  (:documentation "Return a cursor object for the position after CURSOR in CONTEXT."))

(defgeneric attachment (context)
  (:documentation "Return a user-defined data object attached to CONTEXT."))

(defgeneric (setf attachment) (val context)
  (:documentation "Set the user-defined data object attached to CONTEXT to VAL."))

(defclass string-parser-context (parser-context)
  ((data :initarg :data :reader source-data :type string)
   (cursor :initarg :start :accessor cursor :type '(or null (integer 0)))
   ;; Artificial buffer size, used for zero-copy subsequences
   (size :initarg :size :accessor data-size :type '(integer 0))
   (case-regions :initform nil :accessor case-regions :type list)
   (checkpoints :initform nil :accessor checkpoints :type list)
   (attachment :initarg :attachment :accessor attachment))
  (:documentation "A parser context used when parsing a string."))

(defgeneric create-parser-context (input &rest args))

(defmethod create-parser-context ((input string) &key start end attachment)
  (make-instance 'string-parser-context
                 :data input
                 :start (or start 0)
                 :size (or end (length input))
                 :attachment attachment))

(defmethod create-parser-context
    ((input stream) &key buffer-size start end attachment)
  (loop with out = (make-string-output-stream)
        with buffer-size = (or buffer-size 8192)
        with buf = (make-string buffer-size)
        for pos = (read-sequence buf input :end buffer-size)
        sum pos into size
        until (zerop pos)
        do (write-string buf out :end pos)
        finally (return
                  (create-parser-context
                   (get-output-stream-string out)
                   :start start
                   :end (or end size)
                   :attachment attachment))))

(defmethod peek-atom ((ctx string-parser-context))
  (with-accessors ((size data-size)
                   (cursor cursor)
                   (data source-data))
      ctx
    (if (< cursor size)
        (values (elt data cursor) nil)
        (values nil t))))

(defmethod read-atom :after ((ctx string-parser-context))
  (let ((cursor (cursor ctx)))
    (setf (cursor ctx) (next-cursor ctx cursor))))

;; We save the case-region and cursor state in checkpoints
;; Interesting note: the checkpoint list is also implicitly saved as
;; the cdr of the checkpoint list
(defmethod checkpoint ((ctx string-parser-context))
  (push (list (cursor ctx) (case-regions ctx))
        (checkpoints ctx))
  (values))

(defmethod checkpointed-p ((ctx string-parser-context))
  (not (null (checkpoints ctx))))

(defmethod rollback ((ctx string-parser-context))
  (if (checkpointed-p ctx)
      (destructuring-bind (cursor case-regions) (pop (checkpoints ctx))
        (setf (cursor ctx) cursor
              (case-regions ctx) case-regions))
      (error "There is no checkpoint to rollback for context ~S." ctx))
  (values))

(defmethod commit ((ctx string-parser-context))
  (if (checkpointed-p ctx)
      (pop (checkpoints ctx))
      (error "There is no checkpoint to commit for context ~S." ctx))
  (values))

(defmethod begin-nocase ((ctx string-parser-context))
  (push t (case-regions ctx))
  (values))

(defmethod begin-case ((ctx string-parser-context))
  (push nil (case-regions ctx))
  (values))

(defmethod end-case-region ((ctx string-parser-context))
  (flet ((checkpointed-case-region (ctx)
           (second (first (checkpoints ctx)))))
    (cond
      ((null (case-regions ctx))
       (error "There is no case-region is in effect for context ~S." ctx))
      ((eq (case-regions ctx) (checkpointed-case-region ctx))
       (error "Cannot end a case-region that began outside the last checkpoint boundary."))
      (t (pop (case-regions ctx))))
    (values)))

(defmethod case-insensitive-p ((ctx string-parser-context))
  (first (case-regions ctx)))

(defmethod context-subseq ((ctx string-parser-context) start &optional end)
  (check-type start (integer 0))
  (check-type end (or null (integer 0)))
  (make-instance 'string-parser-context
                 :data (source-data ctx)
                 :start start
                 :size (or end (data-size ctx))
                 ;; TODO: not sure this should be copied.
                 :attachment (attachment ctx)))

(defmethod context-data ((ctx string-parser-context))
  (subseq (source-data ctx) (cursor ctx) (data-size ctx)))

;; TODO: cursor gets initialized to nil for 0-length data, data-size
;; gets inialized to (min data-size (length data)) (or at least have
;; some asserts)
(defmethod next-cursor ((ctx string-parser-context) cursor)
  (check-type cursor (integer 0))
  (if (< cursor (data-size ctx))
      (1+ cursor)
      ;; EOF is data-size
      (data-size ctx)))

;;; Atom, Rule & Type Matching

(defun match-char (ctx char &aux)
  (multiple-value-bind (atom eof-p) (peek-atom ctx)
    (if (and (typep atom 'character)
             (not eof-p)
             (if (case-insensitive-p ctx)
                 (char-equal atom char)
                 (char= atom char)))
        (read-atom ctx)
        nil)))

(defmacro match-type (ctx type)
  `(multiple-value-bind (atom eof-p) (peek-atom ,ctx)
     (if (and (not eof-p)
              (typep atom ',type))
         (read-atom ,ctx)
         nil)))

(defmacro match-rule (ctx rule args)
  `(,rule ,@(nconc (list ctx) args)))


;;; Grammar Compiler

(defgeneric transform-grammar (ret ctx in-meta directive &optional args)
  (:documentation "META grammar transformation methods."))

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive character) &optional args)
  "Transforms a character form."
  (declare (ignore ret args))
  `(match-char ,ctx ,directive))

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive string) &optional args)
  "Transforms a string form."
  (declare (ignore args))
  (transform-grammar
   ret ctx t :checkpoint
   `((and
      ,@(map
         'list
         (lambda (form) `(match-char ,ctx ,form))
         directive)
      ,directive))))

(defmethod transform-grammar (ret ctx in-meta directive &optional args)
  "The most unspecific transformation method."
  (declare (ignore args))
  (cond
    ((and in-meta (consp directive) (keywordp (car directive)))
     (transform-grammar ret ctx t (car directive) (cdr directive)))
    ((and (not in-meta) (consp directive) (eql 'meta (car directive)))
     (transform-grammar ret ctx t :and (cdr directive)))
    ((consp directive)
     (mapcar (lambda (form) (transform-grammar ret ctx nil form)) directive))
    (t directive)))

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :icase)) &optional args)
  "\(:ICASE FORM FORM ...)

Make case-insensitive atom comparison in supplied FORMs."
  (with-unique-names (wrapper-ret val)
    `(progn
       (begin-nocase ,ctx)
       (let ((,wrapper-ret
              (lambda (,val)
                (end-case-region ,ctx)
                (funcall ,ret ,val))))
         (declare (ignorable ,wrapper-ret))
         ;; You know, we could just call the return wrapper here
         ;; instead of duplicating return code.... (involves a
         ;; funcall, but is potentially much shorter)
         (prog1
             ,(transform-grammar wrapper-ret ctx t :and args)
           (end-case-region ,ctx))))))

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :checkpoint)) &optional args)
  "\(:CHECKPOINT FORM FORM ...)

Sequentially evaluates supplied forms and if any of them fails, moves cursor
back to its start position :CHECKPOINT began."
  (with-unique-names (wrapper-ret val)
    `(progn
       (checkpoint ,ctx)
       (let ((,wrapper-ret
              (lambda (,val)
                (if ,val
                    (commit ,ctx)
                    (rollback ,ctx))
                (funcall ,ret ,val))))
         (declare (ignorable ,wrapper-ret))
         (let ((,val ,(transform-grammar wrapper-ret ctx t :and args)))
           (if ,val
               (commit ,ctx)
               (rollback ,ctx))
           ,val)))))

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :and)) &optional args)
  "\(:AND FORM FORM ...)

Sequentially evaluates FORMs identical to AND."
  `(and ,@(mapcar (lambda (form) (transform-grammar ret ctx t form)) args)))

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :or)) &optional args)
  "\(:OR FORM FORM ...)

Sequentially evalutes FORMs identical to OR."
  `(or ,@(mapcar (lambda (form) (transform-grammar ret ctx t form)) args)))

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :not)) &optional args)
  "\(:NOT FORM)

Identical to \(NOT FORM). \(FORM is encapsulated within a :CHECKPOINT before
getting evaluated.)"
  (transform-grammar
   ret ctx t :checkpoint
   `((not ,(transform-grammar ret ctx t (car args))))))

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :return)) &optional args)
  "\(:RETURN VALUE VALUE ...)

Returns from the rule with supplied VALUEs."
  (declare (ignore ctx))
  `(funcall ,ret (list ,@args)))

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :render)) &optional args)
  "\(:RENDER RENDERER ARG ARG ...)

Calls specified renderer \(which is defined with DEFRENDERER) with the supplied
arguments."
  (declare (ignore ret))
  `(,(car args) ,@(nconc (list ctx) (cdr args))))

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :?)) &optional args)
  "\(:? FORM FORM ...)

Sequentially evaluates supplied FORMs within an AND scope and regardless of the
return value of ANDed FORMs, block returns T. \(Similar to `?' in regular
expressions.)"
  `(prog1 t ,(transform-grammar ret ctx t :and args)))

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :*)) &optional args)
  "\(:* FORM FORM ...)

Sequentially evaluates supplied FORMs within an AND scope until it returns
NIL. Regardless of the return value of ANDed FORMs, block returns T. \(Similar
to `*' in regular expressions.)"
  `(not (do () ((not ,(transform-grammar ret ctx t :and args))))))

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :+)) &optional args)
  "\(:+ FORM FORM ...)

Sequentially evaluates supplied FORMs within an AND scope, and repeats this
process till FORMs return NIL. Scope returns T if FORMs returned T once or more,
otherwise returns NIL. \(Similar to `{1,}' in regular expressions.)"
  (transform-grammar ret ctx t :and `(,@args (:* ,@args))))

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :type)) &optional args)
  "\(:TYPE TYPE-CHECKER)
\(:TYPE \(OR TYPE-CHECKER TYPE-CHECKER ...))

Checks type of the atom at the current position through supplied function(s)."
  (declare (ignore ret))
  `(match-type ,ctx ,(car args)))

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :rule)) &optional args)
  "\(:RULE RULE ARG ARG ...)
\(:RULE (OR RULE RULE ...) ARG ARG ...)

Tests input in the current cursor position using specified type/form. If any,
supplied arguments will get passed to rule."
  (if (and (consp (car args)) (eql 'or (caar args)))
      (transform-grammar
       ret ctx t :or (mapcar (lambda (form) `(:rule ,form ,@(cdr args)))
                             (cdar args)))
      `(match-rule ,ctx ,(car args) ,(cdr args))))

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :assign)) &optional args)
  "\(:ASSIGN VAR FORM)
\(:ASSIGN \(VAR1 VAR2 ...) FORM)

Assigns returned value of FORM to VAR, and returns assigned value. \(Latter form
works similar to MULTIPLE-VALUE-SETQ.)"
  (if (consp (car args))
      `(multiple-value-setq ,(car args)
         ,(transform-grammar ret ctx t (cadr args)))
      `(setq ,(car args) ,(transform-grammar ret ctx t (cadr args)))))

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :list-push)) &optional args)
  "\(:LIST-PUSH ITEM-VAR LIST-ACCUM)

Pushes ITEM-VAR into the specified LIST-ACCUM. (See MAKE-LIST-ACCUM and
EMPTY-LIST-ACCUM-P.)"
  (declare (ignore ret ctx))
  `(list-accum-push ,(car args) ,(cadr args)))

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :list-reset)) &optional args)
  "\(:LIST-RESET LIST-ACCUM)

Resets supplied LIST-ACCUM."
  (declare (ignore ret ctx))
  `(reset-list-accum ,(car args)))

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :char-push)) &optional args)
  "\(:CHAR-PUSH CHAR-VAR CHAR-ACCUM)
\(:CHAR-PUSH CHAR-ACCUM)

Pushes supplied CHAR-VAR into specified CHAR-ACCUM. If called with
a single argument, current character gets read and pushed into supplied
accumulator. (See MAKE-CHAR-ACCUM and EMPTY-CHAR-ACCUM-P.)"
  (declare (ignore ret))
  (if (cdr args)
      `(char-accum-push ,(car args) ,(cadr args))
      `(char-accum-push (read-atom ,ctx) ,(car args))))

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :char-reset)) &optional args)
  "\(:CHAR-RESET CHAR-ACCUM)

Resets supplied CHAR-ACCUM."
  (declare (ignore ret ctx))
  `(reset-char-accum ,(car args)))

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :eof)) &optional args)
  "\(:EOF)

Returns T when reached to the end of supplied input data."
  (declare (ignore ret args))
  `(nth-value 1 (peek-atom ,ctx)))

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :read-atom)) &optional args)
  "\(:READ-ATOM)

Reads current atom at the cursor position and returns read atom."
  (declare (ignore ret args))
  `(read-atom ,ctx))

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :debug)) &optional args)
  "\(:DEBUG)
\(:DEBUG VAR)

Print current character and its position in the input data. If VAR is specified,
print the value of the VAR."
  (declare (ignore ret))
  `(prog1 t
     ,(if (car args)
          `(format t "DEBUG: ~s: ~a~%" ',(car args) ,(car args))
          `(format t "DEBUG: cursor: [~s] `~s'~%"
                   (cursor ,ctx)
                   (multiple-value-list (peek-atom ,ctx))))))


;;; Atom, Rule & Renderer Definition Macros

(defmacro defatom (name (c) &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name (,c) (when ,c ,@body))
     (deftype ,name () `(satisfies ,',name))))

(defmacro defrule (name (&rest args) (&optional attachment) &body body)
  (with-unique-names (ctx ret val)
    `(defun ,name (,ctx ,@args)
       (let ((,ret
              (lambda (,val)
                (return-from ,name (apply #'values ,val)))))
         ,(if attachment
              `(let ((,attachment (attachment ,ctx)))
                 ,(transform-grammar ret ctx t :checkpoint body))
              (transform-grammar ret ctx t :checkpoint body))))))

(defmacro defrenderer (name (&rest args) (&optional attachment) &body body)
  (with-unique-names (ctx)
    `(defun ,name (,ctx ,@args)
       ,(if attachment
            `(let ((,attachment (attachment ,ctx)))
               ,@body)
            `(progn ,@body))
       t)))
