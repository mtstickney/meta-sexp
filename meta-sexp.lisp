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

;; TODO: just using a checkpoint with begin-nocase would be a lot less
;; code, but some contexts might not checkpoint case-regions....
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
      `(,(car args) ,ctx ,@(cdr args))))

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
