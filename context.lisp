(in-package :meta-sexp)

;; Internal API for parser contexts
;; (create-parser-context data-source &rest args) -- constructor
;; (peek-atom context) -- Return the current item without consuming it
;; (read-atom ctx) -- Consume and return the current item
;; (checkpoint ctx) -- Save the current parser position
;; (checkpointed-p ctx) -- t if there i
;; (commit ctx) --
;; (rollback ctx)
;; (begin-nocase ctx)
;; (begin-case ctx)
;; (end-case-region ctx)
;; (cursor ctx) -- Return some sort of cursor indicating the current
;; position
;; (context-subseq ctx start &optional end) -- Return a fresh
;; parser context for the subsequence of CTX bounded by the cursors
;; START and END.
;; (context-data ctx) -- Return the data held in the context, in
;; whatever form is appropriate.
;; (next-cursor context cursor) -- incf for cursors (needed for
;; context-subseq bounds
;; (attachment context) -- custom data attachment for contexts

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
       (error "There is no case-region in effect for context ~S." ctx))
      ((eq (case-regions ctx) (checkpointed-case-region ctx))
       (error "Cannot end a case-region that began outside the last checkpoint boundary."))
      (t (pop (case-regions ctx))))
    (values)))

(defmethod case-insensitive-p ((ctx string-parser-context))
  (first (case-regions ctx)))

(defmethod context-subseq ((ctx string-parser-context) start &optional end)
  (check-type start (null (integer 0)))
  (check-type end (or null (integer 0)))
  (make-instance 'string-parser-context
                 :data (source-data ctx)
                 :start (or start 0)
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

;; Parser context for lists of objects
(defclass list-parser-context (parser-context)
  ((data :initarg :data :reader source-data :type list)
   (cursor :initarg :start :accessor cursor :type cons)
   (end :initarg :end :accessor end-cursor :type cons)
   (case-regions :initform nil :accessor case-regions :type list)
   (checkpoints :initform nil :accessor checkpoints :type list)
   (attachment :initarg :attachment :accessor attachment))
  (:documentation "A parser context used when parsing a list of objects."))

(defmethod create-parser-context ((input list) &key start end attachment)
  (make-instance 'list-parser-context
                 :data input
                 :start (or start input)
                 :end end
                 :attachment attachment))

(defmethod peek-atom ((ctx list-parser-context))
  (let ((cursor (cursor ctx)))
    (if (null cursor)
        (values nil t)
        (values (car cursor) nil))))

(defmethod read-atom :after ((ctx list-parser-context))
  (setf (cursor ctx) (cdr (cursor ctx))))

(defmethod checkpoint ((ctx list-parser-context))
  (push (list (cursor ctx) (case-regions ctx))
        (checkpoints ctx))
  (values))

(defmethod checkpointed-p ((ctx list-parser-context))
  (not (null (checkpoints ctx))))

(defmethod rollback ((ctx list-parser-context))
  (if (checkpointed-p ctx)
      (destructuring-bind (cursor case-regions) (pop (checkpoints ctx))
        (setf (cursor ctx) cursor
              (case-regions ctx) case-regions))
      (error "There is no checkpoint to rollback for context ~S." ctx))
  (values))

(defmethod commit ((ctx list-parser-context))
  (if (checkpointed-p ctx)
      (pop (checkpoints ctx))
      (error "There is no checkpoint to commit for context ~S." ctx))
  (values))

(defmethod begin-nocase ((ctx list-parser-context))
  (push t (case-regions ctx))
  (values))

(defmethod begin-case ((ctx list-parser-context))
  (push nil (case-regions ctx))
  (values))

(defmethod end-case-region ((ctx list-parser-context))
  (flet ((checkpointed-case-region (ctx)
           (second (first (checkpoints ctx)))))
    (cond
      ((null (case-regions ctx))
       (error "There is no case-region in effect for context ~S." ctx))
      ((eq (case-regions ctx) (checkpointed-case-region ctx))
       (error "Cannot end a case-region that began outside the last checkpoint boundary."))
      (t (pop (case-regions ctx))))
    (values)))

(defmethod case-insensitive-p ((ctx list-parser-context))
  (first (case-regions ctx)))

(deftype list-context-cursor () '(or cons null))

(defmethod context-subseq ((ctx list-parser-context) start &optional end)
  (check-type start list-context-cursor)
  (check-type end list-context-cursor)

  (assert (tailp start (source-data ctx))
          (start)
          "Start cursor ~S does not belong to the context ~S."
          start
          ctx)
  (assert (tailp end (source-data ctx))
          (end)
          "End cursor ~S does not belong to the context ~S."
          end
          ctx)

  (make-instance 'list-parser-context
                 :data (source-data ctx)
                 :start start
                 :end end
                 :attachment (attachment ctx)))

(defun list-subseq (start &optional end)
  (check-type start list-context-cursor)
  (check-type end list-context-cursor)

  (cond
    ((endp end) start)
    ((tailp end start) (ldiff start end))
    (t nil)))

(defmethod context-data ((ctx list-parser-context))
  (if (and (eq (cursor ctx) (source-data ctx))
           (endp (end-cursor ctx)))
      ;; TODO: we're returning the original list, maybe we shouldn't
      (source-data ctx)
      (list-subseq (cursor ctx) (end-cursor ctx))))

(defmethod next-cursor ((ctx list-parser-context) cursor)
  (assert (tailp cursor (source-data ctx))
          (cursor)
          "Cursor ~S does not belong to the context ~S."
          cursor
          ctx)
  (cdr cursor))
