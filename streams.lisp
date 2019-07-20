(defpackage #:mini-streams-sys
  (:use #:cl)
  (:export
   #:invalid-cursor-error
   #:invalid-cursor
   #:target-stream
   #:reason)
  (:export
   #:mini-stream-ops
   #:backed-mini-stream
   #:bound-mini-stream)
  (:export
   #:mini-stream
   #:peek-atom
   #:skip-atom
   #:read-atom
   #:stream-position
   #:valid-cursor-p
   #:eofp)
  (:export
   #:make-string-mini-stream
   #:make-stream-mini-stream))
(in-package #:mini-streams-sys)

;;;; A stream abstraction to provide a consistent interface to various
;;;; datasources for parsing. Implemented as a struct of op functions
;;;; for performance reasons (generic-function accessors on a class
;;;; would be singificantly slower); new stream types can be created
;;;; by constructing a struct with the appropriate op functions.
;;;;
;;;; Note to implementers: it can be very convenient to use lambdas
;;;; that close over arguments to the stream constructor or local
;;;; variables, rather than defining a new stream type with data
;;;; slots. However, if you do this you'll cons new closure objects
;;;; every time a new stream is constructed -- in at least some
;;;; implementations (SBCL), if a lambda is not a closure, the
;;;; function object can be inlined and reused in subsequent calls.
;;;;
;;;; If performance is critical, or you expect to be creating many
;;;; mini-streams at once, it is recommended to define an ops struct
;;;; as a parameter, and re-use the same ops object whenever a new
;;;; stream is constructed. This will reduce the amount of memory
;;;; required to store ops, and generally force you to adhere to the
;;;; previous guideline.

;; TODO: benchmark that claim about speed. Probably true, but you
;; should check.

;; TODO: see if separating the op slots into a mini-stream-ops struct
;; would be useful. Might save on memory, since we can share the op
;; object among all stream instances.

(defstruct (mini-stream-ops (:conc-name #:stream-op-))
  peek-atom
  skip-atom
  valid-cursor-p
  position
  set-position
  at-end-p)

(defstruct mini-stream
  ;; Operation functions.
  ops
  ;; Data members.
  cursor)

;;; Interface to the mini-stream abstraction.

(defmacro self-op (name op &optional args)
  `(defun ,name (stream ,@args)
     (funcall (,op (mini-stream-ops stream)) stream ,@args)))

(self-op peek-atom stream-op-peek-atom)
(self-op skip-atom stream-op-skip-atom)

(defun read-atom (stream)
  (let ((element (peek-atom stream))
        (eofp (eofp stream)))
    (skip-atom stream)
    (values element eofp)))

(self-op stream-position stream-op-position)
(self-op valid-cursor-p stream-op-valid-cursor-p (cursor))
(self-op eofp stream-op-at-end-p)

(define-condition invalid-cursor-error (error)
  ((cursor :initarg :cursor :accessor invalid-cursor)
   (stream :initarg :stream :accessor target-stream)
   (reason :initarg :reason :accessor reason))
  (:default-initargs :reason nil)
  (:report (lambda (c s)
             (format s "Cursor ~S is not valid for stream ~S~:[.~;:~& ~:*~A~]"
                     (invalid-cursor c)
                     (target-stream c)
                     (reason c)))))

(defun (setf stream-position) (cursor stream)
  (multiple-value-bind (validp reason)
      (valid-cursor-p stream cursor)
    (unless validp
      (error 'invalid-cursor-error :cursor cursor :stream stream :reason reason)))
  (funcall (stream-op-set-position (mini-stream-ops stream)) stream cursor)
  cursor)


;;;; Testbed stream.

(defstruct (backed-mini-stream (:include mini-stream))
  (data nil :type t :read-only t))

(defstruct (bound-mini-stream (:include backed-mini-stream))
  (start nil :read-only t)
  (end nil :read-only t))

(defparameter +string-stream-ops+
  (make-mini-stream-ops
   :valid-cursor-p (lambda (stream cursor)
                     (let ((string (backed-mini-stream-data stream))
                           (end (bound-mini-stream-end stream)))
                       (cond
                         ((not (typep cursor '(integer 0)))
                          (values nil "Cursor must be a positive integer."))
                         ((> cursor (or end (length string)))
                          (values nil "Cursor position is beyond the end of the data stream."))
                         ((< cursor (bound-mini-stream-start stream))
                          (values nil "Cursor position is prior to stream start."))
                         (t
                          (values t nil)))))
   :position #'mini-stream-cursor
   :set-position (lambda (stream cursor)
                   (setf (mini-stream-cursor stream) cursor))
   :at-end-p (lambda (stream)
               (= (stream-position stream)
                  (or (bound-mini-stream-end stream)
                      (length (backed-mini-stream-data stream)))))
   :peek-atom (lambda (stream)
                (if (eofp stream)
                    nil
                    (elt (backed-mini-stream-data stream) (mini-stream-cursor stream))))
   :skip-atom (lambda (stream)
                (unless (eofp stream)
                  (incf (mini-stream-cursor stream))))))

(defun make-string-mini-stream (string &key (start 0) end)
  (check-type string string)
  (check-type start (integer 0))
  (check-type end (or null (integer 0)))
  (when (> start (length string))
    (error "Start position ~A is out of bounds: must be no more than ~A."
           start (length string)))
  (when (and end (> end (length string)))
    (error "End position ~A is out of bounds: must be no more than ~A."
           end (length string)))
  (when (and end (> start end))
    (error "Invalid bounds: start position ~A is greater than end position ~A."
           start end))
  (make-bound-mini-stream
   :ops +string-stream-ops+
   :cursor start
   :data string
   :start start
   :end end))

(defparameter +wrapped-stream-ops+
  (make-mini-stream-ops
   ;; We don't really have a way to check the validity of a cursor, so
   ;; don't. Seeking will turn up any additional errors.
   :valid-cursor-p (lambda (stream cursor)
                     (let ((start (bound-mini-stream-start stream))
                           (end (bound-mini-stream-end stream)))
                       (cond
                         ((< cursor start)
                          (values nil "Cursor position is prior to stream start."))
                         ((and end (> cursor end))
                          (values nil "Cursor is beyond the end of the data stream."))
                         (t (values t nil)))))
   :position (lambda (stream)
               (file-position (backed-mini-stream-data stream)))
   :set-position (lambda (stream cursor)
                   (file-position (backed-mini-stream-data stream) cursor))
   :at-end-p (lambda (stream)
               ;; This being a stream, we really can't know if end
               ;; is actually past the end of the stream, so check
               ;; for a character if the position check succeeds.
               (let ((stream (backed-mini-stream-data stream))
                     (end (bound-mini-stream-end stream)))
                 (if (and end (>= (file-position stream) end))
                     t
                     (null (peek-char nil stream nil nil)))))
   :peek-atom (lambda (stream)
                (if (eofp stream)
                    nil
                    (peek-char nil (backed-mini-stream-data stream) nil nil)))
   :skip-atom (lambda (stream)
                ;; Could technically do this with FILE-POSITION or
                ;; similar, but there's not much point.
                (unless (eofp stream)
                  (read-char (backed-mini-stream-data stream) nil nil)
                  (incf (mini-stream-cursor stream))))))

(defun make-stream-mini-stream (stream &key (start 0) end)
  "Return a mini-stream that wraps STREAM. STREAM must be a character
  input-stream seekable with FILE-POSITION. START and END are relative
  to the stream's position at the time of the call. Using :START may
  alter the position of the input stream even before the mini-stream
  has been advanced."
  (check-type stream stream)
  (check-type start (integer 0))
  (check-type end (or null (integer 0)))
  (when (and end (> start end))
    (error "Invalid bounds: start ~A is greater than end ~A."
           start end))
  (let* ((pos (file-position stream))
         (start-pos (+ pos start))
         (end-pos (and end (+ pos end))))
    ;; Seek to the start offset.
    (file-position stream start-pos)
    (make-bound-mini-stream
     :ops +wrapped-stream-ops+
     :cursor start-pos
     :data stream
     :start start
     :end end-pos)))

(in-package #:cl)
(defpackage #:mini-streams
  (:use #:cl)
  (:import-from #:mini-streams-sys
                #:invalid-cursor-error
                #:invalid-cursor
                #:target-stream
                #:reason)
  (:import-from #:mini-streams-sys
                #:mini-stream
                #:peek-atom
                #:skip-atom
                #:read-atom
                #:stream-position
                #:valid-cursor-p
                #:eofp)
  (:import-from #:mini-streams-sys
                #:make-string-mini-stream
                #:make-stream-mini-stream)
  (:export #:invalid-cursor-error
           #:invalid-cursor
           #:target-stream
           #:reason)
  (:export #:mini-stream
           #:peek-atom
           #:skip-atom
           #:read-atom
           #:stream-position
           #:valid-cursor-p
           #:eofp)
  (:export #:make-string-mini-stream
           #:make-stream-mini-stream))
