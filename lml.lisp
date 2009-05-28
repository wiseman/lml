
;; Lisp Markup Language
;;
;; John Wiseman
;; May 2000

(defun read-lml (stream &optional (eof-error-p T) eof-value)
  (let ((token (read-lml-token stream NIL :eof)))
    (cond ((eq token :open-tag)
	   (read-lml-tag stream eof-error-p eof-value))
	  ((stringp token)
	   token)
	  (T
	   (error "oh no")))))

(defun read-lml-file (path)
  (with-open-file (in path :direction :input)
    (read-lml in)))

(defun read-lml-token-or-whitespace (stream &optional (eof-error-p T) eof-value)
  (let ((white (eat-whitespace stream)))
    (if white
	(values white NIL)
      (let ((char (read-char stream NIL :eof)))
	(let ((token (cond ((eql char #\()
			    :open-tag)
			   ((eql char #\))
			    :close-tag)
			   ((eql char #\[)
			    :open-attribs)
			   ((eql char #\])
			    :close-attribs)
			   ((eql char #\;)
			    (read-comment stream)
			    (read-lml-token-or-whitespace stream eof-error-p eof-value))
			   ((eq char :eof)
			    (read-char stream eof-error-p eof-value))
			   (T
			    (unread-char char stream)
			    (read-text-token stream eof-error-p eof-value)))))
	  (values token T))))))

(defun read-lml-token (stream &optional (eof-error-p T) eof-value)
  (multiple-value-bind (token non-whitespace-p)
      (read-lml-token-or-whitespace stream eof-error-p eof-value)
    (if non-whitespace-p
	token
      (read-lml-token stream eof-error-p eof-value))))

(defun read-attribs (stream eof-error-p error-value)
  (read-delimited-list #\] stream T))

(defun read-comment (stream)
  (let ((char (read-char stream)))
    (unless (eql char #\newline)
      (read-comment stream))))

(defun token-char-p (char)
  (and (not (whitespace-char-p char))
   (not (special-char-p char))))

(defun tag-name-char-p (char)
  (and (not (whitespace-char-p char))
       (not (special-char-p char))))

(defun special-char-p (char)
  (or (eql char #\()
      (eql char #\))
      (eql char #\[)
      (eql char #\])))

(defun read-text-token (stream &optional (eof-error-p T) eof-value)
  (let ((eof '#:eof)
	(building-token-p NIL))
    (let ((token
	   (with-output-to-string (token-stream)
	     (labels ((add-to-token (char)
			(princ char token-stream)
			(setf building-token-p T))
		      (process-char ()
			(let ((char (read-char stream NIL eof)))
			  (cond ((eq char eof)
				 building-token-p)
				((not (token-char-p char))
				 (unread-char char stream)
				 T)
				((eql char #\\)
				 (let ((c (read-char stream NIL eof)))
				   (when (eq c eof)
				     (error "bad token"))
				   (add-to-token c)
				   (process-char)))
				(T
				 (add-to-token char)
				 (process-char))))))
	       (process-char)))))
      (if building-token-p
	  token
	(read-char stream eof-error-p eof-value)))))

(defun read-lml-tag (stream &optional (eof-error-p T) eof-value)
  (let ((tag-name (read-lml-token stream NIL :eof))
	(attribs nil)
	(body '())
	(first-token-p T))
    (if (eq tag-name :eof)
	(read-char stream eof-error-p eof-value)
      (labels ((read-tag-expr (prev-token)
		 (let ((token (read-lml-token-or-whitespace stream NIL :eof)))
		   (cond ((eq token :eof)
			  (read-char stream eof-error-p eof-value))
			 ((eq token :close-tag)
			  (create-tag tag-name attribs (nreverse body)))
			 ((eq token :open-tag)
			  (let ((element (read-lml-tag stream NIL :eof)))
			    (if (eq element :eof)
				(read-char stream eof-error-p eof-value)
			      (progn (push element body)
				     (setf first-token-p NIL)
				     (read-tag-expr token)))))
			 ((eq token :open-attribs)
			  (if (or first-token-p
				  (whitespace-token-p prev-token))
			      (progn (setf attribs (read-attribs stream eof-error-p eof-value))
				     (setf first-token-p NIL)
				     (setf body '())
				     (read-tag-expr token))
			    (error "Can't have attribs here: ~S" tag-name)))
			 (T
			  (push token body)
			  (setf first-token-p NIL)
			  (read-tag-expr token))))))
	(read-tag-expr nil)))))

(defstruct tag
  name
  attribs
  body)

(defun create-tag (name attribs body)
  (make-tag :name name :attribs attribs :body body))

(defun whitespace-char-p (char)
  (member char '(#\space #.(code-char 10) #.(code-char 13) #\tab)))

(defun whitespace-token-p (string)
  (every #'whitespace-char-p string))

(defun eat-whitespace (stream &optional eaten-chars)
  (let ((char (read-char stream NIL :eof)))
    (if (whitespace-char-p char)
	(eat-whitespace stream (push char eaten-chars))
      (progn (unless (eq char :eof)
	       (unread-char char stream))
	     (if eaten-chars
		 (coerce (nreverse eaten-chars) 'string)
	       nil)))))

(defun tag-attribs-xml (tag)
  (with-output-to-string (xml)
    (dolist (a (tag-attribs tag))
      (format xml " ~A=\"~A\"" (first a) (second a)))))
  
(defmethod write-xml ((self tag) &optional (stream *standard-output*))
  (format stream "<~A" (tag-name self))
  (when (tag-attribs self)
    (format stream "~A" (tag-attribs-xml self)))
  (if (tag-body self)
      (progn
	(format stream ">")
	(write-tag-body-xml self stream)
	(format stream "</~A>" (tag-name self)))
    (format stream "/>")))

(defun write-tag-body-xml (tag stream)
  (let ((body (tag-body tag)))
    (do ((elements body (cdr elements))
	 (prev-element nil (car elements)))
	((endp elements))
      (let ((element (car elements)))
	(cond ((and (null prev-element)
		    (whitespace-token-p element)))
	      (T
	       (write-xml element stream)))))))

(defmethod write-xml ((self string) &optional (stream *standard-output*))
  (format stream "~A" self))
