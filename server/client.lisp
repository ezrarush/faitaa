(in-package #:faitaa-server)

(defvar *clients* (make-hash-table))

(let ((next-id 0))
  (defun make-client-id ()
    (incf next-id)))

(defun client-id-in-use-p (id)
  (multiple-value-bind (object exists) (gethash id *clients*)
    (declare (ignore id))
    exists))

(defun lookup-client-by-id (id)
  (multiple-value-bind (client exists) (gethash id *clients*)
    (unless exists (error "No client for id ~a." id))
    client))

(defun lookup-client-by-port (port)
  (let ((found nil))
    (loop for client being the hash-value in *clients* do
	 (when (eq (port client) port)
	   (setf found client)))
    found))

(defun add-client (client)
  (let ((id (client-id client)))
    (assert (not (client-id-in-use-p id)))
    (setf (gethash id *clients*) client)))

(defun remove-client (client)
  (let ((id (client-id client)))
    (assert (eql (lookup-client-by-id id) client))
    (remhash id *clients*)))

(defun all-clients-ready-p ()
  (let ((flag t))
    (loop for client being the hash-value in *clients* do
	 (when (eq (ready-p client) nil)
	   (return (setf flag nil))))
    flag))

(defun all-clients-synced-p ()
  (let ((flag t))
    (loop for client being the hash-value in *clients* do
	 (when (eq (synced-p client) nil)
	   (return (setf flag nil))))
    flag))

(defclass client ()
  ((name 
    :initarg :name
    :accessor name)
   (client-id 
    :type integer
    :initform (make-client-id)
    :accessor client-id)
   (entity-id
    :accessor entity-id)
   (addr
    :initarg :addr
    :accessor addr)
   (port
    :initarg :port
    :accessor port)
   (rtt
    :initform 0)
   (sequence
    :initform 0)
   (last-ack
    :initform 0
    :accessor last-ack)
   (acks
    :initform 0)
   (handshaken
    :initform 0)
   (connected-p
    :initform nil)
   (synced-p
    :initform nil)
   (ready-p
    :initform nil)
   (last-seen-command-time
    :initform 0)
   (delta-to-first-tick
    :initform 0
    :accessor delta-to-first-tick)
   (last-agreed-status
    :initform (make-entity-status))
   (first-delta-set-p
    :initform nil)))

(defmethod initialize-instance :after ((self client) &key)
  (add-client self))

(defmethod reset ((self client) new-host new-port new-name)
  (with-slots (name port addr rtt sequence last-ack acks hand-shaken connected-p synced-p ready-p last-seen-command-time delta-to-first-tick) self
    (setf name new-name)
    (setf port new-port)
    (setf addr new-host)
    (setf rtt 0)
    (setf sequence 0)
    (setf last-ack 0)
    (setf acks 0)
    (setf hand-shaken 0)
    (setf connected-p nil)
    (setf synced-p nil)
    (setf ready-p nil)
    (setf last-seen-command-time 0)
    (setf delta-to-first-tick 0)))

(defun make-client (host port name)
  (make-instance 'client :name name))

