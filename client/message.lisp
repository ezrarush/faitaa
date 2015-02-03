(in-package #:faitaa-client)

(defun send-message (buffer)
  (usocket:socket-send *server-connection*
		       buffer
		       (length buffer))
  (network-engine:process-sent-packet *channel* (sdl2:get-ticks) (length buffer)))

(defun read-message ()
  (loop until (not (usocket:wait-for-input *server-connection* :timeout 0 :ready-only t)) do 
    (handle-message-from-server (usocket:socket-receive *server-connection* (make-array 32768 :element-type '(unsigned-byte 8) :fill-pointer t) nil))))

(defun handle-message-from-server (message)
  (userial:with-buffer message
    (userial:buffer-rewind)
    (ecase (userial:unserialize :server-opcode)
      (:welcome     (handle-welcome-message message))
      (:update-data (handle-update-data-message message)))))

(defun handle-welcome-message (message)
  (userial:with-buffer message
    (userial:unserialize-let* (:uint32 sequence :uint32 ack :uint32 ack-bitfield :int32 client-id)
			      (network-engine:process-received-packet *channel* sequence ack ack-bitfield)
			      (setf *client-id* client-id))))

(defun handle-update-data-message (message)
  (userial:with-buffer message
    (userial:unserialize-let* (:uint32 sequence :uint32 ack :uint32 ack-bitfield :int32 data)
			      (network-engine:process-received-packet *channel* sequence ack ack-bitfield))))

(defun make-login-message (name)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :login
			:string name)))

(defun make-input-message (sequence ack ack-bitfield)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :input
			:uint32 sequence
			:uint32 ack
			:uint32 ack-bitfield)))

(defun make-logout-message (sequence ack ack-bitfield)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :logout
			:uint32 sequence
			:uint32 ack
			:uint32 ack-bitfield
			:int32 *client-id*)))