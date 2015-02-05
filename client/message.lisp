(in-package #:faitaa-client)

(defun send-message (buffer)
  (usocket:socket-send *server-connection*
		       buffer
		       (length buffer))
  (network-engine:process-sent-packet *channel* (sdl2:get-ticks) (length buffer)))

(defun read-messages ()
  (loop until (not (usocket:wait-for-input *server-connection* :timeout 0 :ready-only t)) do 
    (handle-message-from-server (usocket:socket-receive *server-connection* (make-array 32768 :element-type '(unsigned-byte 8) :fill-pointer t) nil))))

(defun handle-message-from-server (message)
  (userial:with-buffer message
    (userial:buffer-rewind)
    (ecase (userial:unserialize :server-opcode)
      (:handshake     (handle-handshake-message message))
      (:world-state (handle-world-state-message message)))))

(defun handle-handshake-message (message)
  (userial:with-buffer message
    (userial:unserialize-let* (:uint32 sequence :uint32 ack :uint32 ack-bitfield :int32 client-id)
			      (network-engine:process-received-packet *channel* sequence ack ack-bitfield)
			      (setf (current-screen *game-state*) :game-play)
			      (setf (client-id *game-state*) client-id))))

(defun handle-world-state-message (message)
  (userial:with-buffer message
    (userial:unserialize-let* (:uint32 sequence :uint32 ack :uint32 ack-bitfield :int32 data)
			      (network-engine:process-received-packet *channel* sequence ack ack-bitfield))))

(defun make-first-contact-message (name)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :first-contact
			:string name)))

(defun make-event-message (sequence ack ack-bitfield)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :event
			:uint32 sequence
			:uint32 ack
			:uint32 ack-bitfield)))

(defun make-disconnect-message (sequence ack ack-bitfield)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :disconnect
			:uint32 sequence
			:uint32 ack
			:uint32 ack-bitfield
			:int32 (client-id *game-state*))))
