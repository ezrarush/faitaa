(in-package #:faitaa-server)

(defun read-messages ()
  (loop until (not (usocket:wait-for-input *server-socket* :timeout 0 :ready-only t)) do
    (multiple-value-bind (buffer size remote-host remote-port)
	(usocket:socket-receive *server-socket* (make-array 32768 :element-type '(unsigned-byte 8) :fill-pointer t) nil)
      (setf *current-remote-host* remote-host)
      (setf *current-remote-port* remote-port)
      (handle-message-from-client buffer))))

(defun send-message (channel buffer)
  (usocket:socket-send *server-socket*
		       buffer
		       (length buffer)
		       :host (network-engine:remote-host channel)
		       :port (network-engine:remote-port channel))
  (network-engine:process-sent-packet channel (sdl2:get-ticks) (length buffer)))

(defun handle-message-from-client (message)
  (userial:with-buffer message
    (userial:buffer-rewind)
    (ecase (userial:unserialize :client-opcode)
      (:first-contact  (handle-first-contact-message message))
      (:event  (handle-event-message message))
      (:disconnect (handle-disconnect-message message)))))

(defun handle-first-contact-message (message) 
  (userial:with-buffer message
    (userial:unserialize-let* (:string name)
      (assert (plusp (length name)))
      (let ((client (make-client name))
	    (channel (network-engine:make-channel *current-remote-host* *current-remote-port*)))
	(setf (channel client) channel)  
	(send-message channel (make-handshake-message (network-engine:sequence-number channel) (network-engine:remote-sequence-number channel) (network-engine:generate-ack-bitfield channel) (client-id client)))
	(format t "client ~a logged in~%" (client-id client))
	(finish-output))))
  (when (= (hash-table-count *clients*) 2)
    (format t "2 players found, starting match~%")
    (finish-output)  
    ;; (setf (current-mode *game-state*) :match)
    (loop for channel being the hash-value in network-engine:*channels* do
	 (send-message channel (make-match-begin-message (network-engine:sequence-number channel) (network-engine:remote-sequence-number channel) (network-engine:generate-ack-bitfield channel))))))

(defun handle-event-message (message)
  (userial:with-buffer message 
    (userial:unserialize-let* (:uint32 sequence :uint32 ack :uint32 ack-bitfield)
			      (network-engine:process-received-packet (network-engine:lookup-channel-by-port *current-remote-port*) sequence ack ack-bitfield))))

(defun handle-disconnect-message (message)
  (userial:with-buffer message 
    (userial:unserialize-let* (:uint32 sequence :uint32 ack  :uint32 ack-bitfield :int32 client-id)
			      (network-engine:process-received-packet (network-engine:lookup-channel-by-port *current-remote-port*) sequence ack ack-bitfield)
			      (assert client-id)
			      (let ((client (lookup-client-by-id client-id)))
				(remove-client client)
				(format t "client ~a: logged out~%" client-id)
				(finish-output)))))

(defun make-handshake-message (sequence ack ack-bitfield client-id)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :server-opcode :handshake
			:uint32 sequence
			:uint32 ack
			:uint32 ack-bitfield
			:int32 client-id)))

(defun make-match-begin-message (sequence ack ack-bitfield)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :server-opcode :match-begin
			:uint32 sequence
			:uint32 ack
			:uint32 ack-bitfield)))

(defun make-world-state-message (sequence ack ack-bitfield data)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :server-opcode :world-state
			:uint32 sequence
			:uint32 ack
			:uint32 ack-bitfield
			:int32 data)))
