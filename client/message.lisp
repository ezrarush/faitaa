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
    (userial:unserialize-let* (:uint32 sequence :uint32 ack :uint32 ack-bitfield)
			      ;; (network-engine:process-received-packet (network-engine:lookup-channel-by-port *current-remote-port*) sequence ack ack-bitfield)
			      )
    (ecase (userial:unserialize :server-opcode)
      (:handshake     (handle-handshake-message message))
      (:world-state (handle-world-state-message message)))))

(defun handle-handshake-message (message)
  (userial:with-buffer message
    (userial:unserialize-let* (:int32 client-id)
			      (setf (current-screen *client-state*) :game-play)
			      (setf (client-id *client-state*) client-id))))

(defun handle-world-state-message (message)
  (userial:with-buffer message
    (userial:unserialize-let* (:int32 entity-count)
			      (let ((ws (make-world-state)))
				(loop repeat entity-count do
				     (userial:unserialize-let* (:uint32 entity-id :float32 x :float32 y)
							       (push (make-entity-status :owner entity-id :entity-id entity-id :pos (sb-cga:vec x y 1.0)) (world-state-entities ws))))
				(set-world-state (scene *client-state*) ws)))))

(defun make-first-contact-message (name)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :uint32 0
			:uint32 0
			:uint32 0
			:client-opcode :first-contact
			:string name)))

(defun make-event-message (sequence ack ack-bitfield event)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :uint32 sequence
			:uint32 ack
			:uint32 ack-bitfield
			:client-opcode :event
			:event-type :move
			:uint32 (event-time event)
			:uint32 (event-entity-id event)
			:boolean (input-state-left-p (event-input event))
			:boolean (input-state-right-p (event-input event))
			:boolean (input-state-up-p (event-input event))
			:boolean (input-state-attack-p (event-input event))
			:boolean (input-state-block-p (event-input event)))))

(defun make-disconnect-message (sequence ack ack-bitfield)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :uint32 sequence
			:uint32 ack
			:uint32 ack-bitfield
			:client-opcode :disconnect
			:int32 (client-id *client-state*))))
