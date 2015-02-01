(in-package #:faitaa-client)

(defvar *server-connection* nil)
(defvar *channel*)
(defvar *client-id* nil)

(defvar *last-time*)
(defvar *delta-time*)

(defun connect-to-server (server-ip port)
  (assert (not *server-connection*))
  (setf *server-connection* 
	(usocket:socket-connect server-ip
				port
				:protocol :datagram
				:element-type '(unsigned-byte 8)))
  (setf *channel* (network-engine:make-channel server-ip port)))

(defun disconnect-from-server ()
  (assert *server-connection*)
  (usocket:socket-close *server-connection*)
  (setf *server-connection* nil))

(defun send-packet (buffer)
  (usocket:socket-send *server-connection*
		       buffer
		       (length buffer))
  (network-engine:process-sent-packet *channel* (sdl2:get-ticks) (length buffer)))

(defun read-packet ()
  (loop until (not (usocket:wait-for-input *server-connection* :timeout 0 :ready-only t)) do 
    (handle-packet-from-server (usocket:socket-receive *server-connection* (make-array 32768 :element-type '(unsigned-byte 8) :fill-pointer t) nil))))

(defun handle-packet-from-server (packet)
  (userial:with-buffer packet
    (userial:buffer-rewind)
    (ecase (userial:unserialize :server-opcode)
      (:welcome     (handle-welcome-packet packet))
      (:update-data (handle-update-data-packet packet)))))

(defun handle-welcome-packet (packet)
  (userial:with-buffer packet
    (userial:unserialize-let* (:uint32 sequence :uint32 ack :uint32 ack-bitfield :int32 client-id)
			      (network-engine:process-received-packet *channel* sequence ack ack-bitfield)
			      (setf *client-id* client-id)
			      (format t "received packet ~a - ack with client id: ~A~%" sequence client-id)
			      (finish-output))))

(defun handle-update-data-packet (packet)
  (userial:with-buffer packet
    (userial:unserialize-let* (:uint32 sequence :uint32 ack :uint32 ack-bitfield :int32 data)
			      (network-engine:process-received-packet *channel* sequence ack ack-bitfield)
			      (format t "received packet ~a - data: ~A~%" sequence data)
			      (finish-output))))

(defun make-login-packet (name)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :login
			:string name)))

(defun make-input-packet (sequence ack ack-bitfield)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :input
			:uint32 sequence
			:uint32 ack
			:uint32 ack-bitfield)))

(defun make-logout-packet (sequence ack ack-bitfield)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :logout
			:uint32 sequence
			:uint32 ack
			:uint32 ack-bitfield
			:int32 *client-id*)))

(defun client-main (&key (server-ip "127.0.0.1") (port 2448) (name "anonymous"))
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
	    sdl2-ffi:+sdl-major-version+
	    sdl2-ffi:+sdl-minor-version+
	    sdl2-ffi:+sdl-patchlevel+)
    (finish-output)

    (sdl2:with-window (win :title (concatenate 'string "FAITAA: " name) :w *window-width* :h *window-height* :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
	(sdl2:gl-make-current win gl-context)
	
	(graphics-init)
	(connect-to-server server-ip port)
	(setf *last-time* (sdl2:get-ticks))
	(unwind-protect
	     (progn
	       (format t "sending login message to server~%")
	       (finish-output)
	       (send-packet (make-login-packet name))
	       (format t "waiting for ack~%")
	       (finish-output)
	       (unwind-protect
		    (sdl2:with-event-loop (:method :poll)
		      (:idle
		       ()
		       (read-packet)
		       (setf *delta-time* (- (sdl2:get-ticks) *last-time*))
		       (when (>= *delta-time* 100)
			 (incf *last-time* 100)
			 (when *client-id* (send-packet (make-input-packet (network-engine:sequence-number *channel*) (network-engine:remote-sequence-number *channel*) (network-engine:generate-ack-bitfield *channel*))))
			 (network-engine:update-metrics *channel*))
		       (render-scene)
		       (sdl2:gl-swap-window win))
		      (:quit () t))	   
		 (format t "logging out~%")
		 (finish-output)
		 (send-packet (make-logout-packet (network-engine:sequence-number *channel*) (network-engine:remote-sequence-number *channel*) (network-engine:generate-ack-bitfield *channel*)))))
	  (disconnect-from-server))))))