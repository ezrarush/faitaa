(in-package #:faitaa-server)

(defvar *server-socket* nil)
(defvar *current-remote-host*)
(defvar *current-remote-port*)

(defvar *last-time*)
(defvar *delta-time*)

(defun start-server (server-ip port)
  (assert (not *server-socket*))
  (setf *server-socket*
	(usocket:socket-connect nil 
				nil
				:protocol :datagram
				:element-type '(unsigned-byte 8)
				:local-host server-ip
				:local-port port)))

(defun stop-server ()
  (assert *server-socket*)
  (usocket:socket-close *server-socket*)
  (setf *server-socket* nil))

(defun server-main (&key (server-ip usocket:*wildcard-host*) (port 2448))
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
	    sdl2-ffi:+sdl-major-version+
	    sdl2-ffi:+sdl-minor-version+
	    sdl2-ffi:+sdl-patchlevel+)
    (finish-output)
    (start-server server-ip port)
    
   (setf (current-world-state *game-state*) (current-world-state (scene *game-state*))) ;; necessary???
   (setf (previous-world-state *game-state*) (previous-world-state (scene *game-state*))) ;;necessary ???
    
    (setf *last-time* (sdl2:get-ticks))
    (unwind-protect
	 (sdl2:with-event-loop (:method :poll)
	   (:idle
	    ()
	    (read-messages)
	    (setf *delta-time* (- (sdl2:get-ticks) *last-time*))
	    (when (>= *delta-time* (tick-time *game-state*))
	      (incf *last-time* (tick-time *game-state*))
	      (tick)))
	   (:quit () t))
      (stop-server))))

(defun tick ()
  (loop for channel being the hash-value in network-engine:*channels* do
       (send-message channel (make-world-state-message (network-engine:sequence-number channel) (network-engine:remote-sequence-number channel) (network-engine:generate-ack-bitfield channel)))
       (network-engine:update-metrics channel)))

