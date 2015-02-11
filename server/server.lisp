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
	      (tick)
	      (incf *last-time* (tick-time *game-state*))))
	   (:quit () t))
      (stop-server))))

(defun tick ()
  
  (let* ((update-ptr (world-state-time (current-world-state *game-state*)))
	 (next-event (get-next-event (history *game-state*) (- update-ptr 1)))
	 (next-frame-time (+ update-ptr (frame-time-in-ms *game-state*)))
	 (last-frame-time 0)
	 (first-ws-passed nil)
	 (start-of-last-tick (- (sdl2:get-ticks) (tick-time *game-state*))))
    
    ;; (loop while (or (<= next-frame-time (sdl2:get-ticks)) 
    ;; 		    (and next-event 
    ;; 			 (<= (event-time next-event) (sdl2:get-ticks)))) do
    ;; 	 (if (or (eq (event-type next-event) :null-event) (< next-frame-time (event-time next-event)))
    ;; 	   (progn
    ;; 	     (format t "no events to deal with before reaching the next frame~%"))
    ;; 	   (progn
    ;; 	     (format t "there's an event we have to deal with befor the end of this frame~%"))))
    )

  (loop for channel being the hash-value in network-engine:*channels* do
       (send-message channel (make-world-state-message (network-engine:sequence-number channel) (network-engine:remote-sequence-number channel) (network-engine:generate-ack-bitfield channel)))
       ;; (network-engine:update-metrics channel)
       ))

