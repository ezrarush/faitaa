(in-package #:faitaa-client)

(defvar *server-connection* nil)
(defvar *channel*)
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
	(setf *last-time* (sdl2:get-ticks))
	(unwind-protect
	     (let ((current-input-state (make-input-state)))
	       (connect-to-server server-ip port)
	       (unwind-protect
		    (sdl2:with-event-loop (:method :poll)
		      
		      (:keydown
		       (:keysym keysym)
		       (let ((scancode (sdl2:scancode-value keysym)))
			 (ecase (current-screen *game-state*)
			   (:title-screen)
			   (:game-play
			    (cond
			      ((sdl2:scancode= scancode :scancode-w)
			       (setf (input-state-up-p current-input-state) t))
			      ((sdl2:scancode= scancode :scancode-a)
			       (setf (input-state-left-p current-input-state) t))
			      ((sdl2:scancode= scancode :scancode-s)
			       (setf (input-state-attack-p current-input-state) t))
			      ((sdl2:scancode= scancode :scancode-d)
			       (setf (input-state-right-p current-input-state) t))))
			   (:end-score))))
		      
		       (:keyup
			(:keysym keysym)
			(let ((scancode (sdl2:scancode-value keysym)))
			 (ecase (current-screen *game-state*)
			   (:title-screen)
			   (:game-play
			    (cond
			      ((sdl2:scancode= scancode :scancode-w)
			       (setf (input-state-up-p current-input-state) nil))
			      ((sdl2:scancode= scancode :scancode-a)
			       (setf (input-state-left-p current-input-state) nil))
			      ((sdl2:scancode= scancode :scancode-s) 
			       (setf (input-state-attack-p current-input-state) nil))
			      ((sdl2:scancode= scancode :scancode-d) 
			       (setf (input-state-right-p current-input-state) nil))))
			   (:end-score))))
		       
		      (:mousebuttondown
		       (:x x :y y :button button)
		       (ecase (current-screen *game-state*)
			 (:title-screen
			  (format t "connecting to server~%")
			  (finish-output)
			  (send-message (make-first-contact-message name)))
			 (:game-play)
			 (:end-score)))
		      
		      (:idle
		       ()
		       
		       ; input check
		       (unless (equalp current-input-state (input-state *game-state*))
			 (setf (input-state *game-state*) (copy-structure current-input-state))
			 (let ((event (make-event :type :move 
						  :input current-input-state 
						  :time (sdl2:get-ticks)
						  :entity-id (client-id *game-state*)
						  :owner (client-id *game-state*))))
			   (add (history *game-state*) event)
			   ; set player state 
			   ;; (set-client-input-state-at (scene *game-state*) (event-input event) (event-time event))
			   ))
		       
		       (read-messages)
		       
		       ;; tick
		       (setf *delta-time* (- (sdl2:get-ticks) *last-time*))
		       (when (>= *delta-time* (tick-time *game-state*))
			 (incf *last-time* (tick-time *game-state*))
			 (when (client-id *game-state*) 
			   (send-message (make-event-message (network-engine:sequence-number *channel*) (network-engine:remote-sequence-number *channel*) (network-engine:generate-ack-bitfield *channel*))))
			 (network-engine:update-metrics *channel*))
		       (render-scene)
		       (sdl2:gl-swap-window win))
		      
		      (:quit () t))
		 
		 (format t "disconnecting from server~%")
		 (finish-output)
		 (send-message (make-disconnect-message (network-engine:sequence-number *channel*) (network-engine:remote-sequence-number *channel*) (network-engine:generate-ack-bitfield *channel*)))))
	  (disconnect-from-server))))))
