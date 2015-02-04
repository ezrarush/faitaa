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
	     (progn
	       (connect-to-server server-ip port)
	       (unwind-protect
		    (sdl2:with-event-loop (:method :poll)
		      (:keydown
		       (:keysym keysym)
		       (let ((scancode (sdl2:scancode-value keysym))
			     (input-state (current-input-state *game-state*)))
			 (ecase (current-screen *game-state*)
			   (:title-screen)
			   (:game-play
			    (cond
			      ((sdl2:scancode= scancode :scancode-w) 
			       (setf (input-state-up-p input-state) t) 
			       (format t "~a~%" "KEYDOWN:WALK UP"))
			      ((sdl2:scancode= scancode :scancode-a)
			       (setf (input-state-left-p input-state) t)
			       (format t "~a~%" "KEYDOWN:WALK LEFT"))
			      ((sdl2:scancode= scancode :scancode-s)
			       (setf (input-state-attack-p input-state) t)
			       (format t "~a~%" "KEYDOWN:WALK DOWN"))
			      ((sdl2:scancode= scancode :scancode-d)
			       (setf (input-state-right-p input-state) t)
			       (format t "~a~%" "KEYDOWN:WALK RIGHT"))))
			   (:end-score))
			 (format t "input-state: ~a~%"
				 input-state)))
		       (:keyup
			(:keysym keysym)
			(let ((scancode (sdl2:scancode-value keysym))
			      (input-state (current-input-state *game-state*)))
			 (ecase (current-screen *game-state*)
			   (:title-screen)
			   (:game-play
			    (cond
			      ((sdl2:scancode= scancode :scancode-w)
			       (setf (input-state-up-p input-state) nil)
			       (format t "~a~%" "KEYUP:WALK UP"))
			      ((sdl2:scancode= scancode :scancode-a)
			       (setf (input-state-left-p input-state) nil)
			       (format t "~a~%" "KEYUP:WALK LEFT"))
			      ((sdl2:scancode= scancode :scancode-s) 
			       (setf (input-state-attack-p input-state) nil)
			       (format t "~a~%" "KEYUP:WALK DOWN"))
			      ((sdl2:scancode= scancode :scancode-d) 
			       (setf (input-state-right-p input-state) nil)
			       (format t "~a~%" "KEYUP:WALK RIGHT"))))
			   (:end-score))
			 (format t "input-state: ~a~%"
				 input-state)))
		      (:mousebuttondown 
		       (:x x :y y :button button)
		       (ecase (current-screen *game-state*)
			 (:title-screen
			  (format t "sending login message to server~%")
			  (finish-output)		
			  (send-message (make-login-message name))
			  ;; (setf (current-screen *game-state*) :waiting-for-opponent)
			  )
;;			 (:waiting-for-opponent)
			 (:game-play)
			 (:end-score)))
		      (:idle
		       ()
		       (read-message)
		       (setf *delta-time* (- (sdl2:get-ticks) *last-time*))
		       (when (>= *delta-time* 100)
			 (incf *last-time* 100)
			 (when *client-id* (send-message (make-input-message (network-engine:sequence-number *channel*) (network-engine:remote-sequence-number *channel*) (network-engine:generate-ack-bitfield *channel*))))
			 (network-engine:update-metrics *channel*))
		       (render-scene)
		       (sdl2:gl-swap-window win))
		      (:quit () t))	   
		 (format t "logging out~%")
		 (finish-output)
		 (send-message (make-logout-message (network-engine:sequence-number *channel*) (network-engine:remote-sequence-number *channel*) (network-engine:generate-ack-bitfield *channel*)))))
	  (disconnect-from-server))))))
