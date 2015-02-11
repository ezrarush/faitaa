(in-package #:faitaa-server)

(defvar *server-socket* nil)
(defvar *current-remote-host*)
(defvar *current-remote-port*)

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
    
   (setf (current-world-state *server-state*) (current-world-state (scene *server-state*))) ;; server-state and scene share same world-state object
   (setf (previous-world-state *server-state*) (previous-world-state (scene *server-state*))) 
    
    (unwind-protect
	 (sdl2:with-event-loop (:method :poll)
	   (:idle
	    ()
	    
	    (read-messages)
	    
	    (when (> (hash-table-count *clients*) 0)
	      (setf (current-time *server-state*) (sdl2:get-ticks))
	      (loop while (>= (- (current-time *server-state*) (last-tick-time *server-state*)) (tick-time *server-state*)) do
		   (incf (last-tick-time *server-state*) (tick-time *server-state*))
		   (tick (last-tick-time *server-state*))
		 ;; (send-stuff)
		   )))
	   (:quit () t))
      (stop-server))))

(defun tick (time)
  
  (let* ((update-ptr (world-state-time (current-world-state *server-state*)))
	 (next-event (get-next-event (history *server-state*) (- update-ptr 1)))
	 (next-frame-time (+ update-ptr (frame-time-in-ms *server-state*)))
	 (last-frame-time 0)
	 (first-ws-passed nil)
	 (start-of-last-tick (- time (tick-time *server-state*))))
    
    (loop while (or (<= next-frame-time time) 
		    (and next-event 
			 (<= (event-time next-event) time))) do
	 (if (or (not next-event) (< next-frame-time (event-time next-event)))
	   (progn
	     ;; (format t "no events to deal with before reaching the next frame~%")
	     (update-world-at (scene *server-state*) next-frame-time)
	     (setf last-frame-time next-frame-time)
	     (incf next-frame-time (frame-time-in-ms *server-state*)))
	   (progn
	     (format t "there's an event we have to deal with before the end of this frame~%")
	     (setf update-ptr (event-time next-event))
	     (setf next-event (get-next-event (history *server-state*) update-ptr))))))

  (loop for channel being the hash-value in network-engine:*channels* do
       (send-message channel (make-world-state-message (network-engine:sequence-number channel) (network-engine:remote-sequence-number channel) (network-engine:generate-ack-bitfield channel)))
       ;; (network-engine:update-metrics channel)
       ))

