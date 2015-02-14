(in-package #:faitaa-server)

(defclass server ()
  ((protocol-id
    :initarg :protocol-id)
   (local-sequenc
    :initform 1)
   (pause
    :initform 5)
   
   (scene
    :initform (make-instance 'scene)
    :accessor scene)
   
   ;; (client)
   
   (socket
    :initform nil)
   
   (port
    :initarg :port)
   
   (expected-clients
    :initform 1)
   
   (everyone-synced-p
    :initform nil)
   (sync-attempted-p
    :initform nil)
   
   (clock
    :initform (make-instance 'clock)
    :accessor clock)
   (current-time
    :accessor current-time)
   (last-tick-time
    :accessor last-tick-time)
   
   (waiting-for-ack
    :initform (make-hash-table))
   (sent
    :initform (make-hash-table))
   (urgent-messages
    :initform (list))
   (out-going-messages
    :initform (list))
   (history
    :initform (make-instance 'history)
    :accessor history)

   (isc
    :initform (make-isc)
    :accessor isc
    ) ; for keeping track of input changes in the latest tick
   
   (current-world-state
    ;; :initform (make-world-state)
    :accessor current-world-state)
   (previous-world-state
    ;; :initform (make-world-state)
    :accessor previous-world-state)
   
   (previous-hit-queue)
   
   (in-packet)
   (in-addr)
   (in-port)
   (in-pid
    :accessor in-pid)
   (in-uid
    :accessor in-uid)
   (in-seq
    :accessor in-seq)
   (in-ack
    :accessor in-ack)
   (in-rtt)
   (in-msg)
   
   (out-message)

   (last-sent-cleanup
    :initform 0)
   (last-history-cleanup
    :initform 0)
   (last-wsa-cleanup
    :initform 0)
   (last-evt-sequence-cleanup
    :initform 0)
   
   (sent-this-second)
   (outbox-timer
    :initform 0)
   (outbox-clock
    :initform (make-instance 'clock)
    :accessor outbox-clock)
   
   (max-message-count
    :initform 100)  ; max number of messages in a second
   (handshake-needed
    :initform 8)
   (suspicious-rtt
    :initform 250)
   
   (resend-time
    :initform 400)
   (sent-timeout
    :initform 4000)
   (tick-time
    :initform (/ 1000 20)
    :reader tick-time)
   (history-timeout
    :initform 4000)
   (history-length
    :initform 6000)
   (wsa-timeout
    :initform 4000)
   (wsa-length
    :initform 6000)

   (frame-time-in-ms
    :initform (/ 1000 60)
    :reader frame-time-in-ms)))

(defmethod initialize-instance :after ((self server) &key)
  (with-slots (port scene current-world-state previous-world-state) self
    (start-server self)
    (setf current-world-state (current-world-state scene))
    (setf previous-world-state (previous-world-state scene))
    (format t "Server bound to port ~a~%" port)
    (finish-output)))

(defmethod start-server ((self server))
  (with-slots (socket port) self
    (assert (not socket))
    (setf socket
	  (usocket:socket-connect nil 
				  nil
				  :protocol :datagram
				  :element-type '(unsigned-byte 8)
				  :local-host usocket:*wildcard-host*
				  :local-port port))))

(defmethod stop-server ((self server))
  (with-slots (socket) self
    (assert socket)
    (usocket:socket-close socket)
    (setf socket nil)))

(defmethod roger-that ((self server))
  (with-slots (in-port in-seq ) self
    (let* ((discard nil)
	   (client (lookup-client-by-port in-port))
	   (previous (sequence client)))
      (if (later-sequence in-seq previous)
	  (let ((shift (- in-seq previous)))
	    (setf (acks client) (ash (acks client shift)))
	    (setf (acks client) (boole boole-ior (acks client) (ash 1 (- shift 1))))
	    (setf (sequence client) in-seq))
	  (if (not (eq in-seq previous))
	      (let ((shift (- previous in-seq)))
		(if (and (>= (- shift 1) 0) (< (- shift 1) 32))
		    (if (logbitp (- shift 1) (acks client))
			(setf discard t)
			(setf (acks client) (boole boole-ior (acks client) (ash 1 (- shift 1)))))
		    (setf discard t)))
	      (setf discard t)))
      discard)))

(defmethod receive-stuff ((self server))
  (with-slots (socket in-addr in-port in-pid in-uid in-seq in-ack in-msg in-rtt protocol-id sent waiting-for-ack) self
    (when (usocket:wait-for-input socket :timeout 0 :ready-only t) 
      (multiple-value-bind (buffer size remote-host remote-port)
	  (usocket:socket-receive socket (make-array 32768 :element-type '(unsigned-byte 8) :fill-pointer t) nil)
	
	(setf in-packet buffer)
	(setf in-addr remote-host)
	(setf in-port remote-port)
	
	(userial:with-buffer in-packet
	  (userial:buffer-rewind)
	  (userial:unserialize-let* (:uint32 protocol-id :uint32 client-id :uint32 remote-sequence :uint32 ack :msg message-type)
				    
				    (setf in-pid protocol-id)
				    (setf in-uid client-id)
				    (setf in-seq remote-sequence)
				    (setf in-ack ack)
				    (setf in-msg message-type)
				    
				    (when (eq in-pid protocol-id)
				      (let ((discard-p nil))
					(unless (= in-msg :first-contact)
					  (setf discard-p (roger-that)))
					(unless disard-p
					  (unless (= in-msg :first-contact)
					    (setf (last-ack (lookup-client-by-port in-port)) in-ack))
					  
					  ;; set inRTT if handshake
					  (when (and (not (gethash in-ack sent)) (= in-msg :handshake))
					    (setf in-rtt (- (sdl2:get-ticks) (time-sent (gethash in-ack sent)))))
					  
					  ;; remove acked message from sent and waitingForAck queues
					  (remhash (time-sent (gethash in-ack sent)) waiting-for-ack)
					  (remhash in-ack sent)

					  ;; dispatch, depending on messsage type
					  (ecase in-msg
					    (:event  (incoming-events self))
					    (:first-contact  (incoming-first-contact self))
					    (:handshake (incoming-handshake self))
					    (:ready (incoming-ready self))
					    (:unready (incoming-unready self))
					    (:sync-clocks (incoming-sync))))))))))))

(defmethod run ((self server))
  (with-slots (port current-time  everyone-synced-p last-tick-time tick-time) self
    (sdl2:with-init (:everything)
      (unwind-protect
	   (sdl2:with-event-loop (:method :poll)
	     (:keyup
	      (:keysym keysym)
	      (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
		(sdl2:push-event :quit)))
	     (:idle
	      ()
	      (receive-stuff self)
	      (setf current-time (sdl2:get-ticks))
	      (if everyone-synced-p
		  (loop while (>= (- current-time last-tick-time) tick-time) do
		       (incf last-tick-time tick-time)
		       (tick last-tick-time)
		       (send-stuff self))
		  (send-stuff self)))
	     (:quit () t))
	;; cleanup
	(format t "stopping server")
	(finish-output)
	(stop-server)))))

(defmethod send-stuff ((self server))
  (with-slots (urgent-messages current-time out-message socket sent waiting-for-ack outbox-timer outbox-clock sent-this-second max-message-count resend-time out-going-messages) self
    (loop while urgent-messages do
	 (setf current-time (sdl2:get-ticks))
	 (setf out-message (pop urgent-messages))
	 (setf (time-sent out-message) current-time)
	 
	 (usocket:socket-send socket
			      (packet out-message)
			      (length (packet out-message))
			      :host (addr out-message)
			      :port (port out-message)) 
	 (setf (gethash (sequence out-message) sent) out-message)
	 (unless (= (ttl out-message) 0)
	   (setf (gethash current-time waiting-for-ack) out-message)))
    
    (incf outbox-timer (restart-clock outbox-clock))
    (when (>= outbox-timer 1000)
      (setf sent-this-second 0)
      (decf outbox-timer 1000))
    
    (when (and (> (hash-table-count waiting-for-ack) 0) (< sent-this-second max-message-count))
      (setf current-time (sdl2:get-ticks))
      (let ((time-key (loop for time being the hash-key in waiting-for-ack thereis (> time 0)))) ;; bug warning, hashtable not ordered!
	(when (>= (- current-time time-key) resend-time)
	  (let ((msg (gethash time-key waiting-for-ack)))
	    (remhash time-key waiting-for-ack)
	    (setf (time-sent (gethash (sequence msg) sent)) current-time)
	    
	    (usocket:socket-send socket
				 (packet msg)
				 (length (packet msg))
				 :host (addr msg)
				 :port (port msg))
	    (decf (ttl msg))
	    (unless (= (ttl msg) 0)
	      (setf (time-sent msg) current-time)
	      (setf (gethash (time-sent msg) waiting-for-ack) msg))
	    (incf sent-this-second)))))
    
    (when (and out-going-messages (< sent-this-second max-message-count))
      (let ((msg (pop out-going-messages))))
      (setf current-time (sdl2:get-ticks))
      (setf (sent-time msg) current-time)
      (usocket:socket-send socket
			   (packet msg)
			   (length (packet msg))
			   :host (addr msg)
			   :port (port msg))
      (setf (gethash (sequence msg) sent) msg)
      
      (unless (= (ttl msg) 0)
	(setf (gethash current-time waiting-for-ack) msg))
      (incf sent-this-second))))

(defun tick (time)
  
  (let* ((update-ptr (world-state-time (current-world-state *server-state*)))
	 (next-event (get-next-event (history *server-state*) (- update-ptr 1)))
	 (next-frame-time (+ update-ptr (frame-time-in-ms *server-state*)))
	 (last-frame-time 0)
	 (first-ws-passed nil)
	 (start-of-last-tick (- time (tick-time *server-state*))))
    
    ;; rewind time if needed
    ;; (when (< (oldest-event-this-tick (history *server-state*)) (world-state-time (current-world-state *server-state*)))
    ;;   (format t "rewind time~%")
    ;;   (rewind-world-to (scene *server-state*) (oldest-event-this-tick (history *server-state*))))
    
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
	     (setf next-event (get-next-event (history *server-state*) update-ptr)))))
    
    ;; deal with leftover time when tick doesn't end exactly at tick-time
    (when (< last-frame-time time)
      (update-world-at (scene *server-state*) time))
    
    ;; calculate client's last-command deltas and last agreed status
    
    ;; add our own event, reset history's oldest-event-this-tick, events in next tick will be compared to this ws-event
    ;; (add-event (history *server-state*) (make-event :type :state-refresh :time time))
    ;; (reset-oldest-event (history *server-state*))
    
    ;; (update-prev-world-state (scene *server-state*))
    
    ;; save current state
    ;; (save-current (scene *server-state*) time)
    
    ;; finally, broadcast event
    (loop for channel being the hash-value in network-engine:*channels* do
	 (send-message channel (make-world-state-message (network-engine:sequence-number channel) (network-engine:remote-sequence-number channel) (network-engine:generate-ack-bitfield channel)))
       ;; (network-engine:update-metrics channel)
	 )))

(defun handle-first-contact-message (message) 
  (userial:with-buffer message
    (userial:unserialize-let* (:string name)
      (assert (plusp (length name)))
      (let ((client (make-client name))
	    ;; (channel (network-engine:make-channel *current-remote-host* *current-remote-port*))
	    )
	(setf (channel client) channel)  
	(send-message channel (make-handshake-message (network-engine:sequence-number channel) (network-engine:remote-sequence-number channel) (network-engine:generate-ack-bitfield channel) (client-id client)))
	(setf (entity-id client) (add-entity (scene *server-state*) (client-id client) :red))
	(setf (entity-status-entity-id (last-agreed-status client)) (entity-id client))
	(setf (entity-status-owner (last-agreed-status client)) (client-id client))
	(setf (gethash (client-id client) (isc-isc-count (isc *server-state*))) 0) ;;mISC.mIscCount[i] = 0; // setting up input change records
	(format t "~a (client-id: ~a) has connected.~%" name (client-id client))
	(finish-output))))

  ;; start ticking when a client connects
  (when (= (hash-table-count *clients*) 1)
    (setf (last-tick-time *server-state*) (sdl2:get-ticks)))
  
  ;; (when (= (hash-table-count *clients*) 2)
  ;;   (format t "2 players found, starting match~%")
  ;;   (finish-output)  
  ;;   ;; (setf (current-mode *server-state*) :match)
  ;;   (loop for channel being the hash-value in network-engine:*channels* do
  ;; 	 (send-message channel (make-match-begin-message (network-engine:sequence-number channel) (network-engine:remote-sequence-number channel) (network-engine:generate-ack-bitfield channel)))))
  )

 (defun handle-event-message (message)
  (userial:with-buffer message 
    (userial:unserialize-let* (:event-type type :uint32 time :uint32 entity-id :boolean left-p :boolean right-p :boolean up-p :boolean attack-p :boolean block-p)
      
      ;; (sync time owner)
      (add-event (history *server-state*) (make-event :time time 
						    :type type 
						    :input (make-input-state :left-p left-p :right-p right-p :up-p up-p :attack-p attack-p :block-p block-p)
						    :entity-id entity-id))

      )))

(defun handle-disconnect-message (message)
  (userial:with-buffer message 
    (userial:unserialize-let* (:int32 client-id)
			      (network-engine:process-received-packet (network-engine:lookup-channel-by-port *current-remote-port*) sequence ack ack-bitfield)
			      (assert client-id)
			      (let ((client (lookup-client-by-id client-id)))
				(remove-client client)
				(format t "~a (client-id: ~a) has disconnected.~%" (name client) client-id)
				(finish-output)))))

(defun make-handshake-message (sequence ack ack-bitfield client-id)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :uint32 sequence
			:uint32 ack
			:uint32 ack-bitfield
			:server-opcode :handshake
			:int32 client-id)))

(defun make-world-state-message (sequence ack ack-bitfield)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :uint32 sequence
			:uint32 ack
			:uint32 ack-bitfield
			:server-opcode :world-state
			:int32 (world-state-entity-count (current-world-state *server-state*)))
    (loop for entity-status in (world-state-entities (current-world-state *server-state*)) do
	 (userial:serialize* :uint32 (entity-status-entity-id entity-status)
			     :float32 (aref  (entity-status-pos entity-status) 0)
			     :float32 (aref (entity-status-pos entity-status) 1)))
    (userial:get-buffer)))
