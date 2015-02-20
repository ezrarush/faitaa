(in-package #:faitaa-server)

(defclass server ()
  ((protocol-id
    :initarg :protocol-id)
   (local-sequence
    :initform 1)
   (pause
    :initform 0.005)
   
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
   
   (out-message
    :initform (make-message))

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
	   (previous (client-sequence client)))
      (if (later-sequence in-seq previous)
	  (let ((shift (- in-seq previous)))
	    (setf (client-acks client) (ash (client-acks client) shift))
	    (setf (client-acks client) (boole boole-ior (client-acks client) (ash 1 (- shift 1))))
	    (setf (client-sequence client) in-seq))
	  (if (not (eq in-seq previous))
	      (let ((shift (- previous in-seq)))
		(if (and (>= (- shift 1) 0) (< (- shift 1) 32))
		    (if (logbitp (- shift 1) (client-acks client))
			(setf discard t)
			(setf (client-acks client) (boole boole-ior (client-acks client) (ash 1 (- shift 1))))) ;; TODO check exclusive vs inclusive
		    (setf discard t)))
	      (setf discard t)))
      discard)))

(defmethod incoming-events ((self server))
  (with-slots (in-packet history) self
    (userial:with-buffer in-packet
      (userial:unserialize-let* (:uint32 event-count)
	(loop repeat event-count do
	     (userial:unserialize-let* (:event-type type :int32 time :uint16 owner :uint16 entity-id)
	       (let ((event (make-event :time time 
					:owner owner
					:type type 
					:entity-id entity-id)))
		 (ecase type
		   (:move
		    (userial:unserialize-let* (:boolean left-p :boolean right-p :boolean up-p :boolean attack-p :boolean block-p)
		      (setf (input event) (make-input-state :left-p left-p :right-p right-p :up-p up-p :attack-p attack-p :block-p block-p))))
		   (:hit
		    (userial:unserialize-let* (:boolean left-p :boolean top-p :uint16 e-owner :uint16 u-id :boolean left-p :boolean right-p :boolean up-p :boolean attack-p :boolean block-p :int32 last-updated :float32 pos-x :float32 pos-y :float32 vel-x :float32 vel-y :int32 state-ptr :boolean in-air-p :boolean attacking-p :boolean hit-already-p :boolean hit-needs-release-p :int32 delta-since-lac)
		      (setf (event-letf-p event) letf-p)
		      (setf (event-top-p event) top-p)
		      (setf (event-target-status event) (make-entity-status :owner e-owner 
									    :entity-id u-id 
									    :current-input-state (make-input-state :left-p left-p 
														   :right-p right-p 
														   :up-p up-p 
														   :attack-p attack-p 
														   :block-p block-p)
									    :last-updated last-updated
									    :pos (sb-cga:vec pos-x pos-y 0.0)
									    :vel (sb-cga:vec vel-x vel-y 0.0)
									    :state-ptr state-ptr
									    :in-air-p in-air-p
									    :attacking-p attacking-p
									    :hit-already-p hit-already-p
									    :hit-needs-release-p hit-needs-release-p))
		      (setf (event-delta-since-lac event) delta-since-lac))))
		 (sync self (time event) (owner event))
		 (add-event history event))))))))

(defmethod incoming-first-contact ((self server))
  (with-slots (in-packet in-addr in-port waiting-for-ack out-message clock urgent-messages) self 
      (userial:with-buffer in-packet
	(userial:unserialize-let* (:string name)
	  (if (lookup-client-by-port in-port)
	      (progn
		(let ((waiting nil))
		  (loop for msg being the hash-value in waiting-for-ack do
		       (when (eq (message-addr msg) in-addr)
			 (setf waiting t)))
		  (if waiting
		    (format t "flooding reconnect from address ~a; port ~a~%" in-addr in-port)
		    (progn
		      (format t "existing client reconnect from address ~a; port ~a~%" in-addr in-port)
		      (finish-output)
		      (reset (lookup-client-by-port in-port) in-addr in-port name)
		      (roger-that self)
		      (pack-header self out-message (client-id (lookup-client-by-port in-port)) :handshake (get-elapsed-time clock) 3)
		      (setf urgent-messages (append urgent-messages (list out-message)))))))
	      (progn
		(format t "new client from address ~a; port ~a~%" in-addr in-port)
		(finish-output)
		(make-instance 'client :addr in-addr :port in-port :name name)
		(roger-that self)
		(pack-header self out-message (client-id (lookup-client-by-port in-port)) :handshake (get-elapsed-time clock) 3)
		(setf urgent-messages (append urgent-messages (list out-message)))))))))

(defmethod incoming-handshake ((self server))
  (with-slots (in-addr in-port in-rtt suspicious-rtt handshake-needed out-message clock urgent-messages) self
    (let ((client (lookup-client-by-port in-port)))
      (if (>= in-rtt suspicious-rtt)
	  (progn
	    (format t "suspicious rtt")
	    (finish-output)
	    (decf (client-handshaken client)))
	  (setf (client-rtt client) (+ (client-rtt client) in-rtt)))
      (incf (client-handshaken client))
      (if (= (client-handshaken client) handshake-needed)
	  (progn
	    (setf (client-rtt client) (round (/ (client-rtt client) handshake-needed)))
	    (setf (client-connected-p client) t)
	    (format t "client connected (RTT:~a)~%" (client-rtt client))
	    (finish-output)
	    (broadcast-lobby-info self))
	  (progn
	    (pack-header self out-message (client-id client) :handshake (get-elapsed-time clock) 3)
	    (setf urgent-messages (append urgent-messages (list out-message))))))))

(defmethod broadcast-lobby-info ((self server))
  (with-slots (clock out-message urgent-messages) self
    (let ((connected-clients (list))
	  (time (get-elapsed-time clock)))
      (loop for client being the hash-value in *clients* do
	   (when (client-connected-p client)
	     (push (client-id client) connected-clients)))
      (loop for client being the hash-value in *clients* do
	   (when (client-connected-p client)
	     (pack-header self out-message (client-id client) :lobby-info time 3)
	     (userial:with-buffer (message-packet out-message)
	       (userial:serialize* :uint16 (length connected-clients))
	       (loop for client-id in connected-clients do
		    (userial:serialize* :string (client-name (lookup-client-by-id client-id))
					:int32 (client-rtt (lookup-client-by-id client-id))
					:boolean (client-ready-p (lookup-client-by-id client-id))))
	       (setf urgent-messages (append urgent-messages (list out-message)))))))))

(defmethod sync-time ((self server))
  (with-slots (clock out-message urgent-messages) self
    (let ((time (get-elapsed-time clock)))
      (loop for client-id being the hash-key in *clients* do
	   (pack-header self out-message client-id :sync-clocks time 10)
	   (setf urgent-messages (append urgent-messages (list out-message)))))
    (restart-clock clock)))

(defmethod incoming-sync ((self server))
  (with-slots (in-addr in-port everyone-synced-p last-tick-time clock) self
    (setf (synced-p (lookup-client-by-port in-port)) t)
    (when (and (all-clients-synced-p) (not everyone-synced-p))
      (format t "all clients synced~%")
      (finish-output)
      (setf everyone-synced-p t)
      (setf last-tick-time (get-elapsed-time clock))
      (setup-stage self))))

(defmethod setup-stage ((self server))
  (with-slots (scene isc) self
    (loop for client being the hash-value in *clients* do
	 (format t "adding entity fo client-id:~a~%" (client-id client))
	 (finish-output)
	 (let ((col))
	   (case (mod (client-id client) 4)
	     (0 (setf col :blue))
	     (1 (setf col :yellow))
	     (2 (setf col :green))
	     (3 (setf col :red)))
	   (setf (entity-id client) (add-entity scene (client-id client) col))
	   (setf (entity-status-entity-id (last-agreed-status client)) (entity-id client))
	   (setf (entity-status-owner-id (last-agreed-status client)) (client-id client)) ;; diferent !!!
	   (setf (gethash (client-id client) (isc-isc-count isc)) 0)))))

(defmethod incoming-ready ((self server))
  (with-slots (in-addr in-port expected-clients sync-attempted-p) self
    (setf (client-ready-p (lookup-client-by-port in-port)) t)
    (broadcast-lobby-info self)
    (when (and (all-clients-ready-p) (= (hash-table-count *clients*) expected-clients) (not sync-attempted-p))
      (format t "all clients ready, syncing clocks...~%")
      (finish-output)
      (sync-time self)
      (setf sync-attempted-p t))))

(defmethod incoming-unready ((self server))
  (with-slots (in-addr in-port) self
	(setf (client-ready-p (lookup-client-by-port in-port)) nil)
	(broadcast-lobby-info self)))

(defmethod receive-stuff ((self server))
  (with-slots (socket in-packet in-addr in-port in-pid in-uid in-seq in-ack in-msg clock in-rtt protocol-id sent waiting-for-ack) self
    (when (usocket:wait-for-input socket :timeout 0 :ready-only t) 
      (multiple-value-bind (buffer size remote-host remote-port)
	  (usocket:socket-receive socket (make-array 32768 :element-type '(unsigned-byte 8) :fill-pointer t) nil)
	
	;; (format t "received a packet~%")
	;; (finish-output)
	
	(setf in-packet buffer)
	(setf in-addr remote-host)
	(setf in-port remote-port)
	
	(userial:with-buffer in-packet
	  (userial:buffer-rewind)
	  (userial:unserialize-let* (:uint16 protocol-id :uint16 client-id :uint32 remote-sequence :uint32 ack :msg message-type)

				    (setf in-pid protocol-id)
				    (setf in-uid client-id)
				    (setf in-seq remote-sequence)
				    (setf in-ack ack)
				    (setf in-msg message-type)
				    
				    (when (= in-pid protocol-id)
				      (let ((discard-p nil))
					(unless (eq in-msg :first-contact)
					  (setf discard-p (roger-that self)))
					(unless discard-p
					  (unless (eq in-msg :first-contact)
					    (setf (client-last-ack (lookup-client-by-port in-port)) in-ack))
					  
					  ;; set inRTT if handshake
					  (when (and (gethash in-ack sent) (eq in-msg :handshake))
					    (setf in-rtt (- (get-elapsed-time clock) (message-time-sent (gethash in-ack sent)))))
					  
					  ;; remove acked message from sent and waitingForAck queues
					  (when (gethash in-ack sent)
					    (remhash (message-time-sent (gethash in-ack sent)) waiting-for-ack)
					    (remhash in-ack sent))

					  ;; dispatch, depending on messsage type
					  (ecase in-msg
					    (:event  (incoming-events self))
					    (:first-contact  (incoming-first-contact self))
					    (:handshake (incoming-handshake self))
					    (:ready (incoming-ready self))
					    (:unready (incoming-unready self))
					    (:sync-clocks (incoming-sync))))))))))))

(defmethod run ((self server))
  (with-slots (port current-time  everyone-synced-p last-tick-time tick-time last-sent-cleanup sent-timeout last-history-cleanup history-timeout history history-length last-wsa-cleanup wsa-timeout scene wsa-length pause) self
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
	(when (>= (- current-time last-sent-cleanup) sent-timeout)
	  (setf last-sent-cleanup current-time)
	  (refresh-sent self))
	
	(when (>= (- current-time last-history-cleanup) history-timeout)
	  (setf last-history-cleanup current-time)
	  (cleanup history (- current-time history-length)))
	
	(when (>= (- current-time last-wsa-cleanup) wsa-timeout)
	  (setf last-wsa-cleanup current-time)
	  (cleanup scene (- current-time wsa-length)))
	(format t "stopping server")
	(finish-output)
	(stop-server)
	(sleep pause)))))

(defmethod refresh-sent ((self server))
  ;; (with-slots (waiting-for-ack current-time) self
  ;;   (let ((end (last waiting-for-ack))))
  ;;   )
  
  )

(defmethod send-stuff ((self server))
  (with-slots (urgent-messages current-time clock out-message socket sent waiting-for-ack outbox-timer outbox-clock sent-this-second max-message-count resend-time out-going-messages) self
    
    (loop while urgent-messages do
	 ;; (format t "sending urgent packet~%")
	 ;; (finish-output)
	 (setf current-time (get-elapsed-time clock))
	 (setf out-message (pop urgent-messages))
	 (setf (message-time-sent out-message) current-time)
	 (usocket:socket-send socket
			      (message-packet out-message)
			      (length (message-packet out-message))
			      :host (message-addr out-message)
			      :port (message-port out-message)) 
	 (setf (gethash (message-sequence out-message) sent) out-message)
	 (unless (= (message-ttl out-message) 0)
	   (setf (gethash current-time waiting-for-ack) out-message)))
    
    (incf outbox-timer (restart-clock outbox-clock))
    (when (>= outbox-timer 1000)
      (setf sent-this-second 0)
      (decf outbox-timer 1000))
    
    (when (and (> (hash-table-count waiting-for-ack) 0) (< sent-this-second max-message-count))
      (setf current-time (get-elapsed-time clock))
      (let ((time-key (first (sort (alexandria:hash-table-keys waiting-for-ack) #'>)))) ;; hash-tables are unordered
	(when (>= (- current-time time-key) resend-time)
	  (let ((msg (gethash time-key waiting-for-ack)))
	    (remhash time-key waiting-for-ack)
	    (setf (message-time-sent (gethash (message-sequence msg) sent)) current-time)
	    
	    (usocket:socket-send socket
				 (message-packet msg)
				 (length (message-packet msg))
				 :host (message-addr msg)
				 :port (message-port msg))
	    (decf (message-ttl msg))
	    (unless (= (message-ttl msg) 0)
	      (setf (message-time-sent msg) current-time)
	      (setf (gethash (message-time-sent msg) waiting-for-ack) msg))
	    (incf sent-this-second)))))
    
    (when (and out-going-messages (< sent-this-second max-message-count))
      ;; (format t "sending ordinary packet~%")
      ;; (finish-output)
      (let ((msg (pop out-going-messages)))
	(setf current-time (get-elapsed-time clock))
	(setf (message-sent-time msg) current-time)
	(usocket:socket-send socket
			     (message-packet msg)
			     (length (message-packet msg))
			     :host (message-addr msg)
			     :port (message-port msg))
	(setf (gethash (message-sequence msg) sent) msg)
	
	(unless (= (message-ttl msg) 0)
	  (setf (gethash current-time waiting-for-ack) msg))
	(incf sent-this-second)))))

;; (defmethod move-entity ((self server) event)
;;   (with-slots (scene) self
;;     (move-entity scene (entity-id (lookup-client-by-id (event-owner event))) (event-input event) (event-time event))))

(defmethod broadcast-world-state ((self server) start-of-last-tick)
  (with-slots (out-message current-world-state) self
    (loop for client being the hash-value in *clients* do
	 (pack-header self out-message (client-id client) :world-state (world-state-time current-world-state) 0)
	 (userial:with-buffer (message-packet out-message)
	   (userial:serialize* :uint16 (world-state-entity-count previous-world-state))
	   (loop for entity-status in (world-state-entities previous-world-state) do
		(userial:serialize* :uint32 (entity-status-owner entity-status) 
				    :uint32 (entity-status-entity-id entity-status) 
				    :boolean (input-state-left-p (entity-status-current-input-state entity-status)) 
				    :boolean (input-state-right-p (entity-status-current-input-state entity-status)) 
				    :boolean (input-state-up-p (entity-status-current-input-state entity-status)) 
				    :boolean (input-state-attackt-p (entity-status-current-input-state entity-status)) 
				    :boolean (input-state-block-p (entity-status-current-input-state entity-status)) 
				    :uint32 (entity-status-last-updated entity-status) 
				    :float32 (aref (entity-status-pos entity-status) 0) 
				    :float32 (aref (entity-status-pos entity-status) 1) 
				    :float32 (aref (entity-status-vel entity-status) 0) 
				    :float32 (aref (entity-status-vel entity-status) 1) 
				    :int32 (entity-status-state-ptr entity-status) 
				    :boolean (entity-status-in-air-p entity-status) 
				    :boolean (entity-status-attacking-p entity-status) 
				    :boolean (entity-status-hit-already-p entity-status) 
				    :boolean (entity-status-hit-needs-release-p entity-status)))
	   (userial:serialize :uint32 (world-state-time previous-world-state))
	   
	   (userial:serialize :uint16 (world-state-entity-count current-world-state))
	   (loop for entity-status in (world-state-entities current-world-state) do
		(userial:serialize* :uint32 (entity-status-owner entity-status) 
				    :uint32 (entity-status-entity-id entity-status) 
				    :boolean (input-state-left-p (entity-status-current-input-state entity-status)) 
				    :boolean (input-state-right-p (entity-status-current-input-state entity-status)) 
				    :boolean (input-state-up-p (entity-status-current-input-state entity-status)) 
				    :boolean (input-state-attackt-p (entity-status-current-input-state entity-status)) 
				    :boolean (input-state-block-p (entity-status-current-input-state entity-status)) 
				    :uint32 (entity-status-last-updated entity-status) 
				    :float32 (aref (entity-status-pos entity-status) 0) 
				    :float32 (aref (entity-status-pos entity-status) 1) 
				    :float32 (aref (entity-status-vel entity-status) 0) 
				    :float32 (aref (entity-status-vel entity-status) 1) 
				    :int32 (entity-status-state-ptr entity-status) 
				    :boolean (entity-status-in-air-p entity-status) 
				    :boolean (entity-status-attacking-p entity-status) 
				    :boolean (entity-status-hit-already-p entity-status) 
				    :boolean (entity-status-hit-needs-release-p entity-status)))
	   (userial:serialize :uint32 (world-state-time current-world-state))
	   
	   
	   ;; input changes this tick: all clients receive the input changes of others and not their own
	   (userial:serialize :uint16 (changes-for-others isc (client-id client)))
	   
	   (loop for j from 0 to (isc-item-count isc) do
		(unless (eq (input-state-change-owner (aref (isc-item isc) j)) (client-id client))
		  (userial:serialize* :int32 (input-state-change-delta (aref (isc-item isc) j)) 
				      :uint16 (input-state-change-owner (aref (isc-item isc) j))
				      :uint16 (input-state-change-entity-id (aref (isc-item isc) j))
				      :boolean (input-state-left-p (input-state-change-input (aref (isc-item isc) j))) 
				      :boolean (input-state-right-p (input-state-change-input (aref (isc-item isc) j))) 
				      :boolean (input-state-up-p (input-state-change-input (aref (isc-item isc) j))) 
				      :boolean (input-state-attackt-p (input-state-change-input (aref (isc-item isc) j))) 
				      :boolean (input-state-block-p (input-state-change-input (aref (isc-item isc) j))))))
	   
	   ;; pack last agreed status
	   (userial:serialize* :uint16 (entity-status-owner (last-agreed-status client)) 
			       :uint16 (entity-status-entity-id (last-agreed-status client))
			       :boolean (input-state-left-p (entity-status-current-input-state (last-agreed-status client)))
			       :boolean (input-state-right-p (entity-status-current-input-state (last-agreed-status client)))
			       :boolean (input-state-up-p (entity-status-current-input-state (last-agreed-status client)))
			       :boolean (input-state-attack-p (entity-status-current-input-state (last-agreed-status client)))
			       :boolean (input-state-block-p (entity-status-current-input-state (last-agreed-status client)))
			       :int32  (entity-status-last-updated (last-agreed-status client))
			       :state (entity-status-state (last-agreed-status client))
			       :float32 (aref (entity-status-pos (last-agreed-status client)) 0) 
			       :float32 (aref (entity-status-pos (last-agreed-status client)) 1) 
			       :float32 (aref (entity-status-vel (last-agreed-status client)) 0)
			       :float32 (aref (entity-status-vel (last-agreed-status client)) 1)
			       :int32  (entity-status-state-ptr (last-agreed-status client))
			       :boolean (entity-status-in-air-p (last-agreed-status client))
			       :boolean (entity-status-attackting-p (last-agreed-status client))
			       :boolean (entity-status-blocking-p (last-agreed-status client))
			       :boolean (entity-status-hit-needs-release-p (last-agreed-status client)))
	   (userial:serialize :int32 (delta-to-first-tick client))
	   
	   ;; add hit events. For now send to everyone 
	   (userial:serialize :uint16 (length (hit-queue scene)))
	   (loop for event in (hit-queue scene) do
		
		;; Temporarily modify hitEvent in hitQueue, so that time is expressed
		;; in delta from start of previous tick. Then we need to switch it back
		;; to actual, absolute time, b/c in the next iteration that's how we'll
		;; be able to decide whether that particular event is relevant for the
		;; next client.
		(let ((tmp (event-time event)))
		  (setf (event-delta-since-lac event) (- (event-time event) (entity-statu-last-updated (last-agreed-status client))))
		  (decf (event-time event) start-of-last-tick)
		  
		  ;; pack event
		  (userial:serialize* :type (event-type event) 
				      :uint32 (event-time event)
				      :uint16 (event-owner event)
				      :uint16 (event-entity-id event))
		  
		  (case (event-type event)
		    (:move
		     (userial:serialize* :boolean  (input-state-left-p (event-input event)) 
					 :boolean (input-state-right-p (event-input event)) 
					 :boolean (input-state-up-p (event-input event)) 
					 :boolean (input-state-attack-p (event-input event)) 
					 :boolean (input-state-block-p (event-input event))))
		    (:hit
		     (userial:serialize* :boolean (event-left-p event) 
					 :boolean (event-top-p event) 
					 :uint16 (entity-status-owner (event-target-status event)) 
					 :uint16 (entity-status-entity-id (event-target-status event)) 
					 :boolean (input-state-left-p (entity-status-current-input-state (event-target-status event))) 
					 :boolean (input-state-right-p (entity-status-current-input-state (event-target-status event))) 
					 :boolean (input-state-up-p (entity-status-current-input-state (event-target-status event)))
					 :boolean (input-state-attack-p (entity-status-current-input-state (event-target-status event)))
					 :boolean (input-state-block-p (entity-status-current-input-state (event-target-status event)))
					 :int32 (entity-status-last-updated (event-target-status event))
					 :state (entity-status-state (event-target-status event)) 
					 :float32 (aref (entity-status-pos (event-target-status event)) 0) 
					 :float32 (aref (entity-status-pos (event-target-status event)) 1) 
					 :float32 (aref (entity-status-vel (event-target-status event)) 0) 
					 :float32 (aref (entity-status-vel (event-target-status event)) 1) 
					 :int32 (entity-status-state-ptr (event-target-status event)) 
					 :boolean (entity-status-in-air-p (event-target-status event)) 
					 :boolean (entity-status-attacking-p (event-target-status event)) 
					 :boolean (entity-status-hit-already-p (event-target-status event)) 
					 :boolean (entity-status-hit-needs-release-p (event-target-status event)) 
					 :int32 (event-delta-since-lac event))))
		  (setf (event-time event) tmp)))
	   
	   ;; send previous hit queue for redundancy
	   (userial:serialize :uint16 (length previous-hit-queue))
	   (loop for event in previous-hit-queue do
		
		;; tmp change time to delta
		(let ((tmp (event-time event)))
		  (setf (event-delta-since-lac event) (- (event-time event) (entity-statu-last-updated (last-agreed-status client))))
		  (decf (event-time event) (- start-of-last-tick tick-time))
		  
		  ;; pack event
		  (userial:serialize* :type (event-type event) 
				      :uint32 (event-time event)
				      :uint16 (event-owner event)
				      :uint16 (event-entity-id event))
		  
		  (case (event-type event)
		    (:move
		     (userial:serialize* :boolean  (input-state-left-p (event-input event)) 
					 :boolean (input-state-right-p (event-input event)) 
					 :boolean (input-state-up-p (event-input event)) 
					 :boolean (input-state-attack-p (event-input event)) 
					 :boolean (input-state-block-p (event-input event))))
		    (:hit
		     (userial:serialize* :boolean (event-left-p event) 
					 :boolean (event-top-p event) 
					 :uint16 (entity-status-owner (event-target-status event)) 
					 :uint16 (entity-status-entity-id (event-target-status event)) 
					 :boolean (input-state-left-p (entity-status-current-input-state (event-target-status event))) 
					 :boolean (input-state-right-p (entity-status-current-input-state (event-target-status event))) 
					 :boolean (input-state-up-p (entity-status-current-input-state (event-target-status event)))
					 :boolean (input-state-attack-p (entity-status-current-input-state (event-target-status event)))
					 :boolean (input-state-block-p (entity-status-current-input-state (event-target-status event)))
					 :int32 (entity-status-last-updated (event-target-status event))
					 :state (entity-status-state (event-target-status event)) 
					 :float32 (aref (entity-status-pos (event-target-status event)) 0) 
					 :float32 (aref (entity-status-pos (event-target-status event)) 1) 
					 :float32 (aref (entity-status-vel (event-target-status event)) 0) 
					 :float32 (aref (entity-status-vel (event-target-status event)) 1) 
					 :int32 (entity-status-state-ptr (event-target-status event)) 
					 :boolean (entity-status-in-air-p (event-target-status event)) 
					 :boolean (entity-status-attacking-p (event-target-status event)) 
					 :boolean (entity-status-hit-already-p (event-target-status event)) 
					 :boolean (entity-status-hit-needs-release-p (event-target-status event)) 
					 :int32 (event-delta-since-lac event))))
		  (setf (event-time event) tmp))))
	 (setf urgent-messages (append urgent-messages (list out-message))))
    
    (loop for event in (hit-queue scene) do
	 (add-event history event))
    
    ;; archive hit events
    (setf previous-hit-queue (hit-queue scene))
    
    ;; hit queue is cleared each tick
    
    )
  )

(defmethod tick ((self server) time)
  (with-slots (isc scene history current-world-state frame-time-in-ms tick-time) self
    
    ;; reset input changes
    (setf (isc-item-count isc) 0)
    (loop for i being the hash-value in (isc-isc-count isc) do
	 (setf i 0))

    (loop for client being the hash-value in *clients* do
	 (setf (first-delta-set client) t))
    
    ;; clear hit Q
    (setf (hit-queue scene) (make-array))
    
    ;; rewind time if needed
    (when (< (oldest-event-this-tick history) (world-state-time current-world-state))
      (rewind-world-to scene (oldest-event-this-tick history)))

    ;; get the first event this tick and the next frame time
    (let* ((update-ptr (world-state-time current-world-state))
	   (next-event (get-next-event history (- update-ptr 1)))
	   (next-frame-time (+ update-ptr frame-time-in-ms))
	   (last-frame-time 0)
	   (first-ws-passed-p nil)
	   (start-of-last-tick (- time tick-time)))
      
      ;; iterate through history until we reach the present
      
      (loop while (or (<= next-frame-time time) 
		      (and next-event 
			   (<= (event-time next-event) time))) do
	   (if (or (not next-event) (< next-frame-time (event-time next-event)))
	       (progn
		 ;; (format t "no events to deal with before reaching the next frame~%")
		 (update-world-at scene next-frame-time)
		 (setf last-frame-time next-frame-time)
		 (incf next-frame-time frame-time-in-ms))
	       (progn
		 ;; (format t "there's an event we have to deal with before the end of this frame~%")
		 (when (and (not (eq (event-type next-event) :state-refresh)) (> (event-time next-event) (last-seen-command-time (lookup-client-by-id (event-owner next-event)))))
		   (setf (last-seen-command-time (lookup-client-by-id (event-owner next-event))) (event-time next-event))
		   (setf (first-delta-set (lookup-client-by-id (event-owner next-event))) nil))
		 
		 ;; need to broadcast, so save to isc (input-state-changes)
		 (when (eq (event-type next-event) :move)
		   (if (> (event-time next-event) (start-of-last-tick))
		       (when (< (isc-item-count isc) (isc-max-items))
			 (setf (input-state-change-input (aref (isc-item isc) (isc-item-count isc))) (event-input next-event))
			 (setf (input-state-change-delta (aref (isc-item isc) (isc-item-count isc))) (- (event-time next-event) start-of-last-tick))
			 (setf (input-state-change-owner (aref (isc-item isc) (isc-item-count isc))) (event-owner next-event))
			 (setf (input-state-change-enity-id (aref (isc-item isc) (isc-item-count isc))) (event-entity-id next-event))
			 (incf (gethash (event-owner next-event) (isc-isc-count isc)))
			 (incf (isc-item-count isc)))
		       (format t "event in the past")))
		 
		 ;; event dispatch
		 (ecase (event-type next-event)
		   (:move 
		    ;; (move-entity self next-event)
		    )
		   (:hit 
		    (hit-entity scene next-event))
		   (:state-refresh
		    (if first-ws-passed-p
			(progn
			  (update-world-at scene (event-time next-event))
			  (update-archive-at scene (event-time next-event))
			  (loop for client being the hash-value in *clients* do
			       (if (not (first-delta-set client))
				   (progn)
				   (when (= (entity-status-last-updated (last-agreed-status client)) (event-time next-event))
				     (setf (last-agreed-status client) (get-status scene (client-id client)))))))
			(setf first-ws-passed-p t))))
		 
		 (setf update-ptr (event-time next-event))
		 (setf next-event (get-next-event (history *server-state*) update-ptr)))))
      
      ;; deal with leftover time when tick doesn't end exactly at tick-time
      (when (< last-frame-time time)
	(update-world-at (scene *server-state*) time))
      
      ;; calculate client's last-command deltas and last agreed status
      (loop for client being the hash-value in *clients* do
	   (unless (first-delta-set client)
	     (setf (first-delta-set client) t)
	     (setf (last-agreed-status client) (get-status scene (client-id client)))
	     (setf (delta-to-first-tick client) (- time (last-seen-command-time client)))))
      
      ;; add our own event, reset history's oldest-event-this-tick, events in next tick will be compared to this ws-event
      (add-event history (make-event :type :state-refresh :time time))
      (reset-oldest-event history)
      
      (update-prev-world-state scene)
      
      ;; save current state
      (save-current scene time)
      
      ;; broadcast event
      (broadcast-world-state self start-of-last-tick))))

(defmethod sync ((self server) time id)
  (with-slots (clock) self
    (setf time (+ time (rtt (lookup-client-by-id id))))
    (let ((time-now (get-elapsed-time clock)))
      (when (> time time-now)
	;; todo
	))))

(defmethod pack-header ((self server) msg u-id msg-type time ttl)
  (with-slots (protocol-id local-sequence) self
    (setf (message-packet msg) (userial:make-buffer))
    (userial:with-buffer (message-packet msg)
      (userial:serialize* :uint16 protocol-id
			  :uint16 u-id
			  :uint32 local-sequence
			  :uint32 (client-sequence (lookup-client-by-id u-id))
			  :uint32 (client-acks (lookup-client-by-id u-id))
			  :msg msg-type))
    (setf (message-addr msg) (client-addr (lookup-client-by-id u-id)))
    (setf (message-port msg) (client-port (lookup-client-by-id u-id)))
    (setf (message-time-sent msg) time)
    (setf (message-ttl msg) ttl)
    (setf (message-sequence msg) local-sequence)
    (incf local-sequence)))
