(in-package #:faitaa-client)

(defstruct archive
  (time 0))

(defclass client ()
  ((protocol-id
    :initarg :protocol-id)
   (local-sequence
    :initform 1)
   (client-id 
    :initform 0
    :initarg :client-id
    :accessor client-id)
   (name
    :initarg :name
    :accessor name)
   
   (current-screen 
    :initform :title-screen
    :accessor current-screen)
   
   (avg-pps
    :initform 1)
   (max-pps
    :initform 0)
   (current-input-state
    :initform (make-input-state)
    :accessor input-state)
   
   (scene
    :initform (make-instance 'scene)
    :reader scene)
   
   (socket)
   (port)

   (server-addr
    :initarg :server-addr)
   (server-port
    :initarg :server-port)
   (connected-p
    :initform nil)
   (ready-p
    :initform nil)
   (synced-p
    :initform nil)

   (clock)
   (current-time
    :initform 0)
   
   (my-entity
    :initform :green)
   (sim-entity
    :initform :blue)
   
   (remote-sequence
    :initform 0)
   (last-ack)
   (ack-field)
   
   (missing-ack-p
    :initform nil)
   
   (out-message)
   
   (srv-current-world-state)
   (srv-previous-world-state)
   (last-agreed-status)
   (first-unacked-command-time
    :initform 0)
   (last-agreed-command-time
    :initform 0)
   (first-delta
    :initform 0)
   
   (new-agreed-status)
   (incoming-first-delta
    :initform 0)
   (incoming-last-delta
    :initform 0)
   
   (history
    :initform (make-instance 'history)
    :accessor history)
   (incoming-isc)
   
   (last-command-cast
    :initform 0)
   
   (out-going-messages
    :initform (list))
   (urgent-messages
    :initform (list))
   (waiting-for-ack
    :initform (make-hash-table))
   (sent
    :initform (make-hash-table))
   (input-buffer)
   (sent-archives
    :initform (make-hash-table)) ; which message was sent at what time - originally.
   (ws-on-server
    :initform (make-hash-table)) ; this is where we archive incoming ws messages from the server
   (outbox-clock)
   (outbox-timer
    :initform 0)
   (sent-this-second
    :initform 0)

   (in-packet)
   (in-addr)
   (in-port)
   (in-pid)
   (in-uid)
   (in-seq)
   (in-ack)
   (in-field)
   (in-msg)   
   
   (archive-cleanup
    :initform 0)
   (history-cleanup
    :initform 0)
   (wsa-cleanup
    :initform 0)

   (connect-timeout
    :initform 5000)
   (pause
    :initform 0.005)
   (resend-time
    :initform 400)
   (tick-time
    :initform (/ 1000 30)
    :reader tick-time)
   (server-tick-time
    :initform (/ 1000 20))
   (max-message-count
    :initform 20)
   (archive-cleanup-time
    :initform 4000)
   (history-timeout
    :initform 2000)
   (history-length
    :initform 2000)
   (wsa-timeout
    :initform 2000)
   (wsa-length
    :initform 2000)
   (archive-length
    :initform 100)
   (frame-time-in-ms
    :initform (/ 1000 60))))

(defmethod initialize-instance :after ((self client) &key)
  (with-slots (socket server-addr server-port) self
    (setf socket (usocket:socket-connect server-addr
					 server-port
					 :protocol :datagram
					 :element-type '(unsigned-byte 8)))   
    (format t "connected to server~%")
    (finish-output)))

(defmethod connect ((self client))
  (with-slots (server-addr server-port connected-p out-message clock name connect-timeout pause) self
    (let ((attempts 0))
      (loop until (or connected-p (> attempts 3)) do
	   (pack-header self out-message :first-contact (get-elapsed-time clock) 0)
	   (userial:with-buffer (message-packet out-message)
	     (userial:serialize* :string name))
	   (let ((trying 0)
		 (tmp-clock (make-instance 'clock)))
	     (loop until (or connected-p (> trying connect-timeout)) do
		  (receive-stuff self)
		  (send-stuff self)
		  (sleep pause)
		  (incf trying (clock-start tmp-clock))))
	   (incf attempts)))
    connected-p))

;; (defun disconnect-from-server ()
;;   (assert *server-connection*)
;;   (usocket:socket-close *server-connection*)
;;   (setf *server-connection* nil))

;; TODO make sure msg and client::out-message are the same object
(defmethod pack-header ((self client)  msg msg-type time ttl)
  (with-slots (protocol-id local-sequence remote-sequence server-addr) self
    (setf (message-packet msg) (userial:make-buffer))
    (userial:with-buffer (message-packet msg)
      (userial:serialize* :uint16 protocol-id
			  :uint16 u-id
			  :uint32 local-sequence
			  :uint32 remote-sequence
			  :msg msg-type))
    (setf (message-addr msg) server-addr)
    (setf (message-sequence msg) local-sequence)
    (setf (message-time-sent msg) time)
    (setf (message-ttl msg) ttl)
    (incf local-sequence)))

(defmethod receive-stuff ((self client))
  (with-slots (socket in-addr in-port in-pid in-uid in-seq in-ack in-field in-msg in-rtt protocol-id remote-sequence sent waiting-for-ack) self
    (let ((recvd-p nil))
      (when (usocket:wait-for-input socket :timeout 0 :ready-only t) 
	(multiple-value-bind (buffer size remote-host remote-port)
	    (usocket:socket-receive socket (make-array 32768 :element-type '(unsigned-byte 8) :fill-pointer t) nil)
	  
	  (setf in-packet buffer)
	  (setf in-addr remote-host)
	  (setf in-port remote-port)
	  
	  (userial:with-buffer in-packet
	    (userial:buffer-rewind)
	    (userial:unserialize-let* (:uint16 pid :uint16 uid :uint32 seq :uint32 ack :unit32 field :msg message-type)
				      
				      (setf in-pid pid)
				      (setf in-uid uid)
				      (setf in-seq seq)
				      (setf in-ack ack)
				      (setf in-field field)
				      (setf in-msg message-type)
				      
				      (when (and (eq in-pid protocol-id) (later in-seq remote-sequence))
					(setf remote-sequence in-seq)
					(roger-that self)
					(setf recvd-p t)
					(ecase in-msg
					  (:handshake (incoming-handshake self))
					  (:lobby-info (inncoming-lobby-info self))
					  (:sync-clocks (incoming-sync self))
					  (:world-state (incoming-ws self))))))))
      recvd-p)))

(defmethod roger-that ((self client))
  (with-slots (in-ack last-ack ack-field in-field sent waiting-for-ack missing-ack-p) self
    (when (or (later in-ack last-ack) (= in-ack last-ack))
      (let ((i (- in-ack last-ack)))
	(setf ack-field) (ash ack-field i)
	(setf ack-field (boole boole-xor ack-field in-field)) ;; TODO check bug in using exclusive or instead of inclusive or
	
	;; acking current
	(let ((msg (gethash in-ack sent)))
	  (when msg
	    (remhash (message-time-sent msg) waiting-for-ack)
	    (remhash in-ack sent)))
	
	;;acking past
	(loop for i from 0 to 31 do
	     (when (logbitp i ack-field)
	       (let ((msg (gethash (- in-ack (+ 1 i)) sent)))
		 (when msg
		   (remhash (message-time-sent msg) waiting-for-ack)
		   (remhash in-ack sent)))))
	
	(setf ack-field in-field)
	(setf last-ack in-ack)
	
	;; look for missing acks
	(setf missing-ack-p nil)
	(loop for i from 0 to (if (< (- last-ack 1) 32) (- last-ack 2) 31) do
	     (unless (logbitp i ack-field)
	       ;; no ack received for sequence last-ack - 1 - i
	       (format t "missing ack~%")
	       (finish-output)
	       (setf missing-ack-p t)))))
    missing-ack-p))

(defmethod ready-or-not ((self client))
  (with-slots (ready-p out-message clock out-going-messages) self
    (pack-header self out-message (if ready-p :ready :unready) (get-elapsed-time clock) 3)
    (setf out-going-messages (append out-going-messages (list out-message)))))

(defmethod incoming-lobby-info ((self client))
  (with-slots (client-id in-packet name connected-p ready-p) self
    (format t "uid:~a~%" client-id)
    (format t "incoming lobby info. connected are:~%")
    (finish-output)
    (userial:with-buffer in-packet
      (userial:unserialize-let* (:uint16 count)
				(loop repeat count do 
				     (userial:unserialize-let* (:string in-name :int32 in-ping :boolean in-ready-p)
							       (format t "~a (ping:~a)" in-name in-ping)
							       (if in-ready-p
								   (format t "ready")
								   (format t "not ready"))
							       (when (= in-name name)
								 (format t "*you*"))
							       (format t "~%")))))
    (setf connected-p t)
    
    ;;ack lobby-info packet in ready/unready status
    (unless ready-p
      (setf ready-p t)
      (ready-or-not self))))

;; handshakes are urgent to calculate rtt
(defmethod incoming-handshake ((self client))
  (with-slots (client-id in-uid scene out-message clock urgent-messages) self
    (setf client-id in-uid)
    (setf (my-own-uid scene) client-id)
    (pack-header self out-message :handshake (get-elapsed-time clock) 3)
    (setf urgent-messages (append urgent-messages (list out-message)))))

;; upon receipt of sync message: restart clock, and send ack
(defmethod incoming-sync ((self client))
  (with-slots (clock out-message urgent-messages synced-p) self
    (restart-clock clock)
    (pack-header out-message :sync-clocks (get-elapsed-time clock) 6)
    (setf urgent-messages (append urgent-messages (list out-message)))
    (setf synced-p t)))

(defmethod replay ((self client) status srv-first-delta time)
  (with-slots (scene history frame-time-in-ms) self
    (rewind-world-to scene (entity-status-last-updated status)) ; rewinds world not client
    (set-player-status scene status) ; rewinds client entity
    (update-player-without-delta scene (- srv-first-delta)) ; align client and server time
    
    ;;go back in time and reapply events
    (let* ((update-ptr (world-state-time (current-world-state scene)))
	   (next-event (get-next-event history (- update-ptr 1)))
	   (next-frame-time (+ update-ptr frame-time-in-ms))
	   (last-frame-time 0)
	   (first-ws-passed-p nil))
      
      (loop while (or (<= next-frame-time time)
		      (and next-event (<= (event-time next-event) time))) do
	   (if (or (not next-event) (< next-frame-time (event-time next-event)))
	       (progn
		 (update-all-at scene next-frame-time t)
		 (setf last-frame-time next-frame-time)
		 (incf next-frame-time frame-time-in-ms))
	       (progn
		 (if (= (event-type next-event) :state-refresh)
		     (if first-ws-passed-p
			 (handle scene next-event)
			 (setf first-ws-passed-p t))
		     (handle scene next-event))
		 (setf update-ptr (event-time next-event))
		 (setf next-event (get-next-event history update-ptr)))))
      
      (when (< last-frame-time time)
	(update-all-at scene time t)))))

(defmethod incoming-ws ((self client))
  (with-slots (clock in-packet srv-previous-world-state srv-current-world-state incoming-isc new-agreed-status incoming-first-delta scene last-agreed-command-time sent-archives client-id history ws-on-server server-tick-time ) self
    (let ((time (get-elapsed-time clock)))
      
      (userial:with-buffer in-packet
	;;unpack srv-previous-world-state
	(userial:unserialize-let* (:uint16 entity-count)
				  (let ((ws (make-world-state :entity-count entity-count)))
				    (loop repeat entity-count do
					 (userial:unserialize-let* 
					  (:uint32 owner 
						   :uint32 uid 
						   :boolean left-p 
						   :boolean right-p 
						   :boolean up-p 
						   :boolean attack-p 
						   :boolean block-p 
						   :uint32 last-updated 
						   :state state
						   :float32 pos-x 
						   :float32 pos-y
						   :float32 vel-x
						   :float32 vel-y
						   :int32 state-ptr
						   :boolean in-air-p
						   :boolean attacking-p
						   :boolean hit-already-p
						   :boolean hit-needs-release-p)
					  (push (make-entity-status :owner owner
								    :entity-id uid
								    :current-input-state (make-input-state :left-p left-p 
													   :right-p right-p 
													   :up-p up-p 
													   :attack-p attacking-p 
													   :block-p block-p)
								    :last-updated last-updated
								    :state state
								    :pos (sb-cga:vec pos-x pos-y 1.0)
								    :vel (sb-cga:vec vel-x vel-y 1.0)
								    :state-ptr state-ptr
								    :in-air-p in-air-p
								    :attacking-p attacking-p
								    :hit-already-p hit-already-p
								    :hit-needs-release-p hit-needs-release-p) 
						(world-state-entities ws))))
				    (setf (world-state-time ws) (userial:unserialize :uint32))
				    (setf srv-previous-world-state ws)))
	
	;; unpack srv-current-world-state
	(userial:unserialize-let* (:uint16 entity-count)
				  (let ((ws (make-world-state :entity-count entity-count)))
				    (loop repeat entity-count do
					 (userial:unserialize-let* 
					  (:uint32 owner 
						   :uint32 uid 
						   :boolean left-p 
						   :boolean right-p 
						   :boolean up-p 
						   :boolean attack-p 
						   :boolean block-p 
						   :uint32 last-updated 
						   :state state
						   :float32 pos-x 
						   :float32 pos-y
						   :float32 vel-x
						   :float32 vel-y
						   :int32 state-ptr
						   :boolean in-air-p
						   :boolean attacking-p
						   :boolean hit-already-p
						   :boolean hit-needs-release-p)
					  (push (make-entity-status :owner owner
								    :entity-id uid
								    :current-input-state (make-input-state :left-p left-p 
													   :right-p right-p 
													   :up-p up-p 
													   :attack-p attacking-p 
													   :block-p block-p)
								    :last-updated last-updated
								    :state state
								    :pos (sb-cga:vec pos-x pos-y 1.0)
								    :vel (sb-cga:vec vel-x vel-y 1.0)
								    :state-ptr state-ptr
								    :in-air-p in-air-p
								    :attacking-p attacking-p
								    :hit-already-p hit-already-p
								    :hit-needs-release-p hit-needs-release-p) 
						(world-state-entities ws))))
				    (setf (world-state-time ws) (userial:unserialize :uint32))
				    (setf srv-current-world-state ws)))
	;; unpack incoming isc
	(userial:unserialize-let* (:uint16 item-count)
				  (let ((isc (make-isc :item-count item-count)))
				    (loop for i from 0 to (- item-count 1) do
					 (setf (aref (isc-item isc) i) 
					       (make-input-state-change :delta (userial:unserialize :uint32)
									:owner (userial:unserialize :uint32)
									:entity-id (userial:unserialize :uint32)
									:input (make-input-state :left-p (userial:unserialize :boolean)  
												 :right-p (userial:unserialize :boolean)
												 :up-p (userial:unserialize :boolean) 
												 :attack-p (userial:unserialize :boolean)
												 :block-p (userial:unserialize :boolean)))))
				    (setf incoming-isc isc)))
	;; unpack entity status
	(userial:unserialize-let* 
	         (:uint32 owner 
		  :uint32 uid 
		  :boolean left-p 
		  :boolean right-p 
		  :boolean up-p 
		  :boolean attack-p 
		  :boolean block-p 
		  :uint32 last-updated 
		  :state state
		  :float32 pos-x 
		  :float32 pos-y
		  :float32 vel-x
		  :float32 vel-y
		  :int32 state-ptr
		  :boolean in-air-p
		  :boolean attacking-p
		  :boolean hit-already-p
		  :boolean hit-needs-release-p)
	 (setf new-agreed-status (make-entity-status :owner owner
						     :entity-id uid
						     :current-input-state (make-input-state :left-p left-p 
											    :right-p right-p 
											    :up-p up-p 
											    :attack-p attacking-p 
											    :block-p block-p)
						     :last-updated last-updated
						     :state state
						     :pos (sb-cga:vec pos-x pos-y 1.0)
						     :vel (sb-cga:vec vel-x vel-y 1.0)
						     :state-ptr state-ptr
						     :in-air-p in-air-p
						     :attacking-p attacking-p
						     :hit-already-p hit-already-p
						     :hit-needs-release-p hit-needs-release-p)))
	
	(setf incoming-first-delta (userial:unserialize :int32))
	
	(let ((new-las-p nil)
	      (hit-in-the-past-p nil))
	  
	  ;; Unfortunately we have to accept new LAS even if it's made on the basis
	  ;; of insufficient information (missing-ack might be true)
	  (unless (equalp new-agreed-status (last-agreed-client-status scene))  ;; TODO check 
	    (setf new-las-p t)
	    (setf (last-agreed-client-status scene) new-agreed-status)
	    (setf last-agreed-command-time (archive-time (gethash in-ack sent-archives))))
	  
	  ;; add every incoming hit event to history
	  (let ((incoming-hit-count (userial:unserialize :uint16)))
	    
	    ;; read incoming *fresh* hit events, and add them to the history of the future
	    (loop repeat incoming-hit-count do
		 (userial:unserialize-let* 
		  (:event-type type :int32 time :uint16 owner :uint16 entity-id)
		  (let ((event (make-event :time time 
					   :owner owner
					   :type type 
					   :entity-id entity-id)))
		    (ecase type
		      ;; (:move
		      ;;  (userial:unserialize-let* (:boolean left-p :boolean right-p :boolean up-p :boolean attack-p :boolean block-p)
		      ;; 				 (setf (input event) (make-input-state :left-p left-p :right-p right-p :up-p up-p :attack-p attack-p :block-p block-p))))
		      (:hit
		       (userial:unserialize-let* (:boolean left-p :boolean top-p :uint16 e-owner :uint16 u-id :boolean left-p :boolean right-p :boolean up-p :boolean attack-p :boolean block-p :int32 last-updated :state state :float32 pos-x :float32 pos-y :float32 vel-x :float32 vel-y :int32 state-ptr :boolean in-air-p :boolean attacking-p :boolean hit-already-p :boolean hit-needs-release-p :int32 delta-since-lac)
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
												       :state state
												       :pos (sb-cga:vec pos-x pos-y 0.0)
												       :vel (sb-cga:vec vel-x vel-y 0.0)
												       :state-ptr state-ptr
												       :in-air-p in-air-p
												       :attacking-p attacking-p
												       :hit-already-p hit-already-p
												       :hit-needs-release-p hit-needs-release-p))
						 (setf (event-delta-since-lac event) delta-since-lac))))
		    
		    ;; if the incoming hit affects others, no worries, we add them to the present, otherwise, to the "past"
		    (if (not (= (event-entity-id event) client-id))
			(progn
			  (incf (event-time event) time)
			  (add-event history event))
			(progn
			  (setf (event-time event) (+ (last-agreed-command-time scene) (event-delta-since-lac event)))
			  (setf hit-in-the-past-p t)
			  (add-event history event)))))))
	  
	  ;; Now read *archived* hit events; ones that supposedly were already filed, but
	  ;; were sent over again, just in case the previous world state was lost.
	  (let ((incoming-hit-count (userial:unserialize :uint16)))
	    (loop repeat incoming-hit-count do
		 (userial:unserialize-let* 
		  (:event-type type :int32 time :uint16 owner :uint16 entity-id)
		  (let ((event (make-event :time time 
					   :owner owner
					   :type type 
					   :entity-id entity-id)))
		    (ecase type
		      ;; (:move
		      ;;  (userial:unserialize-let* (:boolean left-p :boolean right-p :boolean up-p :boolean attack-p :boolean block-p)
		      ;; 				 (setf (input event) (make-input-state :left-p left-p :right-p right-p :up-p up-p :attack-p attack-p :block-p block-p))))
		      (:hit
		       (userial:unserialize-let* (:boolean left-p :boolean top-p :uint16 e-owner :uint16 u-id :boolean left-p :boolean right-p :boolean up-p :boolean attack-p :boolean block-p :int32 last-updated :state state :float32 pos-x :float32 pos-y :float32 vel-x :float32 vel-y :int32 state-ptr :boolean in-air-p :boolean attacking-p :boolean hit-already-p :boolean hit-needs-release-p :int32 delta-since-lac)
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
												       :state state
												       :pos (sb-cga:vec pos-x pos-y 0.0)
												       :vel (sb-cga:vec vel-x vel-y 0.0)
												       :state-ptr state-ptr
												       :in-air-p in-air-p
												       :attacking-p attacking-p
												       :hit-already-p hit-already-p
												       :hit-needs-release-p hit-needs-release-p))
						 (setf (event-delta-since-lac event) delta-since-lac))))
		    
		    ;; if we find a hit event that'- already added, that means we already added these all, so no point in going on. otherwise - add them all
		    (if (not (= (event-entity-id event) client-id))
			(progn
			  (incf (event-time event) time)
			  (if (already-added history event)
			      (return) ;; from loop
			      (add-event history event)))
			(progn
			  (setf (event-time event) (+ (last-agreed-command-time scene) (event-delta-since-lac event)))
			  (if (already-added history event)
			      (return) ;; from loop
			      (progn
				(setf hit-in-the-past-p t)
				(add-event history event)))))))))
	  ;; if we have a new LAS or a hit in the past, rewind
	  (when (or new-las-p hit-in-the-past-p)
	    (let ((tmp (last-agreed-client-status scene))) ;; TODO - make sure copy not shared memory
	      (setf (entity-status-last-updated tmp) last-agreed-command-time)
	      (replay self tmp incoming-first-delta time)))
	  
	  ;; finally consider forthcoming events of others
	  (let ((event (make-event :type :state-refresh :time time)))
	    (add-event history event)
	    
	    ;; when saving world states, we need to sync their times to match that on the client!
	    (update-time srv-previous-world-state (event-time event))
	    (setf (gethash (event-time event) ws-on-server) srv-previous-world-state)
	    
	    ;; let the scene know where we're at
	    (set-world-state scene srv-previous-world-state)
	    
	    (incf (event-time event) server-tick-time)
	    
	    (update-time srv-current-world-state (event-time event))
	    (setf (gethash (event-time event) ws-on-server) srv-current-world-state)
	    

	    )
	  ;; then add incoming input changes as events to future history
	  (loop for i from 0 to (- (isc-item-count incoming-isc)) do
	       (let ((move-event (make-event :type :move 
					     :input (input-state-change-input (aref (isc-item incoming-isc) i))
					     :time (+ time (input-state-change-delta (aref (isc-item incoming-isc) i)))
					     :owner (input-state-change-owner (aref (isc-item incoming-isc) i))
					     :entity-id (input-state-change-entity-id (aref (isc-item incoming-isc) i)))))
		 (add-event history move-event))))))))

(defmethod run ((self client))
  (with-slots (name client-id current-screen current-time scene history current-input-state out-message first-delta urgent-messages sent-archives local-sequence clock archive-cleanup archive-cleanup-time archive-length history-cleanup history-timeout history-length wsa-cleanup wsa-timeout ws-on-server  avg-pps max-pps) self
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
	  (unwind-protect
	       (let ((time-tmp 0)
		     (loop-clock (make-instance 'clock))
		     (prev-state (make-input-state))
		     (updated-already-p nil)
		     (next-event ;; (get-next-event history current-time)
		       )
		     (past-frame-tick 0))
		 (sdl2:with-event-loop (:method :poll)
		   
		   (:keydown
		    (:keysym keysym)
		    (let ((scancode (sdl2:scancode-value keysym)))
		      (ecase current-screen
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
		      (ecase current-screen
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
		    (ecase current-screen
		      (:title-screen
		       (format t "connecting to server~%")
		       (finish-output)
		       (send-message (make-first-contact-message name)))
		      (:game-play)
		      (:end-score)))
		   
		   (:idle
		    ()
		    (setf time-tmp (restart-clock loop-clock))
		    (setf current-time (get-elapsed-time clock))
		    
		    ;; see if there's a "future" event 
		    (setf next-event (get-next-event history past-frame-tick))

		    (loop while (and next-event (>= current-time (event-time))) do
			 (case (event-type next-event)
			   (:state-refresh
			    (set-world-state scene (gethash (event-time next-event) ws-on-server)))
			   (:move
			    (handle scene next-event))
			   (:hit
			    (handle scene next-event)))
			 (setf next-event (get-next-event history (event-time next-event))))
		    
		    (setf past-frame-tick current-time)
		
		    ;; input check
		    (unless (equalp current-input-state prev-state)
		      (let ((irrel-event nil))

			;; attack released and no other changes
			(when (and (not (input-state-attack-p current-input-state))
				   (input-state-attack-p prev-state)
				   (= (input-state-left-p current-input-state) (input-state-left-p prev-state))
				   (= (input-state-right-p current-input-state) (input-state-right-p prev-state))
				   (= (input-state-block-p current-input-state) (input-state-block-p prev-state))
				   (= (input-state-up-p current-input-state) (input-state-up-p prev-state)))
			  (setf irrel-event t))
			
			;; jump released and no other changes
			(when (and (not (input-state-up-p current-input-state))
				   (input-state-up-p prev-state)
				   (= (input-state-left-p current-input-state) (input-state-left-p prev-state))
				   (= (input-state-right-p current-input-state) (input-state-right-p prev-state))
				   (= (input-state-block-p current-input-state) (input-state-block-p prev-state))
				   (= (input-state-attack-p current-input-state) (input-state-attack-p prev-state)))
			  (setf irrel-event t))
			
			(when (and (input-state-left-p current-input-state) (input-state-right-p current-input-state))
			  (if (input-state-left-p prev-state)
			      (setf (input-state-left-p current-input-state) nil)
			      (setf (input-state-right-p current-input-state) nil)))
			
			(let ((event (make-event :type :move 
						 :input current-input-state 
						 :time (get-elapsed-time clock)
						 :entity-id client-id
						 :owner client-id)))

			  (setf prev-state (copy-structure current-input-state))
			  
			  (add-event history event)
			  ;; set player state 			   
			  (set-client-input-state-at scene (event-input event) (event-time event))

			  ;; update everything at this very moment
			  (update-all scene (event-time event))
			  (setf update-already t)
			  
			  ;;save world snapshot
			  (archive-current-world-state scene (event-time event))
			  
			  ;;send event immediately
			  (unless irrel-event
			    (pack-header self out-message :event (event-time event) 3)
			    (userial:with-buffer (message-packet out-message)
			      (userial:serialize :uint16 1) ;; number of events
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
						     :int32 (event-delta-since-lac event)))))
			    
			    (setf (message-first-delta out-message) 0)
			    (setf urgent-messages (append urgent-messages (list out-message)))
			    
			    ;; save event
			    (setf (gethash (- local-sequence 1) sent-archives) (make-archive :time (event-time event)))))))
		    
		    (send-stuff self)
		    (receive-stuff self)
		    
		    (if (not update-already-p)
			(update-all-at scene (get-elapsed-time clock))
			(setf update-already-p nil))
		    
		    (render-scene)
		    (sdl2:gl-swap-window win)
		    
		    ;; cleanup
		    (incf archive-cleanup time-tmp)

		    (when (and (>= archive-cleanup archive-cleanup-time) (> local-sequence archive-length))
		      (decf archive-cleanup archive-cleanup-time)
		      ;; todo
		      )
		    
		    (incf history-cleanup time-tmp)
		    (when (>= history-cleanup history-timeout)
		      (decf history-cleanup history-timeout)
		      (cleanup history (- (get-elapsed-time clock) history-length)))
		    
		    (incf wsa-cleanup time-tmp)
		    (when (>= wsa-cleanup wsa-timeout)
		      (decf wsa-cleanup wsa-timeout)
		      ;; todo
		      (archive-cleanup scene)))
		   
		   (:quit () t)))
	    (format t "disconnecting from server~%")
	    (finish-output)
	    (disconnect-from-server)
	    (format t "avg packets per sec:~a~%" avg-pps)
	    (format t "max pps:~a~%" max-pps)
	    (finish-output)))))))

;; (defmethod setup ((self client)))

(defmethod send-stuff ((self client))
  (with-slots (urgent-messages current-time clock out-message sock server-addr server-port sent-this-second max-message-count sent waiting-for-ack outbox-timer outbox-clock avg-pps max-pps out-going-messages max-message-count) self
    (loop while urgent-messages do
	 (setf current-time (get-elapsed-time clock))
	 (setf out-message (pop urgent-messages))
	 (setf (time-sent out-message) current-time)
	 
	 (usocket:socket-send socket
			      (message-packet out-message)
			      (length (packet out-message))
			      :host (addr out-message)
			      :port (port out-message)) 
	 
	 ;; send it twice for reliability
	 (let ((resend nil))
	   (when (< sent-this-second max-message-count)
	     (usocket:socket-send socket
				  (message-packet out-message)
				  (length (packet out-message))
				  :host (addr out-message)
				  :port (port out-message))   
	     (setf resend t))
	   
	   (setf (gethash (message-sequence out-message) sent) out-message)
	   (unless (= (message-ttl out-message) 0)
	     (setf (gethash current-time waiting-for-ack) out-message))
	   (setf sent-this-second (+ sent-this-second 1 (if resent 1 0)))))
    
    (incf outbox-timer (restart-clock outbox-clock))
    (when (>= outbox-timer 1000)
      (when (> sent-this-second 0)
	(incf avg-pps sent-this-second)
	(setf avg-pps (/ avg-pps 2))
	(when (< max-pps sent-this-second)
	  (setf max-pps sent-this-second)))
      (setf sent-this-second 0)
      (decf outbox-timer 1000))

    (when (and (> (hash-table-count waiting-for-ack) 0) (< sent-this-second max-message-count))
      (setf current-time (get-elapsed-time clock))
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
