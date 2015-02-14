(in-package #:faitaa-client)

(defclass client-state ()
  ((client-id 
    :initform nil
    :initarg :client-id
    :accessor client-id)
   (current-screen 
    :initform :title-screen
    :accessor current-screen)
   (client-name
    :initarg :client-name
    :accessor client-name)
   
   (local-sequence
    :initform 1)
      
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

   (server-addr)
   (server-port)
   (connected-p
    :initform nil)
   (ready-p
    :initform nil)
   (synced-p
    :initform nil)

   (clock)
   (current-time)
   
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
   
   (out-going-messages)
   (urgent-messages)
   (waiting-for-ack)
   (sent)
   (input-buffer)
   (sent-archives) ; which message was sent at what time - originally.
   (ws-on-server) ; this is where we archive incoming ws messages from the server
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
    :initform 5)
   (resend-time
    :initform 400)
   (tick-time
    :initform (/ 1000 30)
    :reader tick-time)
   (server-tick-time
    :initform 50)
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

(defvar *client-state* (make-instance 'client-state))
