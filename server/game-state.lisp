(in-package #:faitaa-server)

(defclass game-state ()
  ((pause
    :initform 5)
   (scene
    :initform (make-instance 'scene)
    :accessor scene)
   (everyone-synced-p)
   (sync-attempted-p)
   (clock)
   (current-time)
   (last-tick-time)
   (history
    :initform (make-instance 'history)
    :accessor history)
   (isc
    :initform (make-isc)
    :accessor isc
    ) ; for keeping track of input changes in the latest tick
   (current-world-state
    :accessor current-world-state)
   (previous-world-state
    :accessor previous-world-state)
   (previous-hit-queue)
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
   (outbox-clock)
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
    :initform 50
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
    :initform (/ 1000 60))))

(defvar *game-state* (make-instance 'game-state))
