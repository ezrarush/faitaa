(in-package #:faitaa-client)

(defclass scene ()
  ((my-own-id)
   (last-agreed-player-status)
   (current-world-state
    :initform (make-world-state)
    :accessor current-world-state)
   (last-agreed-command-time
    :initform 0)
   (entities)
   (player-last-update)
   (past-world-states)
   (ws-archive-length
    :initform 2000 ;; in MS
    )))

(defmethod set-world-state ((self scene) world-state))
(defmethod partial-rewind-to ((self scene) time))
(defmethod save-player-state ((self scene)))
(defmethod update-player-now ((self scene)))
(defmethod update-player-at ((self scene) time))
(defmethod update-all-with-delta ((self scene) time))
(defmethod update-all-at ((self scene) time replay-p))
(defmethod update-player-without-delta ((self scene) time))
(defmethod set-player-input-state-at ((self scene) input-state time))
(defmethod get-current-world-state ((self scene) time))
(defmethod archive-current-world-state ((self scene) time))
(defmethod rewind-world-to ((self scene) time))
(defmethod set-player-status ((self scene) entity-status))
(defmethod handle ((self scene) event))
;; (defmethod render ((self scene) render-window))
(defmethod archive-clean-up ((self scene)))
(defmethod exists ((self scene) int))
