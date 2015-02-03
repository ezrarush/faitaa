(in-package #:faitaa-server)

(defstruct hit-record
  (owner)				; the one that attacked
  (entity-id)				; the target(s)
  )

(defclass scene ()
  ((entities)
   (hit-records)
   (next-available-id)
   (current-world-state)
   (previous-world-state)
   (past-world-states)
   (hit-queue)))

;; add entity for client and returns entity id
(defmethod add ((self scene) owner color)
  
  
  )

(defmethod update-world-at ((self scene) time))
(defmethod rewind-world-to ((self scene) time))
(defmethod update-archive-at ((self scene) time))
(defmethod get-status ((self scene) id))
(defmethod update-previous-world-state ((self scene)))
(defmethod save-current ((self scene) time))
;; (defmethod render ((self scene) render-window))
(defmethod clean-up ((self scene) time))
(defmethod move-entity ((self scene) id input-state time))
(defmethod hit-entity ((self scene) hit-event))
