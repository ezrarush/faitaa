(in-package #:faitaa-server)

(defstruct hit-record
  (owner)				; the one that attacked
  (entity-id)				; the target(s)
  )

(defclass scene ()
  ((entities
    :initform (make-hash-table))
   (hit-records
    :initform (make-hash-table))
   (next-available-entity-id
    :initform 1)
   (current-world-state
    :initform (make-world-state)
    :accessor current-world-state)
   (previous-world-state
    :initform (make-world-state)
    :accessor previous-world-state)
   (past-world-states)
   (hit-queue)))

;; add entity for client and returns entity-id
(defmethod add-entity ((self scene) owner color)
  (with-slots (entities next-available-entity-id current-world-state) self
    (let ((entity (make-entity owner next-available-entity-id color)))
      (setf (gethash next-available-entity-id entities) entity)
      (setf (world-state-entities current-world-state) (append (world-state-entities current-world-state) (list (status entity)))))
    (incf (world-state-entity-count current-world-state))
    (- (incf next-available-entity-id) 1)))

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
