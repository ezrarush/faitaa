(in-package #:faitaa-server)

(defstruct entity-status
  current-input-state
  last-updated
  owner
  entity-id
  (pos (sb-cga:vec 5.0 5.0 1.0))
  (vel (sb-cga:vec 0.0 0.0 0.0))
  (in-air-p t)
  (attacking-p nil)
  (blocking-p nil)
  (hit-already-p nil)
  (attack-time 0)
  state ; idle, walking, jumping, falling, hittingOnGround, hittingInAir, blocking, beingHit 
  (state-ptr 0) ; we've been in this particular state for state-ptr ms ???????????????????????????????? 
  (hit-needs-release-p nil))

(defclass entity ()
  ((status
    :initform (make-entity-status)
    :accessor status)
   (attack-start-up
    :initform 80)
   (attack-active
    :initform 280)
   (attack-recovery
    :initform 400)
   (hit-stun
    :initform 400)
   (shape)
   (color
    :initarg :color)
   (acceleration
    :initform 800.0)
   (air-acceleration
    :initform 400.0)
   (jump-acceleration
    :initform -500.0)
   (gravity
    :initform 500)
   (friction
    :initform 20)
   (screen-size
    :initform (sb-cga:vec 640.0 480.0 0.0))))

;; (defmethod render ((self entity) window))
(defmethod can-block-p ((self entity)))
(defmethod can-attack-p ((self entity)))
(defmethod update-with-delta ((self entity) delta-time))
(defmethod update-at ((self entity) time))
(defmethod set-input-state-at ((self entity) time))
(defmethod set-status ((self entity) entity-status))
(defmethod hit ((self entity) left top time))

(defun make-entity (owner entity-id color)
  (let ((entity (make-instance 'entity :color color))
	(status (make-entity-status :owner owner :entity-id entity-id)))
    (setf (status entity) status)
    entity))
