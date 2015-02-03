(in-package #:faitaa-server)

(defstruct entity-status
  (current-input-state)
  (last-updated 0)
  (owner 0)
  (id 0)
  (pos (sb-cga:vec 300.0 300.0 0.0))
  (vel (sb-cga:vec 0.0 0.0 0.0))
  (in-air-p t)
  (attacking-p nil)
  (blocking-p nil)
  (hit-already-p nil)
  (attack-time 0)
  (state) ; idle, walking, jumping, falling, hittingOnGround, hittingInAir, blocking, beingHit 
  (state-ptr 0) ; we've been in this particular state for state-ptr ms ???????????????????????????????? 
  (hit-needs-release-p nil))

(defclass entity ()
  ((status)
   (attack-start-up
    :initform 80)
   (attack-active
    :initform 280)
   (attack-recovery
    :initform 400)
   (hit-stun
    :initform 400)
   (shape)
   (color)
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
    :initform (sb-cga:vec 640.0 480.0))))

;; (defmethod render ((self entity) window))
(defmethod can-i-block ((self entity)))
(defmethod can-i-attack ((self entity)))
(defmethod update-with-delta ((self entity) delta-time))
(defmethod update-at ((self entity) time))
(defmethod set-input-state-at ((self entity) time))
(defmethod set-status ((self entity) entity-status))
(defmethod hit ((self entity) left top time))
