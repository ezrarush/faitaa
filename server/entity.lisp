(in-package #:faitaa-server)

(defstruct entity-status
  current-input-state
  last-updated
  owner
  id
  pos
  vel
  in-air
  is-attacking
  is-blocking
  hit-already
  attack-time
  state ; idle, walking, jumping, falling, hittingOnGround, hittingInAir, blocking, beingHit 
  hit-needs-release)

(defclass entity ()
  ((status)
   (attack-start-up)
   (attack-active)
   (attack-recovery)
   (hit-stun)
   (shape)
   (color)
   (acceleration)
   (air-acceleration)
   (jump-acceleration)
   (gravity)
   (friction)
   (screen-size)))

;; (defmethod render ((self entity) window))
(defmethod can-i-block ((self entity)))
(defmethod can-i-attack ((self entity)))
(defmethod update-with-delta ((self entity) delta-time))
(defmethod update-at ((self entity) time))
(defmethod set-input-state-at ((self entity) time))
(defmethod set-status ((self entity) entity-status))
(defmethod hit ((self entity) left top time))
