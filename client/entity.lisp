(in-package #:faitaa-client)

(defstruct anim
  tex-rect
  center
  (phase-count 0))

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
  (hit-already-p nil) ;; did we hit anyone with this particular attk? if so, we must not hit again
  (attack-time 0)
  (state) ; idle, walking, jumping, falling, hittingOnGround, hittingInAir, blocking, beingHit 
  (state-ptr 0) ; we've been in this particular state for state-ptr ms ???????????????????????????????? 
  (hit-needs-release-p nil) ; new hit event can only register if this is false
  )

(defclass entity ()
  ((status)
   (owner)
   (id)
   (attack-start-up
    :initform 80)
   (attack-active
    :initform 280)
   (attack-recovery
    :initform 400)
   (hit-stun
    :initform 400)
   (anim-fps
    :initform 100 ; it's more like: milliseconds-per-frame
    )
   (texture)
   (sprite)
   (anim)
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
    :initform (sb-cga:vec 640.0 480.0))
   (facing-right-p
    :initform t)
   (prev-facing-right
    :initform t)))
