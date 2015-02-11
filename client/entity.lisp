(in-package #:faitaa-client)

(defstruct anim
  tex-rect
  center
  (phase-count 0))

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
  (hit-already-p nil) ;; did we hit anyone with this particular attk? if so, we must not hit again
  (attack-time 0)
  state ; idle, walking, jumping, falling, hittingOnGround, hittingInAir, blocking, beingHit 
  (state-ptr 0) ; we've been in this particular state for state-ptr ms ???????????????????????????????? 
  (hit-needs-release-p nil) ; new hit event can only register if this is false
  )

(defclass entity ()
  ((status
    :initform (make-entity-status)
    :accessor status)
   (owner)
   (id
    :accessor id)
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
    :initform (sb-cga:vec 640.0 480.0 0.0))
   (facing-right-p
    :initform t)
   (prev-facing-right
    :initform t)))

;; (defmethod render ((self entity) window))

(defmethod temp-update ((self entity)) ;; simple movement for testing. remove this asap
  (with-slots (status) self
    (when (input-state-right-p (entity-status-current-input-state status))
      (setf (entity-status-pos status) (sb-cga:vec+ (entity-status-pos status) 
						    (sb-cga:vec 1.0 
								0.0
								0.0))))))

(defmethod update-with-delta ((self entity) delta-time)
  (with-slots (status) self
    (let ((delta (/ delta-time 1000.0)))
      ; first apply last velocity
      (setf (entity-status-pos status) (sb-cga:vec+ (entity-status-pos status) 
      						    (sb-cga:vec (* (aref (entity-status-vel status) 0) delta) 
      								(* (aref (entity-status-vel status) 1) delta)
      								0.0)))
      ;calculate new velocity on the basis of where we are and what the inputstate is 
      )

    (setf (entity-status-last-updated status) (+ (entity-status-last-updated status) delta-time))))

(defmethod update-at ((self entity) time))
(defmethod update-without-delta ((self entity) delta-time))

(defmethod set-input-state-at ((self entity) input-state time)
  (with-slots (status) self
    ;; (update-with-delta self (- time (entity-status-last-updated status)))
					; update with current state, adjust counter
    (setf (entity-status-current-input-state status) input-state)
    (temp-update self) ; for testing only, remove asap
    ))

(defmethod set-status ((self entity) entity-status))
(defmethod hit ((self entity) left-p top-p time))
(defmethod set-up-animations ((self entity)))
(defmethod can-block-p ((self entity)))
(defmethod can-attack-p ((self entity)))

(defun make-entity (owner entity-id color)
  (let ((entity (make-instance 'entity :color color))
	(entity-status (make-entity-status :owner owner :entity-id entity-id)))
    (setf (status entity) entity-status)
    entity))
