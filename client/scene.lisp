(in-package #:faitaa-client)

(defclass scene ()
  ((last-agreed-client-status)
   (current-world-state
    :initform (make-world-state)
    :accessor current-world-state)
   (last-agreed-command-time
    :initform 0)
   (entities
    :initform (make-hash-table))
   (player-last-update)
   (past-world-states)
   (ws-archive-length
    :initform 2000 ;; in MS
    )))


(defmethod set-world-state ((self scene) world-state)
					; If an entity not owned by us exists, we set its status and that's that.
					; If it doesn't exist, whether or not it's ours, we create the entity, and thereby set its status.
					; But, importantly, we don't touch, at all, our own entity, if it already exists.
  (with-slots (entities) self
    (loop for entity in (world-state-entities world-state) do
	 (if (multiple-value-bind (object exists) (gethash (id entity) entities)
	       exists)
	     (unless (eq (id entity) (client-id *game-state*))
	       ;; set the status
	       )
	     (progn
	       (format t "creating entity with id: ~a~%" (id entity))
	       (setf (gethash (id entity) entities) entity)
	       (when (eq (id entity) (client-id *game-state*))
		 (setf last-agreed-client-status (status entity))))))))

(defmethod partial-rewind-to ((self scene) time))
(defmethod save-player-state ((self scene)))
(defmethod update-player-now ((self scene)))
(defmethod update-player-at ((self scene) time))
(defmethod update-all-with-delta ((self scene) time))
(defmethod update-all-at ((self scene) time replay-p))
(defmethod update-player-without-delta ((self scene) time))

(defmethod set-client-input-state-at ((self scene) input-state time)
  (with-slots (entities) self
    (set-input-state-at (gethash (client-id *game-state*) entities) input-state time)))

(defmethod get-current-world-state ((self scene) time))
(defmethod archive-current-world-state ((self scene) time))
(defmethod rewind-world-to ((self scene) time))
(defmethod set-player-status ((self scene) entity-status))
(defmethod handle ((self scene) event))
;; (defmethod render ((self scene) render-window))
(defmethod archive-clean-up ((self scene)))
