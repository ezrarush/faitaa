(in-package #:faitaa-client)

(defclass scene ()
  ((my-own-uid
    :initform 0
    :accessor my-own-uid)
   (last-agreed-client-status
    :accessor last-agreed-client-status)
   (current-world-state
    :initform (make-world-state)
    :accessor current-world-state)
   (last-agreed-command-time
    :initform 0)
   (entities
    :initform (make-hash-table)
    :accessor entities)
   (player-last-update)
   (past-world-states)
   (ws-archive-length
    :initform 2000 ;; in MS
    )))


(defmethod set-world-state ((self scene) world-state)
					; If an entit exists, we set its status.
					; If it doesn't exist, we create the entity, and thereby set its status.
  (with-slots (entities) self
    (loop for entity-status in (world-state-entities world-state) do
	 (if (multiple-value-bind (object exists) (gethash (entity-status-entity-id entity-status) entities)
	       exists)
	     (setf (status (gethash (entity-status-entity-id entity-status) entities)) entity-status)
	     ;; (unless (eq (entity-status-entity-id entity) (client-id *client-state*))
	     ;;   ;; set the status
	     ;;   )
	     (progn
	       (format t "creating entity with id: ~a~%" (entity-status-entity-id entity-status))
	       
	       (setf (gethash (entity-status-entity-id entity-status) entities) 
		     
		     (make-entity (entity-status-owner entity-status) 
				  (entity-status-entity-id entity-status) 
				  "red"))
	       ;; (when (eq (id entity-status) (client-id *client-state*))
	       ;; 	 (setf last-agreed-client-status (status entity-status)))
	       )))))

(defmethod partial-rewind-to ((self scene) time))
(defmethod save-player-state ((self scene)))
(defmethod update-player-now ((self scene)))
(defmethod update-player-at ((self scene) time))
(defmethod update-all-with-delta ((self scene) time))
(defmethod update-all-at ((self scene) time replay-p))
(defmethod update-player-without-delta ((self scene) time))

(defmethod set-client-input-state-at ((self scene) input-state time)
  (with-slots (entities) self
    (set-input-state-at (gethash (client-id *client-state*) entities) input-state time)))

(defmethod get-current-world-state ((self scene) time))
(defmethod archive-current-world-state ((self scene) time))
(defmethod rewind-world-to ((self scene) time))
(defmethod set-player-status ((self scene) entity-status))
(defmethod handle ((self scene) event))
(defmethod archive-clean-up ((self scene)))
