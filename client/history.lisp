(in-package #:faitaa-client)

(defclass history ()
  ((oldest-event-this-tick
    :initform 0)
   (youngest-event-this-tick
    :initform 0)
   (data
    :initform (make-hash-table))
   (null-event)))

(defmethod add ((self history) event)
  (with-slots (data) self
    ;; if time slot already has an event, increase time by 1 ms and try to add again
    (if (multiple-value-bind (entry exists) (gethash (event-time event) data)
	   exists)
	(progn 
	  (incf (event-time event))
	  (add self event))
	(progn
	  (setf (gethash (event-time event) data) event)
	  (format t "new event added to history:~%~a~%" event)
	  (finish-output)))))

(defmethod update ((self history) event))
(defmethod cleanup ((self history) deadline))
(defmethod get-next-event ((self history) deadline))
(defmethod reset-oldest-event ((self history)))
(defmethod already-added ((self history) event))
