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
  (with-slots (data oldest-event-this-tick youngest-event-this-tick) self
    ;; if time slot already has an event, increase time by 1 ms and try to add again
    (if (multiple-value-bind (entry exists) (gethash (event-time event) data)
	   exists)
	(progn 
	  (incf (event-time event))
	  (add self event))
	(progn
	  (setf (gethash (event-time event) data) event)
	  
	  (when (< (event-time event) oldest-event-this-tick)
	    (setf oldest-event-this-tick (event-time event)))
	  
	  (when (> (event-time event) youngest-event-this-tick)
	    (setf youngest-event-this-tick (event-time event)))
	  
	  (format t "new event added to history:~%~a~%" event)
	  (finish-output)))))

(defmethod update ((self history) event))
(defmethod cleanup ((self history) deadline))
(defmethod get-next-event ((self history) deadline))
(defmethod reset-oldest-event ((self history)))
(defmethod already-added ((self history) event))
