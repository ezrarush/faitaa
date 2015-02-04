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
    ;;TODO find a suitable time spot - if already taken just increase time 1 ms
    (assert (not (multiple-value-bind (entry exists) (gethash (time event) data)
		   exists)))
    (setf (gethash (time event) data) event))
  (format t "new event added to history:~%~a~%" event)
  (finish-output))

(defmethod update ((self history) event))
(defmethod cleanup ((self history) deadline))
(defmethod get-next-event ((self history) deadline))
(defmethod reset-oldest-event ((self history)))
(defmethod already-added ((self history) event))
