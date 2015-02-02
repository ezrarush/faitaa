(in-package #:faitaa-server)

(defclass history ()
  ((oldest-event-this-tick)
   (youngest-event-this-tick)
   (data)
   (null-event)))

(defmethod reset-oldest-event ((self history)))
(defmethod update ((self history) time event))
(defmethod add ((self history) event))
(defmethod get-next-event ((self history) deadline))
(defmethod cleanup ((self history) deadline))


