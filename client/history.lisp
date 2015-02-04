(in-package #:faitaa-client)

(defclass history ()
  ((oldest-event-this-tick
    :initform 0)
   (youngest-event-this-tick
    :initform 0)
   (data)
   (null-event)))

(defmethod add ((self history) event))
(defmethod update ((self history) event))
(defmethod cleanup ((self history) deadline))
(defmethod get-next-event ((self history) deadline))
(defmethod reset-oldest-event ((self history)))
(defmethod already-added ((self history) event))
