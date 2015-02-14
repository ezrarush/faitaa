(in-package #:faitaa-server)

(defun later-sequence ( a b)
  (let ((max 1073741800))
    (or (and (> a b) (< (- a b) max)) (and (< a b) (> (- b a) max)))))

;; make a clock
(defclass clock ()
  ((start-time
    :initform (sdl2:get-ticks))))

(defmethod get-elapsed-time ((self clock))
  (with-slots (start-time) self
    (- (sdl2:get-ticks) start-time)))

(defmethod restart-clock ((self clock))
  (with-slots (start-time) self
    (let ((elapsed-time (- (sdl2:get-ticks) start-time)))
      (setf start-time (sdl2:get-ticks))
      elapsed-time)))
