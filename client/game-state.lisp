(in-package #:faitaa-client)

(defclass game-state ()
  ((player-id 
    :initarg :player-id
    :accessor player-id)
   (current-screen 
    :initform :title-screen
    :accessor current-screen)))

(defvar *game-state* (make-instance 'game-state))
