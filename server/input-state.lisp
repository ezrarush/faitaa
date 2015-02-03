(in-package #:faitta-server)

(defstruct input-state
  (left-p nil)
  (right-p nil)
  (up-p nil)
  (attack-p nil)
  (block-p nil))

(defstruct input-state-change
  (delta)			    ; time difference to start of tick
  (owner) 
  (entity-id)
  (input))

(defstruct isc
  (input-state-changes)
  (item-count 0)
  (max-items 20)
  (isc-count))

(defun changes-for-others (id)
  
  ) 
