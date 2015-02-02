(in-package #:faitta-server)

(defstruct input-state
  left
  right
  up
  attack
  block)

(defstruct input-state-change
  delta ; time difference to start of tick
  owner 
  entity-id
  input)

(defstruct isc
  input-state-changes
  item-count
  max-items
  isc-count)

(defun changes-for-others (id)
  
  ) 
