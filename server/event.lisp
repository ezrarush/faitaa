(in-package #:faitta-server)

(defstruct event
  last-world-state
  time
  type ; nullevent, spawn, move, staterefresh, hit
  sequence
  input
  owner
  entity-id
  left
  top
  target-status
  delta-since-lac)
