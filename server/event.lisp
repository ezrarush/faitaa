(in-package #:faitaa-server)

(defstruct event
  (time 0)
  last-world-state
  (type :null-event)		   ; :nullevent, :spawn, :move, :staterefresh, :hit
  (sequence 0)
  input
  owner
  entity-id
  (left nil)
  (top nil)
  target-status
  (delta-since-lac 0))
