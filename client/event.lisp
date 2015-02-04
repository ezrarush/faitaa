(in-package #:faitaa-client)

(defstruct event
  (time 0)
  (type :null-event)	     ; :null-event, :spawn, :move, :state-refresh, :hit
  (sequence 0)
  input
  owner ; for hit, the one that hits
  entity-id ; for hit, the one that's being hit
  (left nil) ; hit from left
  (top nil) ; hit from top
  (target-status) ; fot hit events, this is where the target was at the instance of the hit
  (delta-since-lac 0) ; time passed after last agreed status
  )
