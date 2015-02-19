(in-package #:faitaa-client)

(defstruct message
  packet
  addr
  port ;; needed as testing will use same addr for all clients
  (sequence 0)
  (time-sent 0)
  (first-delta 0)
  (ttl 0))
