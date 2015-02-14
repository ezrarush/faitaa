(in-package #:faitaa-server)

(defstruct message
  packet
  addr
  port ;; needed as testing will use same addr for all clients
  sequence
  time-sent
  ttl)

