(in-package #:faitaa-server)

(userial:make-enum-serializer :client-opcode
                      (:first-contact :event :disconnect))

(userial:make-enum-serializer :server-opcode
                      (:handshake :world-state))
