(in-package #:faitaa-client)

(userial:make-enum-serializer :client-opcode
                      (:first-contact :event :disconnect))

(userial:make-enum-serializer :server-opcode
                      (:handshake :world-state))

(userial:make-enum-serializer :event-type
                      (:move :hit :state-refresh))
