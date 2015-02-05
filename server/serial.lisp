(in-package #:faitaa-server)

(userial:make-enum-serializer :client-opcode
                      (:login :input :logout))

(userial:make-enum-serializer :server-opcode
                      (:welcome :world-state))
