(in-package #:faitaa-client)

(userial:make-enum-serializer :client-opcode
                      (:first-contact :event :disconnect))

(userial:make-enum-serializer :server-opcode
                      (:handshake :world-state))

(userial:make-accessor-serializer (:game-state-from-server begin-state (make-game-state))
  :int32 player-id )
