(in-package #:faitaa-client)

(userial:make-enum-serializer :client-opcode
                      (:login :input :logout))

(userial:make-enum-serializer :server-opcode
                      (:welcome :world-state))

(userial:make-accessor-serializer (:game-state-from-server begin-state (make-game-state))
  :int32 player-id )
