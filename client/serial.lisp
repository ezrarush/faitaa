(in-package #:faitaa-client)

(userial:make-enum-serializer :msg
                      (:first-contact :handshake :ready :unready :lobby-info :disconnect :sync-clocks :spawn :event :world-state))

(userial:make-enum-serializer :event-type
                      (:move :hit :state-refresh))

(userial:make-enum-serializer :state
                      (:idle :walking :jumping :falling :hitting-on-ground :hitting-in-air :blocking :being-hit))
