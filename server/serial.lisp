(in-package #:faitaa-server)

(userial:make-enum-serializer :msg
                      (:first-contact :handshake :ready :unready :lobby-info :disconnect :sync-clocks :spawn :event :world-state))

(userial:make-enum-serializer :event-type
                      (:move :hit :state-refresh))

(userial:make-enum-serializer :state
                      (:idle :walking :jumping :falling :hitting-on-ground :hitting-in-air :blocking :being-hit))

(defun unpack-input-state (buffer)
  
  )

(defun unpack-entity-status (buffer)
  
  )

(defun unpack-event (buffer)
  
  )

(defun pack-input-state (buffer is)
  
  )

(defun pack-entity-status (buffer s)
  
  )

(defun pack-world-state (buffer ws)
  
  )

(defun pack-input-state-change (buffer isc)
  
  )

(defun pack-event (buffer e)
  
  )
