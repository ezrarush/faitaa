(in-package #:faitaa-server)

(defstruct input-state
  (left-p nil)
  (right-p nil)
  (up-p nil)
  (attack-p nil)
  (block-p nil))

(defstruct input-state-change
  delta			    ; time difference to start of tick
  owner 
  entity-id
  input)

(defstruct isc
  (item (make-array 20))
  (item-count 0)
  (max-items 20)
  (isc-count (make-hash-table)))

(defun changes-for-others (isc id)
  (let ((n 0))
    (loop for i being the hash-key in (isc-isc-count isc) do
	 (unless (= i id)
	   (incf n (gethash i (isc-isc-count isc)))))
    n)) 
