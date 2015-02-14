(in-package #:faitaa-server)

(defun main (&key (port 2448))
  (let ((server (make-instance 'server :protocol-id 999 :port port)))
    (run server)))
