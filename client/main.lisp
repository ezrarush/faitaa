(in-package #:faitaa-client)

(defun main (&key (server-ip "127.0.0.1") (server-port 2448) (name "anonymous"))
  (let ((client (make-instance 'client :protocol-id 999 :server-addr server-addr :server-port server-port :name name)))
    (setup client)
    (if (connect client)
	(progn
	  (format t "connected, starting game loop~%")
	  (finish-output)
	  (run client))
	(progn
	  (format "server refused connection:timeout")
	  (finish-output)))))
