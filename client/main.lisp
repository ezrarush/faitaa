(in-package #:faitaa-client)

(defvar *client*)

(defun main (&key (server-addr "127.0.0.1") (server-port 2448) (name "anonymous"))
  (setf *client* (make-instance 'client :protocol-id 999 :server-addr server-addr :server-port server-port :name name))
  ;; (setup client)
  (if (connect *client*)
      (progn
	(format t "connected, starting game loop~%")
	(finish-output)
	(run *client*))
      (progn
	(format t "server refused connection:timeout~%")
	(finish-output))))
