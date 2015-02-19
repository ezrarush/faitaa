(ql:quickload "faitaa")
( ql:quickload "swank")
(bt:make-thread (lambda () (swank:create-server :port 4006 :dont-close t)))
(faitaa-client:main :name "Ranma Saotome")
