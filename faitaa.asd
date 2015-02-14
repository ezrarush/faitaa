;;;; faitaa.asd

(asdf:defsystem #:faitaa
  :description "A modern OpenGL networked fighter game"
  :author "Ezra Rush <rushwest@gmail.com>"
  :license "The MIT License (MIT) Copyright (c) 2015 Ezra Rush"
  :depends-on (#:usocket
	       #:userial
	       #:sdl2
	       #:network-engine
	       #:graphics-engine)
  :serial t
  :components ((:module client
			:components ((:file "package")
				     (:file "helpers")
				     (:file "serial")
				     (:file "message")
				     (:file "input-state")
				     (:file "event")
				     (:file "history")
				     (:file "entity")
				     (:file "world-state")
				     (:file "scene")
				     (:file "client-state")
				     (:file "graphics")
				     (:file "client")))
	       (:module server
			:components ((:file "package")
				     (:file "helpers")
				     (:file "client")
				     (:file "serial")
				     (:file "message")
				     (:file "input-state")
				     (:file "event")
				     (:file "history")
				     (:file "entity")
				     (:file "world-state")
				     (:file "scene")
				     (:file "server")
				     (:file "main")))))

