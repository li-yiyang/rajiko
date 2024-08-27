(asdf:defsystem #:rajiko
  :author ("凉凉")
  :version "0"
  :description "This is a package to listen Rajiko in CLI. "
  :depends-on (dexador plump clss str qbase64 cl-tui cl-setlocale)
  :serial t
  :components
  ((:module statics
    :pathname "statics"
    :components
    ((:static-file "fullkey-base64")
     (:static-file "rajiko-stations.xml")))
   (:module lisp
    :pathname "lisp"
    :components
    ((:file "package")
     (:module "utils"
      :pathname "utils"
      :components
      ((:file "statics")))
     ;; backend
     (:module "backend"
      :pathname "backend"
      :components
      ((:file "statics")
       (:file "station")
       (:file "rajiko")))
     ;; ui
     (:module "ui"
      :pathname "ui"
      :components
      ((:file "ncurses")))
     ))))
