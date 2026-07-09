(in-package :asdf-user)

(asdf:defsystem #:rajiko
  :author ("凉凉")
  :version "0"
  :description "This is a package to listen Rajiko in CLI. "
  :depends-on (local-time dexador plump clss str qbase64 cl-tui cl-setlocale)
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname  "rajiko"
  :entry-point     "rajiko.ui::rajiko-cli"
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
       (:file "config")
       (:file "rajiko")))
     ;; ui
     (:module "ui"
      :pathname "ui"
      :components
      ((:file "ncurses")))))))

(asdf:defsystem #:rajiko/test
  :depends-on (#:rajiko #:fiveam)
  :perform (test-op (o s)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :rajiko-suite :rajiko.test)))
  :components
  ((:module "test"
    :pathname "test"
    :components
    ((:file "package")
     (:file "config")
     (:file "rajiko")
     (:file "station")
     (:file "integration")))))
