(defpackage #:rajiko.utils
  (:use :cl)
  (:import-from :str
   :join)
  (:export
   #:find-statics))

(defpackage #:rajiko.backend
  (:use :cl :rajiko.utils)
  (:import-from :str
   :concat :join)
  (:import-from :alexandria
   :hash-table-keys
   :hash-table-values)
  (:export
   #:pick-random
   ;; station
   #:rajiko-station
   #:rajiko-region
   #:rajiko-station-id
   #:rajiko-station-name
   #:rajiko-station-ascii-name
   #:rajiko-station-region
   #:rajiko-station-logo
   #:rajiko-station-banner
   #:rajiko-station-href
   #:rajiko-stations-refresh
   #:rajiko-random-station
   #:rajiko-avalible-region-id
   ;; rajiko
   #:rajiko
   #:rajiko-app-version
   #:rajiko-user-id
   #:rajiko-user-agent
   #:rajiko-device
   #:rajiko-area
   #:rajiko-location
   #:rajiko-token
   #:rajiko-partial-key
   #:rajiko-random-area
   #:make-rajiko
   #:rajiko-play
   #:rajiko-pause
   #:rajiko-status
   ))

(defpackage #:rajiko.ui
  (:use :cl :cl-tui :rajiko.backend)
  (:export
   #:rajiko-cli
   #:rajiko-cli-with-sly))

(defpackage #:rajiko
  (:use :cl :rajiko.ui)
  (:export
   #:rajiko-cli))
