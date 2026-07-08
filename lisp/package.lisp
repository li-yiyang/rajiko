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
    ;; all-stations
    #:*all-stations*
    #:rajiko-station-area-id
    #:area-code-to-area-name
    ;; region
    #:rajiko-region-name
    #:rajiko-region-ascii-name
    #:rajiko-region-id
    #:rajiko-region-stations
    ;; config
    #:*config*
    #:*test-config-paths*
    #:load-config
    #:save-config
    #:config-path
    #:config-paths
    #:merge-config
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
   #:rajiko-re-auth
   #:rajiko-play
   #:rajiko-pause
   #:rajiko-status
    #:rajiko-ts-playlist
    #:rajiko-ts-play
     #:timeshift-programs
     #:program-today
     #:current-program-name
     #:station-regions
     #:yyyymmddhhmmss-to-unix
    #:area-name-to-id
    #:get-unix-time
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
