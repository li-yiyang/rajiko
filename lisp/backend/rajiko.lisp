(in-package :rajiko.backend)

(defmacro alist-getf (alist item key)
  `(getf (cdr (assoc ,item ,alist)) ,key))

(declaim (inline pick-random))
(defun pick-random (list)
  (nth (random (length list)) list))

(defun extract-partialkey (offset length)
  (qbase64:encode-bytes
   (subseq +fullkey+ offset (+ offset length))))

(defun gen-random-userid ()
  (let ((userid ()))
    (dotimes (i 32 (join "" userid))
      (push (format nil "~(~X~)" (random 16)) userid))))

(defun gen-GPS (area)
  (destructuring-bind (lat long)
      (cdr (assoc area +coordinates-alist+ :test #'equal))
    (flet ((random-offset ()
	     (* (if (> (random 1.0) 0.5) 1 -1)
		;; +/- 2.77/2.13 km
		(random 0.025))))
      (format nil "~,6F,~,6F,gps"
	      (+ lat (random-offset))
	      (+ long (random-offset))))))

;; (defparameter *rajiko-client-cache*
;;   (make-hash-table :test 'equal)
;;   "Cache of Rajiko Clients. ")

(defclass rajiko ()
  ((app-version :reader rajiko-app-version)
   (user-id     :reader rajiko-user-id)
   (user-agent  :reader rajiko-user-agent)
   (device      :reader rajiko-device)
   (area        :initarg  :area
		:initform (error "Missing area. ")
		:reader rajiko-area)
   (token       :reader rajiko-token)
   (partial-key :reader rajiko-partial-key)
   (location    :reader rajiko-location)
   (player      :initform nil))
  (:documentation
   "The client of rajiko, with GPS bypassing. "))

(defun rajiko-random-area ()
  (first (pick-random +coordinates-alist+)))

(defmethod make-rajiko (area)
  "Make a Rajiko client for Radiko. "
  (assert (member area +coordinates-alist+ :key #'first :test #'equal))
  (make-instance 'rajiko :area area))

(defmethod print-object ((rajiko rajiko) stream)
  (with-slots (area token) rajiko
    (format stream "#<rajiko ~A ~A>" area token)))

(defmethod initialize-instance :after ((rajiko rajiko) &key (dummy nil))
  (with-slots (app-version device user-id user-agent
	       token partial-key area location)
      rajiko
    (let* ((version (first (pick-random +version-alist+)))
	   (sdk        (alist-getf +version-alist+ version :sdk))
	   (build      (pick-random (alist-getf +version-alist+ version :builds)))
	   (model      (pick-random +model-list+)))
      ;; generate random infomation
      (setf app-version (pick-random +app-version-list+)
	    user-id     (gen-random-userid)
	    user-agent  (concat "Dalvik/2.1.0 (Linux; U; Android "
				version "; " model "/" build ")")
	    device      (concat (format nil "~A" sdk) "." model)
	    location    (gen-GPS area)))
      ;; if dummy, will not auth
      (unless dummy
	(auth rajiko))))

(defmethod auth ((rajiko rajiko))
  (auth1 rajiko)
  (auth2 rajiko))

(defmethod auth1 ((rajiko rajiko))
  (with-slots (user-agent app-version device user-id token partial-key) rajiko
    (multiple-value-bind (response http-code headers)
	(dex:get "https://radiko.jp/v2/api/auth1"
		 :headers `(("User-Agent"           . ,user-agent)
			    ("X-Radiko-App"         . "aSmartPhone7a")
			    ("X-Radiko-App-Version" . ,app-version)
			    ("X-Radiko-Device"      . ,device)
			    ("X-Radiko-User"        . ,user-id)))
      (let ((offset     (parse-integer (gethash "x-radiko-keyoffset" headers)))
	    (length     (parse-integer (gethash "x-radiko-keylength" headers))))
	(setf token (gethash "x-radiko-authtoken" headers)
	      partial-key (extract-partialkey offset length))
	(values response http-code)))))

(defmethod auth2 ((rajiko rajiko))
  (with-slots (user-agent app-version token device
	       user-id location partial-key)
      rajiko
    (multiple-value-bind (response http-code)
	(dex:get "https://radiko.jp/v2/api/auth2"
		 :headers `(("User-Agent"           . ,user-agent)
			    ("X-Radiko-App"         . "aSmartPhone7a")
			    ("X-Radiko-App-Version" . ,app-version)
			    ("X-Radiko-AuthToken"   . ,token)
			    ("X-Radiko-Device"      . ,device)
			    ("X-Radiko-User"        . ,user-id)
			    ("X-Radiko-Location"    . ,location)
			    ("X-Radiko-Connection"  . "wifi")
			    ("X-Radiko-Partialkey"  . ,partial-key)))
      (values response http-code))))

(defmethod rajiko-re-auth ((rajiko rajiko) new-area)
  (assert (member new-area +coordinates-alist+ :key #'first :test #'equal))
  (with-slots (area location) rajiko
    (setf area    new-area
          location (gen-GPS new-area)))
  (auth rajiko)
  (rajiko-token rajiko))

(defmethod rajiko-play ((rajiko rajiko) (station rajiko-station))
  (with-slots (player) rajiko
    (when player (rajiko-pause rajiko))
    (setf player
	  (uiop:launch-program `("ffplay"
				 "-v" "0"
				 "-headers" ,(concat "X-Radiko-AuthToken: "
						     (rajiko-token rajiko))
				 "-i" ,(rajiko-station-streaming-url
					station (rajiko-area rajiko))
				 "-nodisp")))))

(defmethod rajiko-pause ((rajiko rajiko))
  (with-slots (player) rajiko
    (when (and player (uiop:process-alive-p player))
      (uiop:terminate-process player :urgent t))
    (setf player nil)))

(defmethod rajiko-status (rajiko)
  (declare (ignore rajiko))
  :unknown)

(defmethod rajiko-status ((rajiko rajiko))
  (with-slots (player) rajiko
    (if (and player (uiop:process-alive-p player))
	:playing :paused)))

(defun rajiko-ts-playlist (station ft to &optional (lt 15))
  "Construct and return the timeshift M3U8 playlist URL."
  (format nil "https://radiko.jp/v2/api/ts/playlist.m3u8?station_id=~A&ft=~D&to=~D&l=~D"
          (rajiko-station-id station) ft to lt))

(defmethod rajiko-ts-play ((rajiko rajiko) (station rajiko-station) ft to &key (lt 15))
  (with-slots (player) rajiko
    (when player (rajiko-pause rajiko))
    (setf player
	  (uiop:launch-program `("ffplay"
				 "-v" "0"
				 "-headers" ,(concat "X-Radiko-AuthToken: "
						     (rajiko-token rajiko))
				 "-i" ,(rajiko-ts-playlist station ft to lt)
				 "-nodisp")))))

(defun get-unix-time ()
  (- (get-universal-time) 2208988800))

(defun station-regions (station)
  "Return list of regions that broadcast STATION."
  (let ((station-id (rajiko-station-id station))
        (result nil))
    (maphash (lambda (id region)
               (declare (ignore id))
               (when (gethash station-id (rajiko-region-stations region))
                 (push region result)))
             +rajiko-regions+)
    result))

(defun yyyymmddhhmmss-to-unix (timestamp)
  "Convert YYYYMMDDHHmmss integer to Unix timestamp."
  (let* ((str (format nil "~14,'0D" timestamp))
         (y (parse-integer (subseq str 0 4)))
         (m (parse-integer (subseq str 4 6)))
         (d (parse-integer (subseq str 6 8)))
         (hh (parse-integer (subseq str 8 10)))
         (mm (parse-integer (subseq str 10 12)))
         (ss (parse-integer (subseq str 12 14))))
    (- (encode-universal-time ss mm hh d m y) 2208988800)))

(defun area-name-to-id (area-name)
  (let ((index (position area-name +coordinates-alist+ :key #'car :test #'equal)))
    (when index
      (format nil "JP~2,'0D" (1+ index)))))

(defun timeshift-programs (station area &optional date)
  (declare (ignore area))
  (let* ((station-id (rajiko-station-id station))
         (date-str (or date
                       (multiple-value-bind (y m d)
                           (decode-universal-time (get-universal-time))
                         (format nil "~4,'0D~2,'0D~2,'0D" y m d))))
         (url (format nil "https://radiko.jp/v3/program/station/date/~A/~A.xml"
                      date-str station-id))
         (xml (handler-case (plump:parse (dex:get url))
                (error () (return-from timeshift-programs nil))))
         (stations (clss:select "stations station" xml)))
    (loop for station-node in (map 'list #'identity stations)
          when (string= (plump:text (aref (clss:select "id" station-node) 0))
                        station-id)
            append (loop for prog in (map 'list #'identity
                                          (clss:select "scd prog" station-node))
                         for ft = (parse-integer (plump:attribute prog "ft"))
                         for to = (parse-integer (plump:attribute prog "to"))
                         for ftl = (plump:attribute prog "ftl")
                         for tol = (plump:attribute prog "tol")
                         for title = (plump:text (aref (clss:select "title" prog) 0))
                         for desc = (plump:text (aref (clss:select "desc" prog) 0))
                         for pfm = (plump:text (aref (clss:select "pfm" prog) 0))
                         collect (list :ft ft :to to
                                       :ftl ftl :tol tol
                                       :title title
                                       :desc desc
                                       :pfm pfm)))))

(defun todays-date-string ()
  "Return today's date as YYYYMMDD string using local-time."
  (let ((now (local-time:now)))
    (format nil "~4,'0D~2,'0D~2,'0D"
            (local-time:timestamp-year now)
            (local-time:timestamp-month now)
            (local-time:timestamp-day now))))

(defun program-today (station)
  (let* ((station-id (rajiko-station-id station))
         (date-str (todays-date-string))
         (url (format nil "https://radiko.jp/v3/program/station/date/~A/~A.xml"
                      date-str station-id))
         (xml (handler-case (plump:parse (dex:get url))
                (error () (return-from program-today nil))))
         (stations-node (ignore-errors
                          (aref (clss:select "stations station" xml) 0))))
    (when stations-node
      (loop for prog in (map 'list #'identity (clss:select "scd prog" stations-node))
            for ft = (parse-integer (plump:attribute prog "ft"))
            for to = (parse-integer (plump:attribute prog "to"))
            for ftl = (plump:attribute prog "ftl")
            for tol = (plump:attribute prog "tol")
            for title = (plump:text (aref (clss:select "title" prog) 0))
            for desc = (plump:text (aref (clss:select "desc" prog) 0))
            for pfm = (plump:text (aref (clss:select "pfm" prog) 0))
            collect (list :ft ft :to to
                          :ftl ftl :tol tol
                          :title title
                          :desc desc
                          :pfm pfm)))))

(defun current-program-name (station area)
  (let* ((area-id (area-name-to-id area))
         (now (get-unix-time))
         (date-str (todays-date-string))
         (url (format nil "https://radiko.jp/v3/program/date/~A/~A.xml" date-str area-id))
         (xml (handler-case (plump:parse (dex:get url))
                (error () (return-from current-program-name nil))))
         (station-id (rajiko-station-id station))
         (stations (clss:select "stations station" xml)))
    (loop for station-node in (map 'list #'identity stations)
          when (string= (plump:text (aref (clss:select "id" station-node) 0))
                        station-id)
            do (loop for prog in (map 'list #'identity
                                      (clss:select "scd prog" station-node))
                     for ft = (parse-integer (plump:attribute prog "ft"))
                     for to = (parse-integer (plump:attribute prog "to"))
                     when (and (<= (yyyymmddhhmmss-to-unix ft) now)
              (< now (yyyymmddhhmmss-to-unix to)))
                       do (return-from current-program-name
                            (plump:text (aref (clss:select "title" prog) 0)))))))
