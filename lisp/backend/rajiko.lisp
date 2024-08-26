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
  (assert (member area +coordinates-alist+ :key #'first))
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
	    device      (concat sdk "." model)
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
