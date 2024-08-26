(in-package :rajiko.backend)

;; This file defines how the rajiko-stations.xml
;; which could be fetched via `+rajiko-station-url+'
;; (and stored under `statics/rajiko-stations.xml').
;;
;; The information should be fetched in `statics.lisp',
;; see `rajiko.backend::+rajiko-stations+' for details.
;;
;; The `rajiko.backend::+rajiko-stations+' is a hash
;; table whose key is the area id

(defparameter +rajiko-regions+
  (make-hash-table :test 'equal))

(defclass rajiko-station ()
  ((id         :initarg :id
	       :reader  rajiko-station-id)
   (name       :initarg :name
	       :reader  rajiko-station-name)
   (ascii-name :initarg :ascii-name
	       :reader  rajiko-station-ascii-name)
   (region     :initarg :region
	       :reader  rajiko-station-region)
   (logo       :initarg :logo
	       :reader  rajiko-station-logo)
   (banner     :initarg :banner
	       :reader  rajiko-station-banner)
   (href       :initarg :href
	       :reader  rajiko-station-href))
  (:documentation ""))

(defmethod print-object ((station rajiko-station) stream)
  (with-slots (id name) station
    (format stream "#<~A ~A>" id name)))

(defmethod rajiko-station-HLS-streaming-url ((station rajiko-station))
  (with-slots (id) station
    (let* ((url (concat "https://radiko.jp/v2/station/stream_smh_multi/"
			id
			".xml"))
	   (xml (clss:select "playlist_create_url"
			     (plump:parse (dex:get url)))))
      (map 'list #'plump:text xml))))

(defmethod rajiko-station-streaming-url ((station rajiko-station) area)
  (declare (ignore area))
  (first (rajiko-station-HLS-streaming-url station)))

(defun %xml-to-station (xml)
  "Given node of station XML.
Return a `rajiko-station' object. "
  (macrolet ((xml-slot (xml tag)
	       `(aref (clss:select ,tag ,xml) 0)))
    (let ((id         (plump:text (xml-slot xml "id")))
	  (name       (plump:text (xml-slot xml "name")))
	  (ascii-name (plump:text (xml-slot xml "ascii_name")))
	  ;; region will be later assigned, see
	  ;; `%xml-to-region' function
	  (logo       (plump:text (xml-slot xml "logo")))
	  (banner     (plump:text (xml-slot xml "banner")))
	  (href       (plump:text (xml-slot xml "href"))))
      (make-instance 'rajiko-station
		     :id         id
		     :name       name
		     :ascii-name ascii-name
		     :logo       logo
		     :banner     banner
		     :href       href))))

(defclass rajiko-region ()
  ((name       :initarg :name
	       :reader  rajiko-region-name)
   (ascii-name :initarg :ascii-name
	       :reader  rajiko-region-ascii-name)
   (id         :initarg :id
	       :reader  rajiko-region-id)
   (stations   :initarg :stations
	       :reader  rajiko-region-stations))
  (:documentation ""))

(defmethod print-object ((region rajiko-region) stream)
  (with-slots (id name stations) region
    (let ((len (hash-table-count stations)))
      (format stream "#<~A ~A ~D station~P>"
	      id name len len))))

(defun %xml-to-region (xml)
  "Given node of stations XML.
Return a `rajiko-region' object. "
  (let* ((id         (plump:attribute xml "region_id"))
	 (name       (plump:attribute xml "region_name"))
	 (ascii-name (plump:attribute xml "ascii_name"))
	 (stations   (make-hash-table :test 'equal))
	 (region     (make-instance 'rajiko-region
				    :stations   stations
				    :id         id
				    :ascii-name ascii-name
				    :name       name)))
    (loop for station in (map 'list #'%xml-to-station
			      (clss:select "station" xml))
	  do (setf (slot-value station 'region) region
		   (gethash (rajiko-station-id station) stations) station))
    region))

(defun %update-rajiko-station (rajiko-station-xml)
  (let ((regions (clss:select "region stations"
			      (plump:parse rajiko-station-xml))))
    (map nil
	 (lambda (region-xml)
	   (let ((region (%xml-to-region region-xml)))
	     (setf (gethash (rajiko-region-id region) +rajiko-regions+)
		   region)))
	 regions)))

(with-open-file (station-xml (find-statics statics "rajiko-stations.xml"))
  (%update-rajiko-station station-xml))

(defun rajiko-stations-refresh ()
  "Update `+rajiko-regions+' infomation. "
  (%update-rajiko-station
   (dex:get "https://radiko.jp/v3/station/region/full.xml")))

(defun rajiko-random-station (&optional (region-id nil region-id?))
  "Return a random station. "
  (if region-id?
      (let ((region (gethash region-id +rajiko-regions+ nil)))
	(when region
	  (pick-random (hash-table-values (rajiko-region-stations region)))))
      (rajiko-random-station
       (pick-random (hash-table-keys +rajiko-regions+)))))

(defun rajiko-avalible-region-id ()
  "Return a list of avaliable region id. "
  (hash-table-keys +rajiko-regions+))
