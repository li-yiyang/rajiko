(in-package :rajiko.ui)

(defparameter *rajiko-areas*
  (mapcar #'first rajiko.backend::+coordinates-alist+))

(defparameter *rajiko-area-nth*
  (random (length *rajiko-areas*)))

(defparameter *rajiko-area*
  (nth *rajiko-area-nth* *rajiko-areas*)
  "Rajiko Client Area. (by default picked randomly)")

(defparameter *rajiko-regions*
  (alexandria:hash-table-keys rajiko.backend::+rajiko-regions+))

(defparameter *rajiko-region-nth* 0)

(defparameter *rajiko-region*
  (nth *rajiko-region-nth* *rajiko-regions*))

(defparameter *rajiko-stations*
  (alexandria:hash-table-keys
   (rajiko.backend::rajiko-region-stations
    (gethash *rajiko-region* rajiko.backend::+rajiko-regions+))))

(defparameter *rajiko-station-nth* 0)

(defparameter *rajiko-station*
  (gethash (nth *rajiko-station-nth* *rajiko-stations*)
	   (rajiko.backend::rajiko-region-stations
	    (gethash *rajiko-region* rajiko.backend::+rajiko-regions+))))

(defparameter *rajiko* nil)

(defparameter *current-status*
  :region)

(defun prev-region ()
  (rajiko-pause *rajiko*)
  (setf *rajiko-region-nth*
	(mod (1- *rajiko-region-nth*)
	     (length *rajiko-regions*)))
  (setf *rajiko-region*
	(nth *rajiko-region-nth* *rajiko-regions*))
  (setf *rajiko-station-nth* 0)
  (setf *rajiko-stations*
	(alexandria:hash-table-keys
	 (rajiko.backend::rajiko-region-stations
	  (gethash *rajiko-region* rajiko.backend::+rajiko-regions+))))
  (setf *rajiko-station*
	(gethash (nth *rajiko-station-nth* *rajiko-stations*)
		 (rajiko.backend::rajiko-region-stations
		  (gethash *rajiko-region* rajiko.backend::+rajiko-regions+)))))

(defun next-region ()
  (rajiko-pause *rajiko*)
  (setf *rajiko-region-nth*
	(mod (1+ *rajiko-region-nth*)
	     (length *rajiko-regions*)))
  (setf *rajiko-region*
	(nth *rajiko-region-nth* *rajiko-regions*))
  (setf *rajiko-station-nth* 0)
  (setf *rajiko-stations*
	(alexandria:hash-table-keys
	 (rajiko.backend::rajiko-region-stations
	  (gethash *rajiko-region* rajiko.backend::+rajiko-regions+))))
  (setf *rajiko-station*
	(gethash (nth *rajiko-station-nth* *rajiko-stations*)
		 (rajiko.backend::rajiko-region-stations
		  (gethash *rajiko-region* rajiko.backend::+rajiko-regions+)))))

(defun prev-station ()
  (rajiko-pause *rajiko*)
  (setf *rajiko-station-nth*
	(mod (1- *rajiko-station-nth*)
	     (length *rajiko-stations*)))
  (setf *rajiko-station*
	(gethash (nth *rajiko-station-nth* *rajiko-stations*)
		 (rajiko.backend::rajiko-region-stations
		  (gethash *rajiko-region* rajiko.backend::+rajiko-regions+)))))

(defun next-station ()
  (rajiko-pause *rajiko*)
  (setf *rajiko-station-nth*
	(mod (1+ *rajiko-station-nth*)
	     (length *rajiko-stations*)))
  (setf *rajiko-station*
	(gethash (nth *rajiko-station-nth* *rajiko-stations*)
		 (rajiko.backend::rajiko-region-stations
		  (gethash *rajiko-region* rajiko.backend::+rajiko-regions+)))))

(defun prev-area ()
  (rajiko-pause *rajiko*)
  (setf *rajiko-area-nth*
	(mod (1- *rajiko-area-nth*)
	     (length *rajiko-areas*)))
  (setf *rajiko-area*
	(nth *rajiko-area-nth* *rajiko-areas*))
  (setf *current-status* :select-area-changed))

(defun next-area ()
  (rajiko-pause *rajiko*)
  (setf *rajiko-area-nth*
	(mod (1+ *rajiko-area-nth*)
	     (length *rajiko-areas*)))
  (setf *rajiko-area*
	(nth *rajiko-area-nth* *rajiko-areas*))
  (setf *current-status* :select-area-changed))

(defun update-rajiko-client ()
  (when (and *rajiko* (eq (rajiko-status *rajiko*) :playing))
    (rajiko-pause *rajiko*))
  (unless (and *rajiko* (string= (rajiko-area *rajiko*) *rajiko-area*))
    (setf *rajiko* (make-rajiko *rajiko-area*))))

(defun render-main-controller (&key frame)
  (draw-box frame)
  (put-text frame 0 2 "Rajiko")
  (put-text frame 2 1 "Region     [r]: ~A" *rajiko-region*)
  (put-text frame 3 1 "Station    [s]: ~A"
	    (rajiko-station-ascii-name *rajiko-station*))
  (put-text frame 4 1 "Client     [a]: ~A" *rajiko-area*)
  (put-text frame 5 1 "Status [Space]: ~A" (rajiko-status *rajiko*)))

(defmacro render-list (frame title list ptr max-h)
  `(progn
     (draw-box ,frame)
     (put-text ,frame 0 1 ,title)
     (loop with max = (length ,list)
	   with half-h = (truncate ,max-h 2)
	   for idx from (min (max (- ,ptr half-h) 0)
			     (max (- max h -2) 0))
	     below max
	   for line from 1 below (- ,max-h 2)
	   for item = (nth idx ,list)
	   do (put-text frame line 3 item)
	   if (= idx ,ptr)
	     do (put-text frame line 1 "> "))))


(defun render-select-region (&key frame h)
  (render-list frame "Select Region"
	       *rajiko-regions* *rajiko-region-nth* h))

(defun render-select-station (&key frame h)
  (render-list frame "Select Station"
	       *rajiko-stations* *rajiko-station-nth* h))

(defun render-select-area (&key frame h)
  (render-list frame "Select Area"
	       *rajiko-areas* *rajiko-area-nth* h))

(define-frame controller-region (container-frame))
(define-children controller-region ()
  (c-1 (simple-frame :render #'render-main-controller))
  (s-r (simple-frame :render #'render-select-region)))

(define-frame controller-station (container-frame))
(define-children controller-station ()
  (c-2 (simple-frame :render #'render-main-controller))
  (s-s (simple-frame :render #'render-select-station)))

(define-frame controller-area (container-frame))
(define-children controller-area ()
  (c-3 (simple-frame :render #'render-main-controller))
  (s-a (simple-frame :render #'render-select-area)))

(defun rajiko-cli ()
  (unwind-protect
       (%rajiko-cli)
    (when (and *rajiko* (eq (rajiko-status *rajiko*) :playing))
      (rajiko-pause *rajiko*))))

(defun %rajiko-cli ()
  (update-rajiko-client)
  (with-screen ()
    (refresh)
    (display 'controller-region)
    (loop for key = (read-key)
	  do (case key
	       (#\Esc
		(rajiko-pause *rajiko*)
		(return))
	       (#\q
		(rajiko-pause *rajiko*)
		(return))
	       (#\Space
		(when (eq *current-status* :select-area-changed)
		  (update-rajiko-client))
		(case (rajiko-status *rajiko*)
		  (:playing (rajiko-pause *rajiko*))
		  (:paused  (rajiko-play  *rajiko* *rajiko-station*))
		  (t (update-rajiko-client))))
	       (#\r
		(display 'controller-region)
		(setf *current-status* :region))
	       (#\s
		(display 'controller-station)
		(setf *current-status* :station))
	       (#\a
		(display 'controller-area)
		(setf *current-status* :area))
	       (:key-up
		(case *current-status*
		  (:sation (prev-station))
		  (:region (prev-region))
		  ((:area :select-area-changed)
		   (prev-area))
		  (otherwise nil)))
	       (:key-down
		(case *current-status*
		  (:station (next-station))
		  (:region  (next-region))
		  ((:area :select-area-changed)
		   (next-area))
		  (otherwise nil)))
	       (t nil))
	  do (refresh))))

;; (defun rajiko-cli-with-sly ()
;;   (slynk:create-server :dont-close t)
;;   (rajiko-cli))
