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

(defparameter *status-message* ""
  "Status text displayed in modeline.")

(defparameter *programs* nil
  "List of program plists for the current station/area/date.")

(defparameter *program-nth* 0
  "Index into *programs* for the currently selected program.")

(defparameter *current-pane* :station
  "Current active pane: :station, :region, or :program.")

(defparameter *station-regions* nil
  "List of regions that broadcast *rajiko-station*.")

(defparameter *station-region-nth* 0
  "Index into *station-regions*.")

(defparameter *program-date-offset* 0
  "Day offset from today (0 = today, -1 = yesterday, 1 = tomorrow).")

(defparameter *last-program-refresh* nil
  "Universal time of the last program list refresh.")

(defun sorted-stations ()
  (let* ((favorites (getf *config* :favorites))
         (faved (make-hash-table :test 'equal)))
    (when favorites
      (dolist (id favorites)
        (setf (gethash id faved) t)))
    (sort (copy-seq rajiko.backend:*all-stations*)
          (lambda (a b)
            (let ((a-fav (gethash (car a) faved))
                  (b-fav (gethash (car b) faved)))
              (cond ((and a-fav (not b-fav)) t)
                    ((and b-fav (not a-fav)) nil)
                    (t (string-lessp (rajiko-station-ascii-name (cdr a))
                                     (rajiko-station-ascii-name (cdr b))))))))))

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
                  (gethash *rajiko-region* rajiko.backend::+rajiko-regions+))))
  (refresh-programs))

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
                  (gethash *rajiko-region* rajiko.backend::+rajiko-regions+))))
  (refresh-programs))

(defun prev-station ()
  (rajiko-pause *rajiko*)
  (let ((stations (sorted-stations)))
    (setf *rajiko-station-nth*
          (mod (1- *rajiko-station-nth*)
               (length stations)))
    (setf *rajiko-station*
          (cdr (nth *rajiko-station-nth* stations)))
    (setf *station-regions* (station-regions *rajiko-station*)
          *station-region-nth* 0)
    (refresh-programs)))

(defun next-station ()
  (rajiko-pause *rajiko*)
  (let ((stations (sorted-stations)))
    (setf *rajiko-station-nth*
          (mod (1+ *rajiko-station-nth*)
               (length stations)))
    (setf *rajiko-station*
          (cdr (nth *rajiko-station-nth* stations)))
    (setf *station-regions* (station-regions *rajiko-station*)
          *station-region-nth* 0)
    (refresh-programs)))

(defun prev-area ()
  (rajiko-pause *rajiko*)
  (setf *rajiko-area-nth*
        (mod (1- *rajiko-area-nth*)
             (length *rajiko-areas*)))
  (setf *rajiko-area*
        (nth *rajiko-area-nth* *rajiko-areas*)))

(defun next-area ()
  (rajiko-pause *rajiko*)
  (setf *rajiko-area-nth*
        (mod (1+ *rajiko-area-nth*)
             (length *rajiko-areas*)))
  (setf *rajiko-area*
        (nth *rajiko-area-nth* *rajiko-areas*)))

(defun prev-program ()
  (setf *program-nth*
        (mod (1- *program-nth*)
             (max 1 (length *programs*)))))

(defun next-program ()
  (setf *program-nth*
        (mod (1+ *program-nth*)
             (max 1 (length *programs*)))))

(defun prev-station-region ()
  (setf *station-region-nth*
        (mod (1- *station-region-nth*)
             (max 1 (length *station-regions*)))))

(defun next-station-region ()
  (setf *station-region-nth*
        (mod (1+ *station-region-nth*)
             (max 1 (length *station-regions*)))))

(defun select-station-region ()
  (let ((region (nth *station-region-nth* *station-regions*)))
    (when region
      (let ((region-id (rajiko-region-id region)))
        (setf *rajiko-region-nth*
              (or (position region-id *rajiko-regions* :test #'equal) 0)
              *rajiko-region*
              (nth *rajiko-region-nth* *rajiko-regions*))
        (setf *rajiko-stations*
              (alexandria:hash-table-keys
               (rajiko-region-stations
                (gethash *rajiko-region* rajiko.backend::+rajiko-regions+))))
        (refresh-programs)))))

(defun refresh-programs ()
  (setf *programs*
        (if (zerop *program-date-offset*)
            (program-today *rajiko-station*)
            (multiple-value-bind (y m d)
                (decode-universal-time (+ (get-universal-time)
                                          (* *program-date-offset* 86400)))
              (timeshift-programs *rajiko-station* *rajiko-area*
                                  (format nil "~4,'0D~2,'0D~2,'0D" y m d))))
        *program-nth* 0
        *last-program-refresh* (get-universal-time)))

(defun restore-config-state ()
  (load-config)
  (let* ((last-station (getf *config* :last-station))
         (stations (sorted-stations))
         (pos (if last-station
                  (position last-station stations :key #'car :test #'equal)
                  nil)))
    (when pos
      (setf *rajiko-station-nth* pos
            *rajiko-station* (cdr (nth pos stations))))
    (let ((last-area (getf *config* :last-area)))
      (when (and last-area (member last-area *rajiko-areas* :test #'equal))
        (setf *rajiko-area-nth* (position last-area *rajiko-areas* :test #'equal)
              *rajiko-area* last-area)))
    (let ((last-region (getf *config* :last-region)))
      (when (and last-region (member last-region *rajiko-regions* :test #'equal))
        (setf *rajiko-region-nth* (position last-region *rajiko-regions* :test #'equal)
              *rajiko-region* last-region))))
  (setf *station-regions* (station-regions *rajiko-station*)
        *station-region-nth* 0))

(defun toggle-favorite ()
  (let* ((id (rajiko-station-id *rajiko-station*))
         (favorites (getf *config* :favorites)))
    (if (member id favorites :test #'equal)
        (setf (getf *config* :favorites) (remove id favorites :test #'equal))
        (push id (getf *config* :favorites)))
    (setf *rajiko-station-nth*
          (position (rajiko-station-id *rajiko-station*)
                    (sorted-stations) :key #'car :test #'equal))
    (save-config)))

(defun format-timestamp (ft)
  (if ft
      (let* ((str (format nil "~14,'0D" ft)))
        (format nil "~A:~A" (subseq str 8 10) (subseq str 10 12)))
      "--:--"))

(defun fit-text (str w)
  "Return STR truncated to at most W characters."
  (if (> (length str) w) (subseq str 0 w) str))

(defun draw-hsep (frame y w)
  (put-text frame y 0 (make-string w :initial-element #\-)))

(defun draw-pane-top (frame y left-w total-w)
  (put-text frame y 0
            (fit-text
             (concatenate 'string
                          (make-string (1- left-w) :initial-element #\-)
                          "+"
                          (make-string (- total-w left-w 2) :initial-element #\-))
             total-w)))

(defun render-list-into (frame offset-y offset-x max-h max-w items selected-idx
                         &key (render-fn #'identity))
  (let* ((num-items (length items))
         (half-h (truncate max-h 2))
         (scroll-pos (min (max (- selected-idx half-h) 0)
                          (max (- num-items max-h) 0))))
    (loop for line from 0 below max-h
          for idx from scroll-pos
          when (>= idx num-items) return nil
          do (let ((item (nth idx items)))
               (if (= idx selected-idx)
                   (with-attributes (:reverse) frame
                     (put-text frame (+ offset-y line) offset-x "> ~A"
                               (funcall render-fn item)))
                   (put-text frame (+ offset-y line) offset-x "  ~A"
                             (funcall render-fn item)))))))

(defun render-key-hints (frame y w)
  (declare (ignore w))
  (ecase *current-pane*
    (:station
     (put-text frame y 1
               "Station [Up/Dn]  Region [Left]  Program [Right]  Play [Space]  Fave [f]  Quit [q]"))
    (:region
     (put-text frame y 1
               "Region [Up/Dn]  Select [Enter]  Back [r/Esc]  Play [Space]  Quit [q]"))
    (:program
     (put-text frame y 1
               "Program [Up/Dn]  Prev [p]  Next [n]  Play [Space]  Refresh [g]  Back [Left]  Quit [q]"))))

(defun render-main-screen (&key frame h w)
  (let* ((left-w (min (truncate w 3) 30))
         (right-w (- w left-w 1))
         (content-top 5)
         (content-bot (- h 3))
         (content-h (- content-bot content-top)))
    (put-text frame 0 0
              (format nil "+~A" (make-string (- w 2) :initial-element #\-)))
    (render-key-hints frame 1 w)
    (draw-hsep frame 2 w)
    (if (eq *current-pane* :region)
        (put-text frame 3 1
                  (fit-text
                   (format nil "Regions for ~A | Programs -- ~A"
                           (rajiko-station-ascii-name *rajiko-station*)
                           (rajiko-station-ascii-name *rajiko-station*))
                   (- w 1)))
        (put-text frame 3 1
                  (fit-text
                   (format nil "Stations (~A) | Programs -- ~A"
                           (let ((reg (gethash *rajiko-region* rajiko.backend::+rajiko-regions+)))
                             (if reg
                                 (rajiko-region-ascii-name reg)
                                 *rajiko-region*))
                           (rajiko-station-ascii-name *rajiko-station*))
                   (- w 1))))
    (draw-pane-top frame 4 left-w w)
    (if (eq *current-pane* :region)
        (render-list-into frame content-top 1 content-h (1- left-w)
                          *station-regions* *station-region-nth*
                          :render-fn (lambda (r)
                                       (rajiko-region-ascii-name r)))
        (render-list-into frame content-top 1 content-h (1- left-w)
                          (sorted-stations) *rajiko-station-nth*
                          :render-fn (lambda (s)
                                       (rajiko-station-ascii-name (cdr s)))))
    (loop for y from content-top below content-bot
          do (put-char frame y left-w #\|))
    (render-list-into frame content-top (+ left-w 1) content-h (1- right-w)
                      *programs* *program-nth*
                      :render-fn (lambda (p)
                                   (let ((now (get-unix-time))
                                         (ft (getf p :ft))
                                         (to (getf p :to)))
                                     (format nil "~A ~A ~A"
                                             (if (and ft to
                                                      (< (yyyymmddhhmmss-to-unix ft) now)
                                                      (< now (yyyymmddhhmmss-to-unix to)))
                                                 "[now]"
                                                 "     ")
                                             (format-timestamp (getf p :ft))
                                             (getf p :title)))))
    (draw-hsep frame (- h 3) w)
    (let ((status (if *rajiko* (rajiko-status *rajiko*) :stopped)))
      (put-text frame (- h 2) 1
                (fit-text
                 (format nil "~A~A | ~A | ~A | ~A"
                         (rajiko-station-ascii-name *rajiko-station*)
                         (if (member (rajiko-station-id *rajiko-station*)
                                     (getf *config* :favorites) :test #'equal)
                             " *" "")
                         (let ((reg (gethash *rajiko-region* rajiko.backend::+rajiko-regions+)))
                           (rajiko-region-ascii-name reg))
                         status
                         *status-message*)
                  (- w 1))))
    (put-text frame (1- h) 0
              (format nil "+~A" (make-string (- w 2) :initial-element #\-)))))

(defun render-area-picker (&key frame h w)
  (put-text frame 0 1 "Select Area")
  (render-list-into frame 1 0 (- h 1) (- w 1)
                    *rajiko-areas* *rajiko-area-nth*))

(define-frame main-screen (container-frame))
(define-children main-screen ()
  (main-content (simple-frame :render #'render-main-screen)))

(define-frame area-picker (container-frame))
(define-children area-picker ()
  (picker-content (simple-frame :render #'render-area-picker)))

(defun rajiko-cli ()
  (unwind-protect
       (%rajiko-cli)
    (when (and *rajiko* (eq (rajiko-status *rajiko*) :playing))
      (rajiko-pause *rajiko*))))

(defun %rajiko-cli ()
  (cl-setlocale:set-all-to-native)
  (load-config)
  (restore-config-state)
  (setf *rajiko* (make-instance 'rajiko :area *rajiko-area* :dummy t))
  (setf *status-message* "Ready")
  (with-screen ()
    (refresh)
    (display 'main-screen)
    (loop
      do
      (when (and *last-program-refresh*
                 (>= (- (get-universal-time) *last-program-refresh*) 1800))
        (refresh-programs)
        (refresh))
      (let ((key (read-key)))
        (case key
          ((#\Esc #\q)
           (rajiko-pause *rajiko*)
           (return))
          (#\Space
           (case *current-pane*
             (:program
              (let ((prog (nth *program-nth* *programs*)))
                (when prog
                  (handler-case
                      (progn
                        (rajiko-ts-play *rajiko* *rajiko-station*
                                        (getf prog :ft) (getf prog :to))
                        (setf *status-message* (format nil "Timeshift: ~A" (getf prog :title))))
                    (error (e)
                      (declare (ignore e))
                      (setf *status-message* "[Error] Playback failed")))))))
             (t
              (unless (and *rajiko* (rajiko-token *rajiko*))
                (handler-case
                    (progn
                      (setf *rajiko* (make-rajiko *rajiko-area*))
                      (setf *status-message* "Ready"))
                  (dex:http-request-failed (e)
                    (declare (ignore e))
                    (setf *status-message* "[Error] Auth failed"))
                  (error (e)
                    (declare (ignore e))
                    (setf *status-message* "[Error] Connection error"))))
              (when (and *rajiko* (rajiko-token *rajiko*))
                (let ((station-area (area-code-to-area-name
                                     (rajiko-station-area-id *rajiko-station*))))
                  (when (and station-area
                             (not (string= station-area *rajiko-area*)))
                    (setf *rajiko-area* station-area
                          *rajiko-area-nth* (or (position station-area *rajiko-areas*
                                                          :test #'equal)
                                                0))
                    (setf *rajiko* (make-rajiko *rajiko-area*))))
                (case (rajiko-status *rajiko*)
                  (:playing (rajiko-pause *rajiko*) (setf *status-message* "Paused"))
                  (t (rajiko-play *rajiko* *rajiko-station*)
                     (setf *status-message* "Playing..."))))))
          (#\p
           (when (eq *current-pane* :program)
             (prev-program)))
          (#\n
           (when (eq *current-pane* :program)
             (next-program)))
          (#\f
           (toggle-favorite))
          (#\g
           (refresh-programs))
          (#\r
           (when (eq *current-pane* :region)
             (setf *current-pane* :station)))
          (#\a
           (display 'area-picker)
           (loop
             do
             (refresh)
             (let ((area-key (read-key)))
               (case area-key
                 (:key-up
                  (setf *rajiko-area-nth*
                        (mod (1- *rajiko-area-nth*)
                             (length *rajiko-areas*))))
                 (:key-down
                  (setf *rajiko-area-nth*
                        (mod (1+ *rajiko-area-nth*)
                             (length *rajiko-areas*))))
                 (#\Return
                  (setf *rajiko-area*
                        (nth *rajiko-area-nth* *rajiko-areas*)
                        *rajiko* (make-instance 'rajiko :area *rajiko-area* :dummy t))
                  (return))
                 ((#\Esc #\q)
                  (return))
                 (t nil))))
           (display 'main-screen))
          (#\Return
           (when (eq *current-pane* :region)
             (select-station-region)
             (setf *current-pane* :station)))
          (:key-up
           (case *current-pane*
             (:station (prev-station))
             (:region (prev-station-region))
             (:program (prev-program))))
          (:key-down
           (case *current-pane*
             (:station (next-station))
             (:region (next-station-region))
             (:program (next-program))))
          (:key-left
           (case *current-pane*
             (:station
              (setf *station-regions* (station-regions *rajiko-station*)
                    *station-region-nth* 0
                    *current-pane* :region))
             (:region
              (setf *current-pane* :station))
             (:program
              (setf *current-pane* :station))))
          (:key-right
           (when (eq *current-pane* :station)
             (refresh-programs)
             (setf *current-pane* :program)))
          (t nil)))
      (refresh))))

;; (defun rajiko-cli-with-sly ()
;;   (slynk:create-server :dont-close t)
;;   (rajiko-cli))
