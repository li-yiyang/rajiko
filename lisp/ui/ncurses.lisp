(in-package :rajiko.ui)

;;; cl-tui's COLOR-PAIR class declares FG/BG slots as FIXNUM, but the rest of
;;; the library passes COLOR objects through it. Normalize the slot types so the
;;; documented API works in this image without touching the cl-tui source.
;;; SBCL-only: on other Lisps the slot types stay FIXNUM and color init fails,
;;; which the guarded (with-screen) retry below degrades gracefully to no-color.
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-class 'cl-tui::color-pair nil)
    (let ((changed nil))
      (dolist (slot-name '(cl-tui::fg cl-tui::bg))
        (let ((slot (find slot-name (sb-mop:class-direct-slots (find-class 'cl-tui::color-pair))
                          :key #'sb-mop:slot-definition-name)))
          (when (and slot (not (eq t (sb-mop:slot-definition-type slot))))
            (setf (sb-mop:slot-definition-type slot) t)
            (setf changed t))))
      (when changed
        (reinitialize-instance (find-class 'cl-tui::color-pair))))))

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

(defparameter *rajiko-station-nth* 0)

(defparameter *rajiko-station* nil)

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

(defparameter *programs-cache* (make-hash-table :test 'equal)
  "Cache of (station-id . date-str) -> (programs . fetched-universal-time).")
(defconstant +program-cache-ttl+ 1800
  "Seconds before a cached program list is considered stale.")

;;; ---------------------------------------------------------------------------
;;; Color palette and rendering helpers
;;; ---------------------------------------------------------------------------

(defparameter *colors-available* nil
  "Whether the terminal supports color (set at screen init).")

;; Restrained dark-theatre palette.  Values are in the 0-1000 range used by
;; cl-tui's COLOR function.  Black background keeps the UI quiet; amber and
;; mint are the only accents.
(defvar *col-bg*       (color   0   0   0) "Black background.")
(defvar *col-text*     (color 900 900 850) "Warm off-white body text.")
(defvar *col-header*   (color 1000 1000 1000) "Bright white for headers.")
(defvar *col-favorite* (color 1000 700   0) "Amber accent for favorites.")
(defvar *col-now*      (color   0 1000 500) "Mint green for now-playing.")
(defvar *col-dim*      (color 500 500 500) "Grey for borders/hints.")
(defvar *col-region*   (color 400 600 800) "Cool grey-blue for region context.")

(defvar *pair-text*    (color-pair *col-text*     *col-bg*))
(defvar *pair-header*  (color-pair *col-header*   *col-bg*))
(defvar *pair-favorite* (color-pair *col-favorite* *col-bg*))
(defvar *pair-now*     (color-pair *col-now*      *col-bg*))
(defvar *pair-dim*     (color-pair *col-dim*      *col-bg*))
(defvar *pair-region*  (color-pair *col-region*   *col-bg*))

(defmacro with-header-style (frame &body body)
  "Header emphasis: bright white + bold, or bold alone without colors."
  `(if *colors-available*
       (with-attributes (:bold (:color *pair-header*)) ,frame ,@body)
       (with-attributes (:bold) ,frame ,@body)))

(defmacro with-favorite-style (frame &body body)
  "Favorite emphasis: amber, or bold without colors."
  `(if *colors-available*
       (with-attributes ((:color *pair-favorite*)) ,frame ,@body)
       (with-attributes (:bold) ,frame ,@body)))

(defmacro with-now-style (frame &body body)
  "Now-playing emphasis: mint green + bold, or bold without colors."
  `(if *colors-available*
       (with-attributes (:bold (:color *pair-now*)) ,frame ,@body)
       (with-attributes (:bold) ,frame ,@body)))

(defmacro with-dim-style (frame &body body)
  "Subtle/dim emphasis: grey + dim, or dim without colors."
  `(if *colors-available*
       (with-attributes (:dim (:color *pair-dim*)) ,frame ,@body)
       (with-attributes (:dim) ,frame ,@body)))

(defmacro with-text-style (frame &body body)
  "Body text style: warm off-white, or plain without colors."
  `(if *colors-available*
       (with-attributes ((:color *pair-text*)) ,frame ,@body)
       (progn ,@body)))

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
        (refresh-programs)))))

(defun program-cache-key (station date-str)
  (cons (rajiko-station-id station) date-str))

(defun get-cached-programs (station date-str)
  (let ((entry (gethash (program-cache-key station date-str) *programs-cache*)))
    (when (and entry
               (<= (- (get-universal-time) (cdr entry)) +program-cache-ttl+))
      (car entry))))

(defun store-cached-programs (station date-str programs)
  (setf (gethash (program-cache-key station date-str) *programs-cache*)
        (cons programs (get-universal-time))))

(defun clear-program-cache ()
  (clrhash *programs-cache*))

(defun program-airing-p (p now-unix)
  "True when program P is currently airing (ft <= now < to). Single source of truth
for airing semantics, shared by find-now-program-index and the render path."
  (let ((ft (getf p :ft)) (to (getf p :to)))
    (and ft to
         (<= (yyyymmddhhmmss-to-unix ft) now-unix)
         (< now-unix (yyyymmddhhmmss-to-unix to)))))

(defun find-now-program-index (programs now-unix)
  "Index of the currently-airing program (ft <= now < to), else the next upcoming, else last; 0 if empty."
  (if (null programs)
      0
      (or (position-if (lambda (p) (program-airing-p p now-unix)) programs)
          (position-if
           (lambda (p)
             (let ((ft (getf p :ft)))
               (and ft (< now-unix (yyyymmddhhmmss-to-unix ft)))))
           programs)
          (1- (length programs)))))

(defun refresh-programs (&key force)
  (unless *rajiko-station*
    (setf *programs* nil
          *status-message* "No station")
    (return-from refresh-programs))
  (let* ((date-str (if (zerop *program-date-offset*)
                       (rajiko.backend::todays-date-string)
                       (multiple-value-bind (y m d)
                           (decode-universal-time
                            (+ (get-universal-time)
                               (* *program-date-offset* 86400)))
                         (format nil "~4,'0D~2,'0D~2,'0D" y m d)))))
    (setf *programs*
          (or (and (not force) (get-cached-programs *rajiko-station* date-str))
              (let ((fetched
                     (if (zerop *program-date-offset*)
                         (program-today *rajiko-station*)
                         (timeshift-programs *rajiko-station* *rajiko-area* date-str))))
                (when fetched
                  (store-cached-programs *rajiko-station* date-str fetched))
                fetched)))
    (setf *program-nth* (find-now-program-index *programs* (get-unix-time))
          *last-program-refresh* (get-universal-time))
    (unless *programs*
      (setf *status-message* "[No programs / fetch failed]"))))

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
  (if *rajiko-station*
      (setf *station-regions* (station-regions *rajiko-station*)
            *station-region-nth* 0)
      ;; First run / no last-station in config: fall back to the first
      ;; sorted station so the render path always has a non-nil station.
      (let ((stations (sorted-stations)))
        (when stations
          (setf *rajiko-station-nth* 0
                *rajiko-station* (cdr (first stations))
                *station-regions* (station-regions *rajiko-station*)
                *station-region-nth* 0)))))

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
  ;; The "+" sits at column LEFT-W so it aligns with the "|" separators in the
  ;; column-header row and the vertical pane separator below it.
  (put-text frame y 0
            (fit-text
             (concatenate 'string
                          (make-string left-w :initial-element #\-)
                          "+"
                          (make-string (- total-w left-w 1) :initial-element #\-))
             total-w)))

(defun render-list-into (frame offset-y offset-x max-h max-w items selected-idx
                         &key (render-fn #'identity) (attr-fn nil))
  "Render ITEMS into FRAME.

RENDER-FN turns an item into a string.  ATTR-FN, if supplied, returns a style
keyword (:FAVORITE or :NOW) for rows that need extra emphasis; the selected
row always gets :REVERSE on top.  Default ATTR-FN is NIL for backward
compatibility."
  (declare (ignore max-w))
  (let* ((num-items (length items))
         (half-h (truncate max-h 2))
         (scroll-pos (min (max (- selected-idx half-h) 0)
                          (max (- num-items max-h) 0))))
    (loop for line from 0 below max-h
          for idx from scroll-pos
          when (>= idx num-items) return nil
          do (let* ((item (nth idx items))
                    (text (funcall render-fn item))
                    (style (and attr-fn (funcall attr-fn item))))
               (if (= idx selected-idx)
                   (if (eq style :favorite)
                       (with-attributes (:reverse) frame
                         (with-favorite-style frame
                           (put-text frame (+ offset-y line) offset-x "> ~A" text)))
                       (if (eq style :now)
                           (with-attributes (:reverse) frame
                             (with-now-style frame
                               (put-text frame (+ offset-y line) offset-x "> ~A" text)))
                           (with-attributes (:reverse) frame
                             (put-text frame (+ offset-y line) offset-x "> ~A" text))))
                   (case style
                     (:favorite
                      (with-favorite-style frame
                        (put-text frame (+ offset-y line) offset-x "  ~A" text)))
                     (:now
                      (with-now-style frame
                        (put-text frame (+ offset-y line) offset-x "  ~A" text)))
                     (t (put-text frame (+ offset-y line) offset-x "  ~A" text))))))))

(defun render-key-hints (frame y w)
  (declare (ignore w))
  (with-header-style frame
    (ecase *current-pane*
      (:station
       (put-text frame y 1
                 "Station [Up/Dn]  Region [Left]  Program [Right]  Play [Space]  Fave [f]  Quit [q]"))
      (:region
       (put-text frame y 1
                 "Region [Up/Dn]  Select [Enter]  Back [r/Esc]  Play [Space]  Quit [q]"))
      (:program
       (put-text frame y 1
                 "Program [Up/Dn]  Prev [p]  Next [n]  Play [Space]  Refresh [g]  Back [Left]  Quit [q]")))))

(defun render-main-screen (&key frame h w)
  (let* ((left-w (min (truncate w 3) 30))
         (right-w (- w left-w 1))
         (content-top 5)
         (content-bot (- h 3))
         (content-h (- content-bot content-top)))
    ;; Top border and key hints.
    (with-dim-style frame
      (put-text frame 0 0
                (format nil "+~A" (make-string (- w 2) :initial-element #\-))))
    (render-key-hints frame 1 w)
    (with-dim-style frame
      (draw-hsep frame 2 w))
    ;; Column headers, aligned to the pane boundary at LEFT-W.
    (let* ((left-label (if (eq *current-pane* :region) "Regions" "Stations"))
           (left-items (if (eq *current-pane* :region)
                           *station-regions*
                           (sorted-stations)))
           (left-text (format nil "~A (~A)" left-label (length left-items)))
           (right-text (format nil "Programs -- ~A"
                               (rajiko-station-ascii-name *rajiko-station*))))
      (with-header-style frame
        (put-text frame 3 1 (fit-text left-text (max 0 (- left-w 2))))
        (put-char frame 3 left-w #\|)
        (put-text frame 3 (1+ left-w) (fit-text right-text (max 0 (- right-w 2))))))
    (with-dim-style frame
      (draw-pane-top frame 4 left-w w))
    ;; Left pane: regions or stations.
    (if (eq *current-pane* :region)
        (render-list-into frame content-top 1 content-h (1- left-w)
                          *station-regions* *station-region-nth*
                          :render-fn (lambda (r)
                                       (rajiko-region-ascii-name r)))
        (render-list-into frame content-top 1 content-h (1- left-w)
                          (sorted-stations) *rajiko-station-nth*
                          :render-fn (lambda (s)
                                       (let ((fav-p (member (car s)
                                                            (getf *config* :favorites)
                                                            :test #'equal)))
                                         (format nil "~A~A"
                                                 (if fav-p "* " "  ")
                                                 (rajiko-station-ascii-name (cdr s)))))
                          :attr-fn (lambda (s)
                                     (when (member (car s)
                                                   (getf *config* :favorites)
                                                   :test #'equal)
                                       :favorite))))
    ;; Vertical pane separator.
    (with-dim-style frame
      (loop for y from content-top below content-bot
            do (put-char frame y left-w #\|)))
    ;; Right pane: programs.
    (render-list-into frame content-top (+ left-w 1) content-h (1- right-w)
                      *programs* *program-nth*
                      :render-fn (lambda (p)
                                   (let ((now (get-unix-time)))
                                     (format nil "~A ~A ~A"
                                             (if (program-airing-p p now) "[now]" "     ")
                                             (format-timestamp (getf p :ft))
                                             (getf p :title))))
                      :attr-fn (lambda (p)
                                 (when (program-airing-p p (get-unix-time))
                                   :now)))
    ;; Bottom separator and status bar.
    (with-dim-style frame
      (draw-hsep frame (- h 3) w))
    (let ((status (if *rajiko* (rajiko-status *rajiko*) :stopped)))
      (with-text-style frame
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
                   (- w 1)))))
    (with-dim-style frame
      (put-text frame (1- h) 0
                (format nil "+~A" (make-string (- w 2) :initial-element #\-))))))

(defun render-area-picker (&key frame h w)
  (with-header-style frame
    (put-text frame 0 1 "Select Area"))
  (render-list-into frame 1 0 (- h 1) (- w 1)
                    *rajiko-areas* *rajiko-area-nth*))

(define-frame main-screen (container-frame))
(define-children main-screen ()
  (main-content (simple-frame :render #'render-main-screen)))

(define-frame area-picker (container-frame))
(define-children area-picker ()
  (picker-content (simple-frame :render #'render-area-picker)))

(defun handle-live-play ()
  "Auth (if needed), switch area to the station's area, then toggle play/pause. Never crashes the TUI."
  (unless (and *rajiko* (rajiko-token *rajiko*))
     (handler-case
         (progn
           (setf *rajiko* (make-rajiko *rajiko-area*)))
       (dex:http-request-failed ()
         (setf *status-message* "[Error] Auth failed"))
       (error ()
         (setf *status-message* "[Error] Connection error"))))
  (when (and *rajiko* (rajiko-token *rajiko*))
    (handler-case
        (progn
          (let ((station-area (area-code-to-area-name
                               (rajiko-station-area-id *rajiko-station*))))
            (when (and station-area (not (string= station-area *rajiko-area*)))
              (setf *rajiko-area* station-area
                    *rajiko-area-nth* (or (position station-area *rajiko-areas*
                                                    :test #'equal)
                                         0))
              (setf *rajiko* (make-rajiko *rajiko-area*))))
          (case (rajiko-status *rajiko*)
            (:playing (rajiko-pause *rajiko*) (setf *status-message* "Paused"))
            (t (rajiko-play *rajiko* *rajiko-station*)
               (setf *status-message* "Playing..."))))
      (dex:http-request-failed ()
        (setf *status-message* "[Error] Auth failed"))
      (error ()
        (setf *status-message* "[Error] Playback failed")))))

(defun run-area-picker ()
  "Show the area-picker overlay modal and handle its keys until Return/Esc/q."
  (display 'area-picker)
  (loop
    do
    (refresh)
    (let ((area-key (read-key)))
      (case area-key
        (:key-up
         (setf *rajiko-area-nth*
               (mod (1- *rajiko-area-nth*) (length *rajiko-areas*))))
        (:key-down
         (setf *rajiko-area-nth*
               (mod (1+ *rajiko-area-nth*) (length *rajiko-areas*))))
        (#\Return
         (setf *rajiko-area* (nth *rajiko-area-nth* *rajiko-areas*)
               *rajiko* (make-instance 'rajiko :area *rajiko-area* :dummy t))
         (return))
        ((#\Esc #\q) (return))
        (t nil))))
  (display 'main-screen))

(defun rajiko-cli ()
  (unwind-protect
       (%rajiko-cli)
    (when (and *rajiko* (eq (rajiko-status *rajiko*) :playing))
      (rajiko-pause *rajiko*))
    (handler-case
        (when *rajiko-station*
          (setf (getf *config* :last-station) (rajiko-station-id *rajiko-station*))
          (setf (getf *config* :last-area) *rajiko-area*)
          (setf (getf *config* :last-region) *rajiko-region*)
          (save-config))
      (error () nil))))

(defun %rajiko-cli ()
  (cl-setlocale:set-all-to-native)
  (restore-config-state)
  (refresh-programs)
  (setf *rajiko* (make-instance 'rajiko :area *rajiko-area* :dummy t))
  (setf *status-message* "Ready")
  (flet ((run-tui ()
           (refresh)
           (display 'main-screen)
           (loop
             do
             (when (and *last-program-refresh*
                        (>= (- (get-universal-time) *last-program-refresh*) 1800))
               (refresh-programs :force t)
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
                             (setf *status-message* "[Error] Playback failed"))))))
                    (t (handle-live-play))))
                 (#\p
                  (prev-program))
                 (#\n
                  (next-program))
                 (#\f
                  (toggle-favorite))
                 (#\g
                  (refresh-programs :force t))
                 (#\r
                  (when (eq *current-pane* :region)
                    (setf *current-pane* :station)))
                 (#\a
                  (run-area-picker))
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
    (handler-case
        (with-screen (:colors)
          (setf *colors-available* t)
          (run-tui))
      (error ()
        (with-screen ()
          (setf *colors-available* nil)
          (run-tui))))))

;; (defun rajiko-cli-with-sly ()
;;   (slynk:create-server :dont-close t)
;;   (rajiko-cli))
