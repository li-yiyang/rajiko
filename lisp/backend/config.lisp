(in-package :rajiko.backend)

(defparameter *test-config-paths* nil
  "When non-nil, CONFIG-PATHS returns this list instead of the real paths.
Used by tests to avoid touching ~/.config/.")

(defun config-paths ()
  (or *test-config-paths*
      (list (merge-pathnames #p".config/rajiko/rajiko.lisp" (user-homedir-pathname))
            (merge-pathnames #p".rajiko.lisp" (user-homedir-pathname))
            (asdf:system-relative-pathname :rajiko "rajiko.lisp"))))

(defparameter *config* nil
  "Current config plist.  Set by LOAD-CONFIG.")

(defun load-config ()
  "Search config file paths, read the first found plist into *CONFIG*.
If no file is found, *CONFIG* stays nil.
On read errors, logs to stderr and sets *CONFIG* to nil."
  (setf *config*
        (loop for path in (config-paths)
              when (uiop:file-exists-p path)
                return (handler-case (uiop:read-file-form path)
                         (error (e)
                           (format *error-output*
                                   "~&;; Error reading config ~A: ~A~%" path e)
                           nil))
              finally (return nil))))

(defun save-config ()
  "Write *CONFIG* as a plist to the first config path."
  (let ((path (first (config-paths))))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (prin1 *config* stream)
      (terpri stream))))

(defun config-path ()
  "Return the first existing config path, or the first writable path."
  (or (find-if #'uiop:file-exists-p (config-paths))
      (first (config-paths))))

(defun merge-config (plist)
  "Merge override PLIST into *CONFIG*, overwriting existing keys."
  (loop for (key value) on plist by #'cddr
        do (setf (getf *config* key) value))
  *config*)
