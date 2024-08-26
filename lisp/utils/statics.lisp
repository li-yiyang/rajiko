(in-package :rajiko.utils)

(defun %find-statics (component-description)
  (asdf:system-relative-pathname
   :rajiko
   (join "/" (mapcar (lambda (comp)
		       (if (stringp comp)
			   comp
			   (format nil "~(~A~)" comp)))
		     component-description))))

(defmacro find-statics (&rest component-descriptions)
  "Get system relative statics files. "
  `(%find-statics ',component-descriptions))
