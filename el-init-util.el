(require 'bytecomp)

(defmacro el-init::aif (test then &rest else)
  (declare (indent 2))
  `(let ((it ,test))
     (if it
         ,then
       ,@else)))

(defmacro el-init::awhen (test &rest body)
  (declare (indent 1))
  `(let ((it ,test))
     (when it
       ,@body)))

(defun el-init::listify (object)
  (if (listp object)
      object
    (list object)))

(defun el-init::file-name->symbol (file-name)
  (intern
   (file-name-nondirectory
    (file-name-sans-extension file-name))))

(defun el-init::current-file-name ()
  (or load-file-name
      byte-compile-current-file
      (buffer-file-name)))

(defmacro el-init:provide ()
  `(provide ',(el-init::file-name->symbol (el-init::current-file-name))))


(provide 'el-init-util)
