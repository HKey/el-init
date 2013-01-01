(eval-when-compile
  (require 'cl))
(require 'el-init-util)

(defvar el-init:record nil
  "alist (feature . plist (prop val))")

(defun el-init:get-record (symbol &optional property)
  (el-init::awhen (cdr (assoc symbol el-init:record))
    (if property
        (plist-get it property)
      it)))

(defun el-init:add-record (symbol property value)
  (el-init::aif (assoc symbol el-init:record)
      (setf (cdr (assoc symbol el-init:record))
            (plist-put (cdr it) property value))
    (push (cons symbol (list property value)) el-init:record)))

(defsetf el-init:get-record (symbol &optional property) (value)
  (if property
      `(el-init:add-record ,symbol ,property ,value)
    `(el-init::aif (assoc ,symbol el-init:record)
         (setf (cdr (assoc ,symbol el-init:record)) ,value)
       (push (cons ,symbol ,value) el-init:record))))


(provide 'el-init-record)
