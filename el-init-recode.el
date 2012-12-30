(eval-when-compile
  (require 'cl))
(require 'el-init-util)

(defvar el-init:recode nil
  "alist (feature . plist (prop val))")

(defun el-init:get-recode (symbol &optional property)
  (el-init::awhen (cdr (assoc symbol el-init:recode))
    (if property
        (plist-get it property)
      it)))

(defun el-init:add-recode (symbol property value)
  (el-init::aif (assoc symbol el-init:recode)
      (setf (cdr (assoc symbol el-init:recode))
            (plist-put (cdr it) property value))
    (push (cons symbol (list property value)) el-init:recode)))

(defsetf el-init:get-recode (symbol &optional property) (value)
  (if property
      `(el-init:add-recode ,symbol ,property ,value)
    `(el-init::aif (assoc ,symbol el-init:recode)
         (setf (cdr (assoc ,symbol el-init:recode)) ,value)
       (push (cons ,symbol ,value) el-init:recode))))


(provide 'el-init-recode)
