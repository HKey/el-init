(require 'cl-lib)

(defvar el-init:load-function-list '(el-init:require/record-error))

(defalias 'el-init::require/original (symbol-function 'require))

(defun el-init::combine-require (function-list &optional compile)
  (if function-list
      (funcall (if compile #'byte-compile #'identity)
               `(lambda (feature &optional filename noerror)
                  (,(car function-list)
                   #',(el-init::combine-require (cdr function-list))
                   feature
                   filename
                   noerror)))
    #'el-init::require/original*))


(defvar el-init::next-fn nil)

(cl-defun el-init:next
    (&optional (feature feature) (filename filename) (noerror noerror))
  (declare (special feature filename noerror))
  (funcall el-init::next-fn feature filename noerror))

(defmacro el-init:define-require (name &rest body)
  (declare (indent 1))
  `(defun ,name (el-init::next-fn feature &optional filename noerror)
     ,@body))

(cl-defun el-init::require/original*
  (&optional (feature feature) (filename filename) (noerror noerror))
  (declare (special feature filename noerror))
  (el-init::require/original feature filename noerror))

(provide 'el-init-require)
