(require 'cl)
(require 'el-init-record)
(require 'el-init-require)


;; benchmark
(el-init:define-require el-init:require/benchmark
  (let ((result (benchmark-run (el-init:next))))
    (unless (el-init:get-record feature :bench)
      (el-init:add-record feature :bench result))))


;; record error
(el-init:define-require el-init:require/record-error
  (condition-case e
      (el-init:next)
    (error (el-init:add-record feature
                               :error
                               (error-message-string e)))))


;; ignore error
(el-init:define-require el-init:require/ignore-errors
  (ignore-errors (el-init:next)))


;; eval-after-load
(defalias 'el-init::eval-after-load/original (symbol-function 'eval-after-load))

(el-init:define-require el-init:require/record-eval-after-load-error
  (flet ((eval-after-load (file form)
           (el-init::eval-after-load/original
            file
            (let ((e (gensym)))
              `(condition-case ,e
                   ,form
                 (error
                  (el-init:add-record ,feature
                                      :eval-after-load-error
                                      (error-message-string ,e))))))))
    (el-init:next)))


;; システムによる分岐 init-loader から
(defvar el-init:meadow-regexp       "^init-meadow-")
(defvar el-init:carbon-emacs-regexp "^init-carbon-emacs-")
(defvar el-init:cocoa-emacs-regexp  "^init-cocoa-emacs-")
(defvar el-init:nw-regexp           "^init-nw-")

(defun el-init:meadowp        () (featurep 'meadow))
(defun el-init:carbon-emacs-p () (featurep 'carbon-emacs-package))
(defun el-init:cocoa-emacs-p  () (eq window-system 'ns))
(defun el-init:nwp            () (null window-system))

;; osによる分岐
(defvar el-init:mac-regexp     "^init-mac-")
(defvar el-init:windows-regexp "^init-windows-")
(defvar el-init:linux-regexp   "^init-linux-")
(defvar el-init:freebsd-regexp "^init-freebsd-")

(defun el-init:macp     () (string-match-p "apple-darwin" system-configuration))
(defun el-init:windowsp () (string-match-p "mingw" system-configuration))
(defun el-init:linuxp   () (string-match-p "linux" system-configuration))
(defun el-init:freebsdp () (string-match-p "freebsd" system-configuration))

(defvar el-init:system-case-alist
  (list (cons el-init:meadow-regexp       #'el-init:meadow-p)
        (cons el-init:carbon-emacs-regexp #'el-init:carbon-emacs-p)
        (cons el-init:cocoa-emacs-regexp  #'el-init:cocoa-emacs-p)
        (cons el-init:nw-regexp           #'el-init:nw-p)
        (cons el-init:mac-regexp          #'el-init:macp)
        (cons el-init:windows-regexp      #'el-init:windowsp)
        (cons el-init:linux-regexp        #'el-init:linuxp)
        (cons el-init:freebsd-regexp      #'el-init:freebsdp)))

(el-init:define-require el-init:require/system-case
  (let ((match (loop for (regexp . predicate) in el-init:system-case-alist
                     when (string-match-p regexp (symbol-name feature))
                     return predicate)))
    (when (or (not match) (funcall match))
      (el-init:next))))


;; 古い elc ファイルの検出

(defun el-init::ensure-string (object)
  (format "%s" object))

(defun el-init::file-name-el (filename)
  (concat (file-name-sans-extension filename) ".el"))

(defun el-init::file-name-elc (filename)
  (concat (file-name-sans-extension filename) ".elc"))

(defun el-init::old-library-p (library)
  (el-init::awhen (locate-library (el-init::ensure-string library))
    (let ((el (el-init::file-name-el it))
          (elc (el-init::file-name-elc it)))
      (when (file-newer-than-file-p el elc)
        elc))))

(defun el-init::byte-compile-library (library)
  (el-init::awhen (locate-library (el-init::ensure-string library))
    (ignore-errors
      (byte-compile-file
       (el-init::file-name-el it)))))

(el-init:define-require el-init:require/record-old-library
  (el-init::awhen (el-init::old-library-p (or filename feature))
    (el-init:add-record feature :old-library it))
  (el-init:next))

;; 古い elc ファイルのバイトコンパイル

(el-init:define-require el-init:require/compile-old-library
  (when (el-init::old-library-p (or filename feature))
    (let ((result (el-init::byte-compile-library (or filename feature))))
      (el-init:add-record feature
                          :compile-old-library
                          (if result :success :failure))))
  (el-init:next))


(provide 'el-init-require-definitions)
