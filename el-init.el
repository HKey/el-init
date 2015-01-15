;;; el-init.el --- A loader inspired by init-loader -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Hiroki YAMAKAWA

;; Author: Hiroki YAMAKAWA <s06139@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((cl-lib "0.5") (anaphora "1.0.0"))
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'anaphora)

;;;; Utilities

(defun el-init::listify (object)
  (if (listp object)
      object
    (list object)))

(defun el-init::file-name-to-symbol (file-name)
  (intern
   (file-name-nondirectory
    (file-name-sans-extension file-name))))

;;;###autoload
(defun el-init:provide ()
  (provide
   (el-init::file-name-to-symbol
    (or load-file-name
        (buffer-file-name)))))



;;;; Record

(defvar el-init:record nil
  "alist (feature . plist (prop val))")

(defun el-init:get-feature-record (feature)
  (cdr (assoc feature el-init:record)))

(defun el-init:get-record (feature property)
  (plist-get (el-init:get-feature-record feature) property))

(defun el-init:add-record (feature property value)
  (aif (assoc feature el-init:record)
      (setf (cdr (assoc feature el-init:record))
            (plist-put (cdr it) property value))
    (push (cons feature (list property value)) el-init:record)))

(with-no-warnings
  ;; Since Emacs 24.3, `defsetf' has been obsolete.
  (if (fboundp 'gv-define-setter)
      (gv-define-setter el-init:get-record (value feature property)
        `(el-init:add-record ,feature ,property ,value))
    (defsetf el-init:get-record (feature property) (value)
      `(el-init:add-record ,feature ,property ,value))))



;;;; Alert

(defvar el-init:alert-buffer-name "*el-init alerts*")

(defvar el-init:alert-function #'el-init:default-alert)

(defvar el-init:alert-enable-p t)

(defun el-init:alert (message)
  (when el-init:alert-enable-p
    (funcall el-init:alert-function message)))

(defun el-init:default-alert (message)
  (display-warning 'el-init message nil el-init:alert-buffer-name))



;;;; Require Wrapper

(defvar el-init:wrappers '(el-init:require/record-error))

(define-obsolete-variable-alias
  'el-init:load-function-list
  'el-init:wrappers
  "0.1.0")

(defvar el-init::require-wrappers nil)

(defun el-init:next (feature &optional filename noerror)
  (let ((fn (car el-init::require-wrappers))
        (el-init::require-wrappers (cdr el-init::require-wrappers)))
    (funcall fn feature filename noerror)))

(defmacro el-init:define-require (name &rest body)
  (declare (indent 1))
  (warn "`el-init:define-require' is obsolete.")
  (let ((next (cl-gensym)))
    `(defun ,name (feature &optional filename noerror)
       (cl-labels ((,next () (el-init:next feature filename noerror)))
         (cl-macrolet ((el-init:next () '(,next)))
           ,@body)))))



;;;; Require Wrapper Definitions

;; benchmark
(defun el-init:require/benchmark (feature &optional filename noerror)
  (let ((result (benchmark-run (el-init:next feature filename noerror))))
    (unless (el-init:get-record feature 'el-init:require/benchmark)
      (el-init:add-record feature 'el-init:require/benchmark result))))


;; record error
(defun el-init:require/record-error (feature &optional filename noerror)
  (condition-case e
      (el-init:next feature filename noerror)
    (error (el-init:add-record feature 'el-init:require/record-error e)
           (el-init:alert (error-message-string e)))))


;; ignore error
(defun el-init:require/ignore-errors (feature &optional filename noerror)
  (ignore-errors (el-init:next feature filename noerror)))


;; eval-after-load
(defun el-init:require/record-eval-after-load-error
    (feature &optional filename noerror)
  (let* ((original (symbol-function 'eval-after-load))
         (e (cl-gensym))
         (fn (lambda (file form)
               (funcall original
                        file
                        `(condition-case ,e
                             ,(if (functionp form) `(funcall ,form) form)
                           (error
                            (push (list :file ,file :error ,e)
                                  (el-init:get-record
                                   ',feature
                                   'el-init:require/record-eval-after-load-error))
                            (el-init:alert (error-message-string ,e))))))))
    (cl-letf (((symbol-function 'eval-after-load) fn))
      (el-init:next feature filename noerror))))


;; switching by the system (from init-loader)

(defvar el-init:meadow-regexp       "^init-meadow-")
(defvar el-init:carbon-emacs-regexp "^init-carbon-emacs-")
(defvar el-init:cocoa-emacs-regexp  "^init-cocoa-emacs-")
(defvar el-init:nw-regexp           "^init-nw-")

(defun el-init:meadowp        () (featurep 'meadow))
(defun el-init:carbon-emacs-p () (featurep 'carbon-emacs-package))
(defun el-init:cocoa-emacs-p  () (eq window-system 'ns))
(defun el-init:nwp            () (null window-system))

(defvar el-init:mac-regexp     "^init-mac-")
(defvar el-init:windows-regexp "^init-windows-")
(defvar el-init:linux-regexp   "^init-linux-")
(defvar el-init:freebsd-regexp "^init-freebsd-")

(defun el-init:macp     () (string-match-p "apple-darwin" system-configuration))
(defun el-init:windowsp () (string-match-p "mingw" system-configuration))
(defun el-init:linuxp   () (string-match-p "linux" system-configuration))
(defun el-init:freebsdp () (string-match-p "freebsd" system-configuration))

(defvar el-init:system-case-alist
  (list (cons el-init:meadow-regexp       #'el-init:meadowp)
        (cons el-init:carbon-emacs-regexp #'el-init:carbon-emacs-p)
        (cons el-init:cocoa-emacs-regexp  #'el-init:cocoa-emacs-p)
        (cons el-init:nw-regexp           #'el-init:nwp)
        (cons el-init:mac-regexp          #'el-init:macp)
        (cons el-init:windows-regexp      #'el-init:windowsp)
        (cons el-init:linux-regexp        #'el-init:linuxp)
        (cons el-init:freebsd-regexp      #'el-init:freebsdp)))

(defun el-init:require/system-case (feature &optional filename noerror)
  (let ((match (cl-loop for (regexp . predicate) in el-init:system-case-alist
                        when (string-match-p regexp (symbol-name feature))
                        return predicate)))
    (when (or (not match) (funcall match))
      (el-init:next feature filename noerror))))


;; old .elc files

(defun el-init::ensure-string (object)
  (format "%s" object))

(defun el-init::file-name-el (filename)
  (concat (file-name-sans-extension filename) ".el"))

(defun el-init::file-name-elc (filename)
  (concat (file-name-sans-extension filename) ".elc"))

(defun el-init::old-library-p (library)
  (awhen (locate-library (el-init::ensure-string library))
    (let ((el (el-init::file-name-el it))
          (elc (el-init::file-name-elc it)))
      (when (file-newer-than-file-p el elc)
        elc))))

(defun el-init::byte-compile-library (library)
  (awhen (locate-library (el-init::ensure-string library))
    (ignore-errors
      (byte-compile-file
       (el-init::file-name-el it)))))

(defun el-init:require/record-old-library (feature &optional filename noerror)
  (el-init:add-record feature
                      'el-init:require/record-old-library
                      (and (el-init::old-library-p (or filename feature)) t))
  (el-init:next feature filename noerror))

(defun el-init:require/compile-old-library (feature &optional filename noerror)
  (when (el-init::old-library-p (or filename feature))
    (let ((result (el-init::byte-compile-library (or filename feature))))
      (el-init:add-record feature
                          'el-init:require/compile-old-library
                          result)))
  (el-init:next feature filename noerror))


;; lazy loading

(defvar el-init:lazy-init-regexp "^init-lazy-\\(.+\\)$")

(defun el-init:require/lazy (feature &optional filename noerror)
  (save-match-data
    (if (string-match el-init:lazy-init-regexp (symbol-name feature))
        (eval-after-load (match-string 1 (symbol-name feature))
          `(let ((el-init::require-wrappers ',el-init::require-wrappers))
             (el-init:next ',feature ',filename ',noerror)))
      (el-init:next feature filename noerror))))



;;;; Loader

(defvar el-init:load-file-regexp "\\.elc?$")
(defvar el-init:subdirectories '("."))
(defvar el-init:override-only-init-files-p t)
(defvar el-init:overridep t)
(defvar el-init:before-load-hook nil)
(defvar el-init:after-load-hook nil)

(define-obsolete-variable-alias
  'el-init:load-directory-list
  'el-init:subdirectories
  "0.1.0")

(defun el-init::path-concat (&rest paths)
  (expand-file-name
   (cl-reduce (lambda (x y) (concat (file-name-as-directory x) y))
              paths)))

(defun el-init::list-subdirectories (directory)
  (cl-remove-if
   (lambda (x)
     (string-match-p (rx bos (or ".." ".") eos)
                     (file-name-nondirectory x)))
   (cl-remove-if-not #'file-directory-p
                     (directory-files directory t))))

(defun el-init::list-all-directories (directory)
  (cons directory
        (cl-mapcan #'el-init::list-all-directories
                   (el-init::list-subdirectories directory))))

(defun el-init::expand-directory-list (directory subdirectories)
  (cl-loop for dir in (mapcar #'el-init::listify subdirectories)
           when (file-directory-p (el-init::path-concat directory (cl-first dir)))
           append (if (cl-second dir)
                      (el-init::list-all-directories
                       (el-init::path-concat directory (cl-first dir)))
                    (list (el-init::path-concat directory (cl-first dir))))))

(defun el-init::target-files (directory subdirectories)
  (cl-loop for d in (el-init::expand-directory-list directory subdirectories)
           append (directory-files d nil el-init:load-file-regexp)))

(defun el-init::make-overridden-require (original
                                         wrappers
                                         init-features
                                         only-init-files)
  (lambda (feature &optional filename noerror)
    (if (or (not only-init-files)
            (memq feature init-features))
        (let ((el-init::require-wrappers wrappers))
          (el-init:next feature filename noerror))
      (funcall original feature filename noerror))))

;;;###autoload
(cl-defun el-init:load (directory
                        &key
                        (subdirectories el-init:subdirectories)
                        (wrappers el-init:wrappers)
                        (override-only-init-files el-init:override-only-init-files-p)
                        (override el-init:overridep)
                        ;; for compatibility with 0.0.9
                        (directory-list nil directory-list-flag)
                        (function-list nil function-list-flag))
  ;; for compatibility with 0.0.9
  (when directory-list-flag
    (setq subdirectories directory-list)
    (warn "`:directory-list' parameter of `el-init:load' is obsolete; use `:subdirectories' instead."))
  (when function-list-flag
    (setq wrappers function-list)
    (warn "`:function-list' parameter of `el-init:load' is obsolete; use `:wrappers' instead."))

  (run-hooks 'el-init:before-load-hook)
  (cl-dolist (dir (el-init::expand-directory-list directory subdirectories))
    (add-to-list 'load-path dir))
  (condition-case e
      (let* ((original
              (if override (symbol-function 'require) 'require))
             (el-init::require-wrappers
              (append wrappers (list original)))
             (init-features
              (cl-remove-duplicates
               (mapcar #'el-init::file-name-to-symbol
                       (el-init::target-files directory subdirectories))))
             (overridden-require
              (el-init::make-overridden-require original
                                                el-init::require-wrappers
                                                init-features
                                                override-only-init-files)))
        (cl-dolist (feature init-features)
          (if override
              (cl-letf (((symbol-function 'require) overridden-require))
                (el-init:next feature))
            (el-init:next feature))))
    (error (el-init:alert (error-message-string e))))
  (run-hooks 'el-init:after-load-hook))



(provide 'el-init)
;;; el-init.el ends here
