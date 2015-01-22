;;; el-init.el --- A loader inspired by init-loader -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Hiroki YAMAKAWA

;; Author: Hiroki YAMAKAWA <s06139@gmail.com>
;; Version: 0.1.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (anaphora "1.0.0"))
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

;; el-init loads split configuration files with `require' instead of `load'.

;; * Basic Usage

;; For example, there are configuration files like below,

;;     ~/.emacs.d/inits
;;     ├── ext/
;;     │   └── init-helm.el
;;     ├── lang/
;;     │   ├── init-emacs-lisp.el
;;     │   └── init-javascript.el
;;     └── init-package.el

;; you load them with following code.

;;     (require 'el-init)

;;     (el-init-load "~/.emacs.d/inits"
;;                   :subdirectories '("." "ext" "lang"))

;;; Code:

(require 'cl-lib)
(require 'anaphora)

;;;; Utilities

(defun el-init--listify (object)
  (if (listp object)
      object
    (list object)))

(defun el-init--file-name-to-symbol (file-name)
  (intern
   (file-name-nondirectory
    (file-name-sans-extension file-name))))

;;;###autoload
(defun el-init-provide ()
  "Call `provide' with the file name as a feature name."
  (provide
   (el-init--file-name-to-symbol
    (or load-file-name
        (buffer-file-name)))))



;;;; Record

(defvar el-init-record nil
  "A record which is used by `require' wrapper functions.
This is a list of alists.
The alist consists of a feature name and a plist.
The plist consists of wrapper names and values.

Example:
  ((init-foo wrapper-foo value-foo-1 wrapper-bar value-bar-1 ...)
   (init-bar wrapper-foo value-foo-2 wrapper-bar value-bar-2 ...) ...)")

(defun el-init-get-feature-record (feature)
  "Get a record plist of FEATURE from `el-init-record'."
  (cdr (assoc feature el-init-record)))

(defun el-init-get-record (feature property)
  "Get a record value of PROPERTY of FEATURE from `el-init-record'."
  (plist-get (el-init-get-feature-record feature) property))

(defun el-init-add-record (feature property value)
  "Add a record value of PROPERTY of FEATURE to `el-init-record'."
  (aif (assoc feature el-init-record)
      (setf (cdr (assoc feature el-init-record))
            (plist-put (cdr it) property value))
    (push (cons feature (list property value)) el-init-record)))

(with-no-warnings
  ;; Since emacs 24.3, `defsetf' has been obsolete.
  (if (fboundp 'gv-define-setter)
      (gv-define-setter el-init-get-record (value feature property)
        `(el-init-add-record ,feature ,property ,value))
    (defsetf el-init-get-record (feature property) (value)
      `(el-init-add-record ,feature ,property ,value))))



;;;; Alert

(defvar el-init-alert-buffer-name "*el-init alerts*"
  "A name of a buffer which `el-init-alert' outputs to.")

(defvar el-init-alert-function #'el-init-default-alert
  "A function which is called by `el-init-alert'.
It has a parameter which is a string to display like `el-init-default-alert'.")

(defvar el-init-alert-enabled-p t
  "When its value is non-nil, `el-init-alert' displays messages.")

(defun el-init-alert (message)
  "Display MESSAGE as an alert.
When `el-init-alert-enabled-p' is nil, this function does nothing.
This function just calls `el-init-alert-function'."
  (when el-init-alert-enabled-p
    (funcall el-init-alert-function message)))

(defun el-init-default-alert (message)
  "Default alert function of `el-init-alert'."
  (display-warning 'el-init message nil el-init-alert-buffer-name))



;;;; Require Wrapper

(defvar el-init--require-wrappers nil
  "[internal] A `require' wrapper function list which is used by `el-init-next'.")

(defun el-init-next (feature &optional filename noerror)
  "Call the next `require' wrapper function."
  (let ((fn (car el-init--require-wrappers))
        (el-init--require-wrappers (cdr el-init--require-wrappers)))
    (funcall fn feature filename noerror)))

(defmacro el-init:define-require (name &rest body)
  "This is an obsolete macro.

Migration guide:
- Change `el-init:define-require' to `defun'
- Add parameter list: (feature &optional filename noerror)
- Add feature, filename and noerror to calling `el-init-next'

Example:
  ;; version 0.0.9
  (el-init:define-require my-require/ignore-errors
    (ignore-errors (el-init-next)))

  ;; version 0.1.0
  (defun my-require/ignore-errors (feature &optional filename noerror)
    (ignore-errors (el-init-next feature filename noerror)))"
  (declare (indent 1))
  (warn "`el-init:define-require' is obsolete.")
  (let ((next (cl-gensym)))
    `(defun ,name (feature &optional filename noerror)
       (cl-labels ((,next () (el-init-next feature filename noerror)))
         (cl-macrolet ((el-init-next () '(,next)))
           ,@body)))))



;;;; Require Wrapper Definitions

;; benchmark
(defun el-init-require/benchmark (feature &optional filename noerror)
  "A `require' wrapper function to benchmark loading FEATURE with `benchmark-run'.
Its score is a list as a return value of `benchmark-run' and recorded to
`el-init-record'.

Example:
  (el-init-get-record 'init-foo 'el-init-require/benchmark)
  ;; => (3.6666e-05 0 0.0)"
  (let ((result (benchmark-run (el-init-next feature filename noerror))))
    (unless (el-init-get-record feature 'el-init-require/benchmark)
      (el-init-add-record feature 'el-init-require/benchmark result))))


;; record error
(defun el-init-require/record-error (feature &optional filename noerror)
  "A `require' wrapper function to record errors which occur while `el-init-load' is loading.
The record value is an error value and recorded to `el-init-record'.

Example:
  (el-init-get-record 'init-foo 'el-init-require/record-error)
  ;; => (error \"Required feature `init-foo' was not provided\")"
  (condition-case e
      (el-init-next feature filename noerror)
    (error (el-init-add-record feature 'el-init-require/record-error e)
           (el-init-alert (error-message-string e)))))


;; ignore error
(defun el-init-require/ignore-errors (feature &optional filename noerror)
  "A `require' wrapper function to ignore errors which occur while `el-init-load' is loading.
This wrapper records no values."
  (ignore-errors (el-init-next feature filename noerror)))


;; eval-after-load
(defun el-init-require/record-eval-after-load-error
    (feature &optional filename noerror)
  "A `require' wrapper function to record errors which occur in `eval-after-load' form.
The record value is a list of plists and recorded to `el-init-record'.
The plist consists of `:file' property and `:error' property.
`:file' property is the value of file argument of `eval-after-load' and
`:error' property is an error value.

Example:
  (el-init-get-record 'init-foo 'el-init-require/record-eval-after-load-error)
  ;; => ((:file lib-bar :error (error \"Error\")) ...)"
  (let* ((original (symbol-function 'eval-after-load))
         (e (cl-gensym))
         (fn (lambda (file form)
               (funcall original
                        file
                        `(condition-case ,e
                             ,(if (functionp form) `(funcall ,form) form)
                           (error
                            (push (list :file ,file :error ,e)
                                  (el-init-get-record
                                   ',feature
                                   'el-init-require/record-eval-after-load-error))
                            (el-init-alert (error-message-string ,e))))))))
    (cl-letf (((symbol-function 'eval-after-load) fn))
      (el-init-next feature filename noerror))))


;; switching by the system (from init-loader)

;; regexps
(defvar el-init-meadow-regexp "^init-meadow-"
  "A regexp which indicates configuration files for meadow.
This is used by `el-init-require/system-case'.")

(defvar el-init-carbon-emacs-regexp "^init-carbon-emacs-"
  "A regexp which indicates configuration files for carbon emacs.
This is used by `el-init-require/system-case'.")

(defvar el-init-cocoa-emacs-regexp "^init-cocoa-emacs-"
  "A regexp which indicates configuration files for cocoa emacs.
This is used by `el-init-require/system-case'.")

(defvar el-init-nw-regexp "^init-nw-"
  "A regexp which indicates configuration files for \"emacs -nw\".
This is used by `el-init-require/system-case'.")

(defvar el-init-mac-regexp "^init-mac-"
  "A regexp which indicates configuration files for emacs on mac.
This is used by `el-init-require/system-case'.")

(defvar el-init-windows-regexp "^init-windows-"
  "A regexp which indicates configuration files for emacs on windows.
This is used by `el-init-require/system-case'.")

(defvar el-init-linux-regexp "^init-linux-"
  "A regexp which indicates configuration files for emacs on linux.
This is used by `el-init-require/system-case'.")

(defvar el-init-freebsd-regexp "^init-freebsd-"
  "A regexp which indicates configuration files for emacs on freebsd.
This is used by `el-init-require/system-case'.")

;; predicates

(defun el-init-meadowp ()
  "Return non-nil if the running emacs is meadow.
This is used by `el-init-require/system-case'."
  (featurep 'meadow))

(defun el-init-carbon-emacs-p ()
  "Return non-nil if the running emacs is carbon emacs.
This is used by `el-init-require/system-case'."
  (featurep 'carbon-emacs-package))

(defun el-init-cocoa-emacs-p ()
  "Return non-nil if the running emacs is cocoa emacs.
This is used by `el-init-require/system-case'."
  (eq window-system 'ns))

(defun el-init-nwp ()
  "Return non-nil if the running emacs is \"emacs -nw\".
This is used by `el-init-require/system-case'."
  (null window-system))

(defun el-init-macp ()
  "Return non-nil if emacs is running on mac.
This is used by `el-init-require/system-case'."
  (string-match-p "apple-darwin" system-configuration))

(defun el-init-windowsp ()
  "Return non-nil if emacs is running on windows.
This is used by `el-init-require/system-case'."
  (string-match-p "mingw" system-configuration))

(defun el-init-linuxp ()
  "Return non-nil if emacs is running on linux.
This is used by `el-init-require/system-case'."
  (string-match-p "linux" system-configuration))

(defun el-init-freebsdp ()
  "Return non-nil if emacs is running on freebsd.
This is used by `el-init-require/system-case'."
  (string-match-p "freebsd" system-configuration))

(defvar el-init-system-case-alist
  (list (cons el-init-meadow-regexp       #'el-init-meadowp)
        (cons el-init-carbon-emacs-regexp #'el-init-carbon-emacs-p)
        (cons el-init-cocoa-emacs-regexp  #'el-init-cocoa-emacs-p)
        (cons el-init-nw-regexp           #'el-init-nwp)
        (cons el-init-mac-regexp          #'el-init-macp)
        (cons el-init-windows-regexp      #'el-init-windowsp)
        (cons el-init-linux-regexp        #'el-init-linuxp)
        (cons el-init-freebsd-regexp      #'el-init-freebsdp))
  "A list of \"(regexp . predicate)\".
The regexp indicates whether a configuration file is for a system.
The predicate is a function; it returns non-nil if the execution environment
matches a system.
This is used by `el-init-require/system-case'.")

(defun el-init-require/system-case (feature &optional filename noerror)
  "A `require' wrapper function to switch configuration files to load.
The switching is based on `el-init-system-case-alist'.
If a regexp of `el-init-system-case-alist' matches a feature name of
a configuration file and the paired predicate function returns non-nil,
the file will be loaded, but it will not if the predicate returns nil.
If all the regexps don't match a feature name, the file will also be loaded.
This wrapper records no values."
  (let ((match (cl-loop for (regexp . predicate) in el-init-system-case-alist
                        when (string-match-p regexp (symbol-name feature))
                        return predicate)))
    (when (or (not match) (funcall match))
      (el-init-next feature filename noerror))))


;; old .elc files

(defun el-init--ensure-string (object)
  (format "%s" object))

(defun el-init--file-name-el (filename)
  (concat (file-name-sans-extension filename) ".el"))

(defun el-init--file-name-elc (filename)
  (concat (file-name-sans-extension filename) ".elc"))

(defun el-init--old-library-p (library)
  (awhen (locate-library (el-init--ensure-string library))
    (let ((el (el-init--file-name-el it))
          (elc (el-init--file-name-elc it)))
      (when (file-newer-than-file-p el elc)
        elc))))

(defun el-init--byte-compile-library (library)
  (awhen (locate-library (el-init--ensure-string library))
    (ignore-errors
      (byte-compile-file
       (el-init--file-name-el it)))))

(defun el-init-require/record-old-library (feature &optional filename noerror)
  "A `require' wrapper function to record whether a configuration file has an old .elc file.
The record value is boolean; if old .elc file exists, the value is t.

Example:
  (el-init-get-record 'init-foo 'el-init-require/record-old-library)
  ;; => t"
  (el-init-add-record feature
                      'el-init-require/record-old-library
                      (and (el-init--old-library-p (or filename feature)) t))
  (el-init-next feature filename noerror))

(defun el-init-require/compile-old-library (feature &optional filename noerror)
  "A `require' wrapper function to compile an old .elc file of a configuration file.
The compilation is executed before loading the configuration file.
The record value is boolean; non-nil means compilation is successful and
nil means failure.
If the .elc file is not old, this wrapper records no values.

Example:
  (el-init-get-record 'init-foo 'el-init-require/record-old-library)
  ;; => t"
  (when (el-init--old-library-p (or filename feature))
    (let ((result (el-init--byte-compile-library (or filename feature))))
      (el-init-add-record feature
                          'el-init-require/compile-old-library
                          result)))
  (el-init-next feature filename noerror))


;; lazy loading

(defvar el-init-lazy-init-regexp "^init-lazy-\\(.+\\)$"
  "A regexp which matches configuration files for lazy loading.
The first group of the regexp indicates a feature name as an argument
of `eval-after-load'.
This is used by `el-init-require/lazy'.")

(defun el-init-require/lazy (feature &optional filename noerror)
  "A `require' wrapper function for lazy loading.
If a configuration file feature name matches `el-init-lazy-init-regexp',
this calls `eval-after-load' instead of loading it.
This wrapper records no values."
  (save-match-data
    (if (string-match el-init-lazy-init-regexp (symbol-name feature))
        (eval-after-load (match-string 1 (symbol-name feature))
          `(let ((el-init--require-wrappers ',el-init--require-wrappers))
             (el-init-next ',feature ',filename ',noerror)))
      (el-init-next feature filename noerror))))



;;;; Loader

(defvar el-init-load-file-regexp "\\.elc?$"
  "A regexp which matches configuration file names.")

(defvar el-init-subdirectories '(".")
  "A default value of the subdirectories parameter of `el-init-load'.")

(defvar el-init-wrappers '(el-init-require/record-error)
  "A default value of the wrappers parameter of `el-init-load'.")

(defvar el-init-override-only-init-files-p t
  "A default value of the override-only-init-files parameter of `el-init-load'.")

(defvar el-init-overridep t
  "A default value of the override parameter of `el-init-load'.")

(defvar el-init-before-load-hook nil
  "A hook which is run before loading of `el-init-load'.")

(defvar el-init-after-load-hook nil
  "A hook which is run after loading of `el-init-load'.")

(define-obsolete-variable-alias
  'el-init:load-function-list
  'el-init-wrappers
  "0.1.0")

(define-obsolete-variable-alias
  'el-init:load-directory-list
  'el-init-subdirectories
  "0.1.0")

(defun el-init--path-concat (&rest paths)
  (expand-file-name
   (cl-reduce (lambda (x y) (concat (file-name-as-directory x) y))
              paths)))

(defun el-init--list-subdirectories (directory)
  (cl-remove-if
   (lambda (x)
     (string-match-p (rx bos (or ".." ".") eos)
                     (file-name-nondirectory x)))
   (cl-remove-if-not #'file-directory-p
                     (directory-files directory t))))

(defun el-init--list-all-directories (directory)
  (cons directory
        (cl-mapcan #'el-init--list-all-directories
                   (el-init--list-subdirectories directory))))

(defun el-init--expand-directory-list (directory subdirectories)
  (cl-loop for dir in (mapcar #'el-init--listify subdirectories)
           when (file-directory-p (el-init--path-concat directory (cl-first dir)))
           append (if (cl-second dir)
                      (el-init--list-all-directories
                       (el-init--path-concat directory (cl-first dir)))
                    (list (el-init--path-concat directory (cl-first dir))))))

(defun el-init--target-files (directory subdirectories)
  (cl-loop for d in (el-init--expand-directory-list directory subdirectories)
           append (directory-files d nil el-init-load-file-regexp)))

(defun el-init--make-overridden-require (original
                                         wrappers
                                         init-features
                                         only-init-files)
  (lambda (feature &optional filename noerror)
    (if (or (not only-init-files)
            (memq feature init-features))
        (let ((el-init--require-wrappers wrappers))
          (el-init-next feature filename noerror))
      (funcall original feature filename noerror))))

;;;###autoload
(cl-defun el-init-load (directory
                        &key
                        (subdirectories el-init-subdirectories)
                        (wrappers el-init-wrappers)
                        (override-only-init-files el-init-override-only-init-files-p)
                        (override el-init-overridep)
                        ;; for compatibility with 0.0.9
                        (directory-list nil directory-list-flag)
                        (function-list nil function-list-flag))
  "Load configuration files in DIRECTORY with `require'.

DIRECTORY is a path of a directory which is root of configuration files.
SUBDIRECTORIES is a list of subdirectories of DIRECTORY; its element is
a string or a list like (\"path\" t).
 means including all the directories in \"path\".
OVERRIDE-ONLY-INIT-FILES is a flag to use overridden `require' only for
configuration files.
OVERRIDE is a flag to use overridden `require' when `require' called in
configuration files.

The mechanism:
- Add SUBDIRECTORIES to `load-path'
- Call `require' for all the configure files in SUBDIRECTORIES"
  ;; for compatibility with 0.0.9
  (when directory-list-flag
    (setq subdirectories directory-list)
    (warn "`:directory-list' parameter of `el-init-load' is obsolete; use `:subdirectories' instead."))
  (when function-list-flag
    (setq wrappers function-list)
    (warn "`:function-list' parameter of `el-init-load' is obsolete; use `:wrappers' instead."))

  (run-hooks 'el-init-before-load-hook)
  (cl-dolist (dir (el-init--expand-directory-list directory subdirectories))
    (add-to-list 'load-path dir))
  (condition-case e
      (let* ((original
              (if override (symbol-function 'require) 'require))
             (el-init--require-wrappers
              (append wrappers (list original)))
             (init-features
              (cl-remove-duplicates
               (mapcar #'el-init--file-name-to-symbol
                       (el-init--target-files directory subdirectories))))
             (overridden-require
              (el-init--make-overridden-require original
                                                el-init--require-wrappers
                                                init-features
                                                override-only-init-files)))
        (cl-dolist (feature init-features)
          (if override
              (cl-letf (((symbol-function 'require) overridden-require))
                (el-init-next feature))
            (el-init-next feature))))
    (error (el-init-alert (error-message-string e))))
  (run-hooks 'el-init-after-load-hook))


;; for compatibility with 0.1.0
(define-obsolete-function-alias 'el-init:provide 'el-init-provide "0.1.1")
(define-obsolete-variable-alias 'el-init:record 'el-init-record "0.1.1")
(define-obsolete-function-alias 'el-init:get-feature-record 'el-init-get-feature-record "0.1.1")
(define-obsolete-function-alias 'el-init:get-record 'el-init-get-record "0.1.1")
(define-obsolete-function-alias 'el-init:add-record 'el-init-add-record "0.1.1")
(define-obsolete-variable-alias 'el-init:alert-buffer-name 'el-init-alert-buffer-name "0.1.1")
(define-obsolete-variable-alias 'el-init:alert-function 'el-init-alert-function "0.1.1")
(define-obsolete-variable-alias 'el-init:alert-enabled-p 'el-init-alert-enabled-p "0.1.1")
(define-obsolete-function-alias 'el-init:alert 'el-init-alert "0.1.1")
(define-obsolete-variable-alias 'el-init:default-alert 'el-init-default-alert "0.1.1")
(define-obsolete-function-alias 'el-init:next 'el-init-next "0.1.1")
(define-obsolete-function-alias 'el-init:require/benchmark 'el-init-require/benchmark "0.1.1")
(define-obsolete-function-alias 'el-init:require/record-error 'el-init-require/record-error "0.1.1")
(define-obsolete-function-alias 'el-init:require/ignore-errors 'el-init-require/ignore-errors "0.1.1")
(define-obsolete-function-alias 'el-init:require/record-eval-after-load-error 'el-init-require/record-eval-after-load-error "0.1.1")
(define-obsolete-variable-alias 'el-init:meadow-regexp 'el-init-meadow-regexp "0.1.1")
(define-obsolete-variable-alias 'el-init:carbon-emacs-regexp 'el-init-carbon-emacs-regexp "0.1.1")
(define-obsolete-variable-alias 'el-init:cocoa-emacs-regexp 'el-init-cocoa-emacs-regexp "0.1.1")
(define-obsolete-variable-alias 'el-init:nw-regexp 'el-init-nw-regexp "0.1.1")
(define-obsolete-variable-alias 'el-init:mac-regexp 'el-init-mac-regexp "0.1.1")
(define-obsolete-variable-alias 'el-init:windows-regexp 'el-init-windows-regexp "0.1.1")
(define-obsolete-variable-alias 'el-init:linux-regexp 'el-init-linux-regexp "0.1.1")
(define-obsolete-variable-alias 'el-init:freebsd-regexp 'el-init-freebsd-regexp "0.1.1")
(define-obsolete-function-alias 'el-init:meadowp 'el-init-meadowp "0.1.1")
(define-obsolete-function-alias 'el-init:carbon-emacs-p 'el-init-carbon-emacs-p "0.1.1")
(define-obsolete-function-alias 'el-init:cocoa-emacs-p 'el-init-cocoa-emacs-p "0.1.1")
(define-obsolete-function-alias 'el-init:nwp 'el-init-nwp "0.1.1")
(define-obsolete-function-alias 'el-init:macp 'el-init-macp "0.1.1")
(define-obsolete-function-alias 'el-init:windowsp 'el-init-windowsp "0.1.1")
(define-obsolete-function-alias 'el-init:linuxp 'el-init-linuxp "0.1.1")
(define-obsolete-function-alias 'el-init:freebsdp 'el-init-freebsdp "0.1.1")
(define-obsolete-variable-alias 'el-init:system-case-alist 'el-init-system-case-alist "0.1.1")
(define-obsolete-function-alias 'el-init:require/system-case 'el-init-require/system-case "0.1.1")
(define-obsolete-function-alias 'el-init:require/record-old-library 'el-init-require/record-old-library "0.1.1")
(define-obsolete-function-alias 'el-init:require/compile-old-library 'el-init-require/compile-old-library "0.1.1")
(define-obsolete-variable-alias 'el-init:lazy-init-regexp 'el-init-lazy-init-regexp "0.1.1")
(define-obsolete-function-alias 'el-init:require/lazy 'el-init-require/lazy "0.1.1")
(define-obsolete-variable-alias 'el-init:load-file-regexp 'el-init-load-file-regexp "0.1.1")
(define-obsolete-variable-alias 'el-init:subdirectories 'el-init-subdirectories "0.1.1")
(define-obsolete-variable-alias 'el-init:wrappers 'el-init-wrappers "0.1.1")
(define-obsolete-variable-alias 'el-init:override-only-init-files-p 'el-init-override-only-init-files-p "0.1.1")
(define-obsolete-variable-alias 'el-init:overridep 'el-init-overridep "0.1.1")
(define-obsolete-variable-alias 'el-init:before-load-hook 'el-init-before-load-hook "0.1.1")
(define-obsolete-variable-alias 'el-init:after-load-hook 'el-init-after-load-hook "0.1.1")
(define-obsolete-function-alias 'el-init:load 'el-init-load "0.1.1")


(provide 'el-init)
;;; el-init.el ends here
