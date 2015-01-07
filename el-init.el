;;; el-init.el --- A loader inspired by init-loader -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Hiroki YAMAKAWA

;; Author: Hiroki YAMAKAWA <s06139@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((cl-lib "0.5"))
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

;;;; Utilities

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

(defun el-init:provide ()
  (provide
   (el-init::file-name->symbol
    (or load-file-name
        (buffer-file-name)))))



;;;; Record

(defvar el-init:record nil
  "alist (feature . plist (prop val))")

(defun el-init:get-record (feature property)
  (plist-get (cdr (assoc feature el-init:record)) property))

(defun el-init:add-record (feature property value)
  (el-init::aif (assoc feature el-init:record)
      (setf (cdr (assoc feature el-init:record))
            (plist-put (cdr it) property value))
    (push (cons feature (list property value)) el-init:record)))

(gv-define-setter el-init:get-record (value feature property)
  `(el-init:add-record ,feature ,property ,value))



;;;; Require Wrapper

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



;;;; Require Wrapper Definitions

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
  (cl-flet ((eval-after-load (file form)
              (el-init::eval-after-load/original
               file
               (let ((e (cl-gensym)))
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
  (let ((match (cl-loop for (regexp . predicate) in el-init:system-case-alist
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



;;;; Loader

;; directory-list, subdirectoriesの統一

(defvar el-init:load-file-regexp "\\.elc?$" "読み込み対象ファイルの正規表現")
(defvar el-init:load-directory-list '("base" "init" "lang") "探索対象のディレクトリ")
(defvar el-init:load-function-compile nil)
(defvar el-init:before-load-hook nil)
(defvar el-init:after-load-hook nil)


(defun el-init::path-concat-rec (&rest path)
  (if (null (cdr path))
      (car path)
    (concat (file-name-as-directory (car path))
            (apply #'el-init::path-concat-rec (cdr path)))))

(defun el-init::path-concat (&rest path)
  (expand-file-name (apply #'el-init::path-concat-rec path)))

(defun el-init::directory-directories (directory)
  (cl-remove-if
   (lambda (x)
     (string-match-p (rx bos (or ".." ".") eos)
                     (file-name-nondirectory x)))
   (cl-remove-if-not #'file-directory-p
                     (directory-files directory t))))

(defun el-init::directory-directories-all (directory)
  (cons directory
        (cl-mapcan #'el-init::directory-directories-all
                   (el-init::directory-directories directory))))


(cl-defun el-init::load-directories (directory &optional (subdirectories el-init:load-directory-list))
  (cl-loop for dir in (mapcar #'el-init::listify subdirectories)
           when (file-directory-p (el-init::path-concat directory (cl-first dir)))
           append (if (cl-second dir)
                      (el-init::directory-directories-all
                       (el-init::path-concat directory (cl-first dir)))
                    (list (el-init::path-concat directory (cl-first dir))))))

(cl-defun el-init::load-files (directory &optional (subdirectories el-init:load-directory-list))
  (cl-loop for d in (el-init::load-directories directory subdirectories)
           collect (directory-files d nil el-init:load-file-regexp)))

(cl-defun el-init:load (directory
                        &key
                        (directory-list el-init:load-directory-list)
                        (function-list el-init:load-function-list)
                        (compile el-init:load-function-compile)
                        override)              ;require の乗っ取り
  ;; フックの実行
  (run-hooks 'el-init:before-load-hook)
  ;; load-pathへの追加
  (cl-dolist (dir (el-init::load-directories directory directory-list))
    (add-to-list 'load-path dir))
  ;; 各ファイルのロード
  (unwind-protect
      (let ((load-fn (el-init::combine-require function-list compile)))
        (cl-dolist (files (el-init::load-files directory directory-list))
          (cl-dolist (feature (cl-remove-duplicates
                               (mapcar #'el-init::file-name->symbol files)))
            (if override
                (cl-letf (((symbol-function 'require) load-fn))
                  (require feature))
              (funcall load-fn feature)))))
    ;; フックの実行
    (run-hooks 'el-init:after-load-hook)))



(provide 'el-init)
;;; el-init.el ends here
