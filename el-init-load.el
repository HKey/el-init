(require 'cl)
(require 'el-init-require)

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
  (remove-if
   (lambda (x)
     (string-match-p (rx bos (or ".." ".") eos)
                     (file-name-nondirectory x)))
   (remove-if-not #'file-directory-p
                  (directory-files directory t))))

(defun el-init::directory-directories-all (directory)
  (cons directory
        (mapcan #'el-init::directory-directories-all
                (el-init::directory-directories directory))))


(defun* el-init::load-directories (directory &optional (subdirectories el-init:load-directory-list))
  (loop for dir in (mapcar #'el-init::listify subdirectories)
        when (file-directory-p (el-init::path-concat directory (first dir)))
        append (if (second dir)
                   (el-init::directory-directories-all
                    (el-init::path-concat directory (first dir)))
                 (list (el-init::path-concat directory (first dir))))))

(defun* el-init::load-files (directory &optional (subdirectories el-init:load-directory-list))
  (loop for d in (el-init::load-directories directory subdirectories)
        collect (directory-files d nil el-init:load-file-regexp)))

(defun* el-init:load (directory
                      &key
                      (directory-list el-init:load-directory-list)
                      (function-list el-init:load-function-list)
                      (compile el-init:load-function-compile)
                      override)              ;require の乗っ取り
  ;; フックの実行
  (run-hooks 'el-init:before-load-hook)
  ;; load-pathへの追加
  (dolist (dir (el-init::load-directories directory directory-list))
    (add-to-list 'load-path dir))
  ;; 各ファイルのロード
  (unwind-protect
      (let ((load-fn (el-init::combine-require function-list compile)))
        (dolist (files (el-init::load-files directory directory-list))
          (dolist (feature (remove-duplicates
                            (mapcar #'el-init::file-name->symbol files)))
            (if override
                (letf (((symbol-function 'require) load-fn))
                  (require feature))
              (funcall load-fn feature)))))
    ;; フックの実行
    (run-hooks 'el-init:after-load-hook)))



(provide 'el-init-load)
