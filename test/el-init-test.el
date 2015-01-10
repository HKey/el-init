;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'el-init)
(require 'el-init-test-helper)

;;;; Record

(ert-deftest el-init-test:record ()
  (el-init-test:sandbox
    (el-init:add-record 'el-init-test 'foo "foo")

    (should (equal (el-init:get-feature-record 'el-init-test)
                   '(foo "foo")))
    (should (string= (el-init:get-record 'el-init-test 'foo) "foo"))
    (should (eq (el-init:get-record 'el-init-test 'bar) nil))

    (setf (el-init:get-record 'el-init-test 'bar) "bar")

    (should (string= (el-init:get-record 'el-init-test 'bar) "bar"))))

;;;; Loader

(ert-deftest el-init-test:loader ()
  (let ((target-directory (el-init-test:get-path "test-inits/loader")))
    (el-init-test:sandbox
      (should-not (featurep 'init-test-a))
      (should-not (featurep 'init-test-b))
      (should-not (featurep 'init-test-c))

      (el-init:load target-directory
                    :directory-list '(".")
                    :function-list  nil)

      (should     (featurep 'init-test-a))
      (should-not (featurep 'init-test-b))
      (should-not (featurep 'init-test-c)))

    ;; sub directory
    (el-init-test:sandbox
      (should-not (featurep 'init-test-a))
      (should-not (featurep 'init-test-b))
      (should-not (featurep 'init-test-c))

      (el-init:load target-directory
                    :directory-list '("." "subdir1")
                    :function-list  nil)

      (should     (featurep 'init-test-a))
      (should     (featurep 'init-test-b))
      (should-not (featurep 'init-test-c)))

    ;; recursive
    (el-init-test:sandbox
      (should-not (featurep 'init-test-a))
      (should-not (featurep 'init-test-b))
      (should-not (featurep 'init-test-c))

      (el-init:load target-directory
                    :directory-list '(("." t))
                    :function-list  nil)

      (should (featurep 'init-test-a))
      (should (featurep 'init-test-b))
      (should (featurep 'init-test-c)))

    ;; override
    (el-init-test:sandbox
      (let ((feature-list nil))
        (add-to-list 'load-path target-directory)

        (el-init:load target-directory
                      :directory-list '("override")
                      :function-list
                      (list
                       (lambda (feature &optional filename noerror)
                         (push feature feature-list)
                         (el-init:next feature filename noerror)))
                      :override t)

        (should (memq 'init-test-a        feature-list))
        (should (memq 'init-test-override feature-list))))))

;;;; Require Wrappers

(ert-deftest el-init-test:require/benchmark ()
  (el-init-test:sandbox
    (el-init:load (el-init-test:get-path "test-inits/wrappers/benchmark")
                  :directory-list '(".")
                  :function-list (list #'el-init:require/benchmark))

    (let ((record (el-init:get-record 'init-test-benchmark
                                      'el-init:require/benchmark)))
      (should (= (length record) 3))
      (should (cl-every #'numberp record)))))

(ert-deftest el-init-test:require/record-error ()
  (el-init-test:sandbox
    (el-init:load (el-init-test:get-path "test-inits/wrappers/error")
                  :directory-list '(".")
                  :function-list (list #'el-init:require/record-error))

    (should (equal (el-init:get-record 'init-test-error
                                       'el-init:require/record-error)
                   '(error "Error")))))

(ert-deftest el-init-test:require/ignore-errors ()
  (el-init-test:sandbox
    (let ((caught nil))
      (condition-case e
          (el-init:load (el-init-test:get-path "test-inits/wrappers/error")
                        :directory-list '(".")
                        :function-list (list #'el-init:require/ignore-errors))
        (error (setq caught e)))

      (should (null caught)))))

(ert-deftest el-init-test:require/record-eval-after-load-error ()
  (el-init-test:sandbox
    (el-init:load (el-init-test:get-path "test-inits/wrappers/eval-after-load")
                  :directory-list '("error")
                  :function-list (list #'el-init:require/record-eval-after-load-error))
    (add-to-list 'load-path
                 (el-init-test:get-path "test-inits/wrappers/eval-after-load"))
    (require 'init-test-library)

    (let ((record (el-init:get-record 'init-test-error
                                      'el-init:require/record-eval-after-load-error)))
      (should (= (length record) 1))
      (should (equal (plist-get (cl-first record) :error)
                     '(error "Error"))))))

(ert-deftest el-init-test:require/record-old-library ()
  (let* ((dir (el-init-test:get-path "test-inits/wrappers/old-library"))
         (el  (concat dir "/init-test.el"))
         (fn  (lambda ()
                (el-init:load
                 dir
                 :directory-list '(".")
                 :function-list (list #'el-init:require/record-old-library))))
         (rec (lambda ()
                (el-init:get-record 'init-test
                                    'el-init:require/record-old-library))))
    (byte-compile-file el)

    (el-init-test:sandbox
      (funcall fn)
      (should-not (funcall rec)))

    (sleep-for 1)                       ; certainly update timestamp
    (shell-command (format "touch %s" (shell-quote-argument el)))

    (el-init-test:sandbox
      (funcall fn)
      (should (funcall rec)))))

;;; el-init-test.el ends here
