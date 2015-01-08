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
  (let ((target-directory (expand-file-name "test-inits"
                                            el-init-test:test-directory)))
    (el-init-test:sandbox
      (should-not (featurep 'init-test-a))
      (should-not (featurep 'init-test-b))
      (should-not (featurep 'init-test-c))

      (el-init:load target-directory :directory-list '("."))

      (should     (featurep 'init-test-a))
      (should-not (featurep 'init-test-b))
      (should-not (featurep 'init-test-c)))

    ;; sub directory
    (el-init-test:sandbox
      (should-not (featurep 'init-test-a))
      (should-not (featurep 'init-test-b))
      (should-not (featurep 'init-test-c))

      (el-init:load target-directory :directory-list '("." "subdir1"))

      (should     (featurep 'init-test-a))
      (should     (featurep 'init-test-b))
      (should-not (featurep 'init-test-c)))

    ;; recursive
    (el-init-test:sandbox
      (should-not (featurep 'init-test-a))
      (should-not (featurep 'init-test-b))
      (should-not (featurep 'init-test-c))

      (el-init:load target-directory :directory-list '(("." t)))

      (should (featurep 'init-test-a))
      (should (featurep 'init-test-b))
      (should (featurep 'init-test-c)))))

;;; el-init-test.el ends here
