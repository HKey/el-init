;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'el-init)

;;;; Record

(ert-deftest el-init-test:record ()
  (let ((el-init:record nil))
    (el-init:add-record 'el-init-test 'foo "foo")

    (should (string= (el-init:get-record 'el-init-test 'foo) "foo"))
    (should (eq (el-init:get-record 'el-init-test 'bar) nil))))

;;; el-init-test.el ends here
