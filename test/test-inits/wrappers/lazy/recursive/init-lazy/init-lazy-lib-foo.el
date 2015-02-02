(require 'lib-foo)

(defvar el-init-test-init-lazy-recursive-count 0)

(setq el-init-test-init-lazy-recursive-count
      (1+ el-init-test-init-lazy-recursive-count))

(provide 'init-lazy-lib-foo)
