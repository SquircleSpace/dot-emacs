(use-package c-config
  :no-require t
  :magic (((rx (sequence line-start "#include <" (+ alpha) ">" line-end)) . c++-mode)))

(provide 'c++-config)
