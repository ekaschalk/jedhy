(import
  builtins inspect types

  hy hy.compiler hy.macros

  ;; [hy.core.shadow [*]] [hy.core.language [*]]
  )

;; See https://github.com/hylang/hy/issues/1467
(hy.eval '(import hy.macros))
