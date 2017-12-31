(require [src.utils.macros [*]])
(import [src.utils.macros [*]])
(require [hy.extra.anaphoric [*]])

(import
  hy hy.compiler hy.macros
  [hy.lex.parser [hy-symbol-mangle hy-symbol-unmangle]])

;; eval due to issue #1467: https://github.com/hylang/hy/issues/1467
(hy.eval `(import hy.macros [hy.core.language [*]] [hy.core.macros [*]]))


;; * Namespace

(defclass Namespace [object]
  (defn --init-- [self &optional globals- locals-]
    (setv self.globals
          (or globals- (globals)))
    (setv self.locals
          (or locals- (locals)))

    (setv self.compile-table
          (.-collect-compile-table self))
    (setv self.macros
          (.-collect-macros self))
    (setv self.shadows
          (.-collect-shadows self))

    (setv self.names
          (.-collect-names self)))

  #@(staticmethod
      (defn -collect-compile-table []
        "Collect compile table as dict."
        (->> hy.compiler.-compile-table
          (tz.keymap hy-symbol-unmangle))))

  #@(staticmethod
      (defn -collect-macros []
        "Collect and merge macros from all namespaces as single dict."
        (->> hy.macros.-hy-macros
          (.values)
          (#%(merge-with (fn [a b] a) #* %1))
          (tz.keymap hy-symbol-unmangle))))

  #@(staticmethod
      (defn -collect-shadows []
        "Collect shadows as a list, purely for annotation checks."
        (->> hy.core.shadow
          dir
          (map hy-symbol-unmangle)
          tuple)))

  (defn -collect-names [self]
    "Collect all global names from (locals), macros, and the compile-table."
    (->>
      (chain (.keys self.globals)
             (.keys self.locals)
             (.keys self.macros)
             (.keys self.compile-table))
      flatten  ; Required for globals/locals
      (map #%(if (instance? str %1) %1 %1.--name--))
      distinct
      tuple)))
