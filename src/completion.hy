(require [src.utils.macros [*]])
(require [hy.extra.anaphoric [*]])
(import
  hy hy.compiler hy.macros
  [hy.lex.parser [hy-symbol-unmangle]])


(defclass Completer [object]
  "Instantiates global candidates, then called with prefixes for completion."

  (defn --init-- [self]
    (setv self.candidates (.-collect-globals self)))

  #@(staticmethod
      (defn -collect-globals []
        "Collect globals from (locals), macros, and the compile-table."
        (->> hy.macros.-hy-macros
          (.values)
          (map dict.keys)
          (chain (.keys (globals)) (.keys (locals)) hy.compiler.-compile-table)
          flatten
          (map #%(if (instance? str %1) %1 %1.--name--))
          (map hy-symbol-unmangle)
          distinct
          tuple)))

  (defn reset [self]
    "Reconstruct global candidates to detect changes in macros/locals."
    (.--init-- self))

  (defn --call-- [self prefix]
    "Get candidates for a given Prefix."
    (setv candidates
          (or (.attributes self.prefix.candidate) self.candidates))

    (some->> candidates
      (filter #%(.startswith %1 prefix.attr-prefix))
      (map #$(+ candidate "."))
      tuple)))
