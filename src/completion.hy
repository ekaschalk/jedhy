(require [src.utils.macros [*]])
(import [src.utils.macros [*]])
(require [hy.extra.anaphoric [*]])
(import
  hy hy.compiler hy.macros
  [hy.lex.parser [hy-symbol-unmangle]])


(defclass Completer [object]
  "Instantiates global candidates, then called with prefixes for completion."

  (defn --init-- [self &optional namespace local]
    (setv self.namespace (or namespace (globals)))
    (setv self.local (or local (locals)))

    (setv self.candidates (.-collect-globals self)))

  (defn -collect-globals [self]
    "Collect globals from (locals), macros, and the compile-table."
    (hy.eval `(import [hy.core.shadow [*]] [hy.core.macros [*]]))

    (->> hy.macros.-hy-macros
      (.values)
      (map dict.keys)
      (chain (.keys self.namespace)
             (.keys self.local)
             hy.compiler.-compile-table)
      flatten
      (map #%(if (instance? str %1) %1 %1.--name--))
      (map hy-symbol-unmangle)
      distinct
      tuple))

  (defn reset [self &optional namespace local]
    "Reconstruct global candidates to detect changes in macros/locals."
    (.--init-- self namespace local))

  (defn --call-- [self prefix]
    "Get candidates for a given Prefix."
    (setv candidates
          (or (.attributes prefix.candidate) self.candidates))

    (print prefix.attr-prefix)
    (print candidates)

    (some->> candidates
      (filter #%(.startswith %1 prefix.attr-prefix))
      (map #$(+ (str prefix.candidate) "."))
      tuple)))
