;; * Completer

(defclass Completer [object]
  "Instantiates global candidates, then called with prefixes for completion."

  (defn --init-- [self]
    (setv self.candidates (.-collect-globals self)))

  #@(staticmethod
      (defn -collect-globals []
        "Collect globals from (locals), macros, and the compile-table."
        #t(->> hy.macros.-hy-macros
            (.values)
            (map dict.keys)
            (chain (.keys locals) hy.compiler.-compile-table)
            flatten
            (map #%(if (instance? str %1) %1 %1.--name--))
            (map hy-symbol-unmangle)
            distinct)))

  (defn reset [self]
    "Reconstruct global candidates to detect changes in macros/locals."
    (.--init-- self))

  (defn --call-- [self prefix]
    "Get candidates for a given Prefix."
    (setv candidates
          (or (.attributes self.prefix.candidate) self.candidates))

    #t(some->> candidates
            (filter #%(.startswith %1 prefix.attr-prefix))
            (map #$(+ candidate ".")))))
