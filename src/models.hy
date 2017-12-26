(require [src.utils.macros [*]])
(import [src.utils.macros [*]])
(require [hy.extra.anaphoric [*]])
(import
  builtins

  hy hy.compiler hy.macros
  [hy.lex.parser [hy-symbol-mangle hy-symbol-unmangle]]
  ;; [hy.core.shadow [*]] [hy.core.language [*]]
  )

;; * Candidate

(defclass Candidate [object]
  (defn --init-- [self symbol]
    (setv self.symbol
          (hy-symbol-unmangle symbol))
    (setv self.mangled
          (hy-symbol-mangle symbol)))

  (defn --repr-- [self]
    (.format "Candidate<(symbol={}>)" self.symbol))

  (defn compiler? [self]
    "Is candidate a compile table construct and return it."
    (try (get hy.compiler.-compile-table self.symbol)
         (except [e KeyError] None)))

  (defn macro? [self]
    "Is candidate a macro and return it."
    ;; TODO Enable for namespaces other than None (hy core)
    (hy.eval '(import hy.macros))  ; See https://github.com/hylang/hy/issues/1467
    (try (get hy.macros.-hy-macros None self.mangled)
         (except [e KeyError] None)))

  (defn shadow? [self]
    "Is candidate a shadowed operator and return it."
    (try (some #$(= self.mangled) (dir hy.core.shadow))
         (except [e KeyError] None)))

  (defn evaled? [self]
    "Is candidate evaluatable and return it."
    (try (builtins.eval self.mangled (globals))
         (except [e Exception] None)))

  (defn get-obj [self]
    "Get object for underlying candidate."
    (or (.compiler? self) (.macro? self) (.evaled? self)))

  (defn attributes [self]
    "Return attributes for obj if they exist."
    (some->> self
           (.evaled?)
           dir
           (map hy-symbol-unmangle)
           tuple)))

;; * Prefix

(defclass Prefix [object]
  "A completion candidate."

  (defn --init-- [self prefix]
    (setv self.prefix prefix)
    (setv [self.candidate
           self.attr-prefix]
          (.split-prefix self prefix)))

  #@(staticmethod
      (defn split-prefix [prefix]
        "Split prefix on last dot accessor, returning an obj, attr pair."
        (setv components
              (.split prefix "."))

        [(->> components butlast (.join ".") Candidate)
         (->> components last)])))
