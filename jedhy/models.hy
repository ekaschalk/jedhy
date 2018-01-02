(require [jedhy.utils.macros [*]])
(import [jedhy.utils.macros [*]])
(require [hy.extra.anaphoric [*]])
(import
  builtins

  hy hy.compiler hy.macros
  [hy.lex.parser [hy-symbol-mangle hy-symbol-unmangle]]
  [hy.core.language [*]] [hy.core.macros [*]]  ; for Namespace's defaults
  )

;; TODO Blacklist some names ("koan" macro, "copyright" from allkeys, ...)

;; * Namespace

;; eval due to issue #1467: https://github.com/hylang/hy/issues/1467
(hy.eval `(import hy.macros))
(hy.eval `(require [hy.extra.anaphoric [*]]))

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

  #@(property
      (defn -keys-to-names [self]
        "Function for converting keys to names."
        #%(hy-symbol-unmangle
            (if (instance? str %1) %1 %1.--name--))))

  (defn -collect-compile-table [self]
    "Collect compile table as dict."
    (->> hy.compiler.-compile-table
      (tz.keymap self.-keys-to-names)))

  (defn -collect-macros [self]
    "Collect and merge macros from all namespaces as single dict."
    (->> hy.macros.-hy-macros
      (.values)
      (#%(merge-with (fn [a b] a) #* %1))
      (tz.keymap self.-keys-to-names)))

  (defn -collect-shadows [self]
    "Collect shadows as a list, purely for annotation checks."
    (->> hy.core.shadow
      dir
      (map self.-keys-to-names)
      tuple))

  (defn -collect-names [self]
    "Collect all global names from (locals), macros, and the compile-table."
    (->>
      (chain (allkeys self.globals)
             (allkeys self.locals)
             (.keys self.macros)
             (.keys self.compile-table))
      (map self.-keys-to-names)
      distinct
      tuple))

  (defn eval [self mangled-symbol]
    "Evaluate `mangled-symbol' within the Namespace."
    (builtins.eval mangled-symbol self.globals self.locals)))

;; * Candidate

(defclass Candidate [object]
  (defn --init-- [self symbol &optional namespace]
    (setv self.symbol
          (hy-symbol-unmangle symbol))
    (setv self.mangled
          (hy-symbol-mangle symbol))
    (setv self.namespace
          (or namespace (Namespace))))

  (defn --str-- [self]
    self.symbol)

  (defn --repr-- [self]
    (.format "Candidate<(symbol={}>)" self.symbol))

  (defn --bool-- [self]
    (bool self.symbol))

  (defn compiler? [self]
    "Is candidate a compile table construct and return it."
    (try (get self.namespace.compile-table self.symbol)
         (except [e KeyError] None)))

  (defn macro? [self]
    "Is candidate a macro and return it."
    (try (get self.namespace.macros self.symbol)
         (except [e KeyError] None)))

  (defn shadow? [self]
    "Is candidate a shadowed operator, do *not* return it."
    (or (in self.symbol self.namespace.shadows) None))

  (defn evaled? [self]
    "Is candidate evaluatable and return it."
    (try (.eval self.namespace self.mangled)
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
      tuple))

  #@(staticmethod
      (defn -translate-class [klass]
        "Return annotation given a name of a class."
        (cond [(in klass ["function" "builtin_function_or_method"])
               "def"]
              [(= klass "type")
               "class"]
              [(= klass "module")
               "module"]
              [True
               "instance"])))

  (defn annotate [self]
    "Return annotation for a candidate."
    (setv obj
          (.evaled? self))

    (setv annotation
          (cond [(not (none? obj))  ; Obj could be instance of bool
                 (.-translate-class self obj.--class--.--name--)]
                ;; Shadow takes priority over compiler annotations
                [(.shadow? self)
                 "shadowed"]
                [(.compiler? self)
                 "compiler"]
                [(.macro? self)
                 "macro"]))

    (.format "<{} {}>" annotation self)))

;; * Prefix

(defclass Prefix [object]
  "A completion candidate."

  (defn --init-- [self prefix &optional namespace]
    (setv self.prefix prefix)
    (setv self.namespace
          (or namespace (Namespace)))

    (setv [self.candidate
           self.attr-prefix]
          (.split-prefix self prefix self.namespace)))

  (defn --repr-- [self]
    (.format "Prefix<(prefix={})>" self.prefix))

  #@(staticmethod
      (defn split-prefix [prefix namespace]
        "Split prefix on last dot accessor, returning an obj, attr pair."
        (setv components
              (.split prefix "."))

        [(->> components
           butlast
           (.join ".")
           (Candidate :namespace namespace))
         (->> components
           last
           hy-symbol-unmangle
           ;; Hy-symbol-unmangle is inconsistent in case of just "_"
           ;; This is due to custom of using "_" as the last shell prompt return
           ;; However it is important it is mangled to "-" in the case of
           ;; eg. `print._` to complete all the dunder methods.
           ;; This only matters for the `attr-prefix` so we do not need
           ;; to use our own version in all places of `hy-symbol-unmangle`.
           (#%(if (= %1 "_") "-" %1)))]))

  (defn complete [self]
    "Get candidates for a given Prefix."
    (setv candidates
          (or (.attributes self.candidate) self.namespace.names))

    (some->> candidates
      (filter #%(.startswith %1 self.attr-prefix))
      (map #%(if self.candidate
                 (+ (str self.candidate) "." %1)
                 %1))
      tuple)))
