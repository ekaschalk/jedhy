"Implements Namespace-dependent methods and structures."

;; * Imports

(require [jedhy.macros [*]])
(import [jedhy.macros [*]])
(import builtins

        hy
        hy.compiler
        hy.macros
        ;; [hy.compiler [_special_form_compilers :as -compile-table]]

        ;; Below imports populate __macros__ for Namespace
        [hy.core.language [*]]
        [hy.core.macros [*]])

;; * Fixes

;; See this issue for below #1467: https://github.com/hylang/hy/issues/1467
(hy.eval `(import hy.macros))
(hy.eval `(require [hy.extra.anaphoric [*]]))
;; Overwrite Hy's mangling
(import [jedhy.macros [mangle]])

;; * Namespace

(defclass Namespace [object]
  (defn __init__ [self [globals- None] [locals- None] [macros- None]]
    ;; Components
    (setv self.globals       (or globals- (globals)))
    (setv self.locals        (or locals- (locals)))
    (setv self.macros        (tz.keymap unmangle (or macros- __macros__)))
    ;; (setv self.compile-table (self.-collect-compile-table))
    (setv self.shadows       (self.-collect-shadows))

    ;; Collected
    (setv self.names (self.-collect-names)))

  #@(staticmethod
      (defn -to-names [key]
        "Function for converting keys (strs, functions, modules...) to names."
        (unmangle (if (string? key)
                      key
                      key.__name__))))

  (defn -collect-compile-table [self]
    "Collect compile table as dict."
    (->> -compile-table
       (tz.keymap self.-to-names)))

  (defn -collect-shadows [self]
    "Collect shadows as a list, purely for annotation checks."
    (->> hy.core.shadow
      dir
      (map self.-to-names)
      tuple))

  (defn -collect-names [self]
    "Collect all names from all places."
    (->>
      (chain (allkeys self.globals)
             (allkeys self.locals)
             (.keys self.macros)
            ;;  (.keys self.compile-table)
             )
      (map self.-to-names)
      distinct
      tuple))

  (defn eval [self mangled-symbol]
    "Evaluate `mangled-symbol' within the Namespace."
    ;; Short circuit a common case (completing without "." present at all)
    (when (not mangled-symbol)
      (return None))

    (setv hy-tree (read-str mangled-symbol))

    (try (hy.eval hy-tree :locals self.globals)
         (except [e NameError]
           (try (hy.eval hy-tree :locals self.locals)
                (except [] None))))))

;; * Candidate

(defclass Candidate [object]
  (defn __init__ [self symbol [namespace None]]
    (setv self.symbol    (unmangle symbol))
    (setv self.mangled   (mangle symbol))
    (setv self.namespace (or namespace (Namespace))))

  (defn __str__ [self]
    self.symbol)

  (defn __repr__ [self]
    (.format "Candidate<(symbol={}>)" self.symbol))

  (defn __eq__ [self other]
    (when (isinstance other Candidate)
      (= self.symbol other.symbol)))

  (defn __bool__ [self]
    (bool self.symbol))

  ;; (defn compiler? [self]
  ;;   "Is candidate a compile table construct and return it."
  ;;   (try (get self.namespace.compile-table self.symbol)
  ;;        (except [e KeyError] None)))

  (defn macro? [self]
    "Is candidate a macro and return it."
    (try (get self.namespace.macros self.symbol)
         (except [e KeyError] None)))

  (defn evaled? [self]
    "Is candidate evaluatable and return it."
    (try (.eval self.namespace self.symbol)
         (except [e Exception] None)))

  (defn shadow? [self]
    "Is candidate a shadowed operator, do *not* return it."
    (or (in self.symbol self.namespace.shadows)
        None))

  (defn get-obj [self]
    "Get object for underlying candidate."
    ;; Compiler *must* come after .evaled to catch objects that are
    ;; both shadowed and in the compile table as shadowed (eg. `+`)
    (or (self.macro?)
        (self.evaled?)
        ;; (self.compiler?)
        ))

  (defn attributes [self]
    "Return attributes for obj if they exist."
    (setv obj (self.evaled?))

    (when obj
      (->> obj dir (map unmangle) tuple)))

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
    (setv obj (self.evaled?))
    (setv obj? (not (is obj None)))  ; Obj could be instance of bool

    ;; Shadowed takes first priority but compile table takes last priority
    (setv annotation (cond [(self.shadow?)
                            "shadowed"]

                           [obj?
                            (self.-translate-class obj.__class__.__name__)]

                          ;;  [(.compiler? self)
                          ;;   "compiler"]

                           [(.macro? self)
                            "macro"]))

    (.format "<{} {}>" annotation self)))

;; * Prefix

(defclass Prefix [object]
  "A completion candidate."

  (defn __init__ [self prefix [namespace None]]
    (setv self.prefix prefix)
    (setv self.namespace (or namespace (Namespace)))

    (setv self.candidate (self.-prefix->candidate prefix self.namespace))
    (setv self.attr-prefix (self.-prefix->attr-prefix prefix))

    (setv self.completions (tuple)))

  (defn __repr__ [self]
    (.format "Prefix<(prefix={})>" self.prefix))

  #@(staticmethod
      (defn -prefix->candidate [prefix namespace]
        (->> (.split prefix ".")
             butlast
             (.join ".")
             (Candidate :namespace namespace))))

  #@(staticmethod
      (defn -prefix->attr-prefix [prefix]
        "Get prefix as str of everything after last dot if a dot is there."
        (->> (.split prefix ".")
           last
           unmangle
           ;; TODO since 0.15 below line shouldnt be needed
           (#%(if (= %1 "_") "-" %1)))))

  #@(property
      (defn has-attr? [self]
        "Does prefix reference an attr?"
        (in "." self.prefix)))

  #@(property
      (defn obj? [self]
        "Is the prefix's candidate an object?"
        (bool (.get-obj self.candidate))))

  (defn complete-candidate [self completion]
    "Given a potential string `completion`, attach to candidate."
    (if self.candidate
        (+ (str self.candidate) "." completion)
        completion))

  (defn complete [self [cached-prefix None]]
    "Get candidates for a given Prefix."
    ;; Short circuit the case: "1+nonsense.real-attr" eg. "foo.__prin"
    (when (and self.has-attr?  ; The and ordering here matters for speed
               (not self.obj?))
      (setv self.completions (tuple))
      (return self.completions))

    ;; Complete on relevant top-level names or candidate-dependent names
    (if (and cached-prefix
             (= self.candidate cached-prefix.candidate))
        (setv self.completions cached-prefix.completions)
        (setv self.completions (or (.attributes self.candidate)
                                   self.namespace.names)))

    (->> self.completions
       (filter #f(str.startswith self.attr-prefix))
       (map self.complete-candidate)
       tuple)))
