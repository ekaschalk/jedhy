(import
  builtins inspect

  hy hy.compiler hy.macros
  ;; [hy.core.shadow [*]] [hy.core.language [*]]

  [src.docstrings [builtin-docs-to-lispy-docs]])
(require [src.utils.macros [*]])


;; * Candidate

(defclass Candidate [object]
  (defn --init-- [self symbol]
    (setv self.symbol
          (hy-symbol-unmangle symbol))
    (setv self.mangled
          (hy-symbol-mangle symbol)))

  (defn compiler? [self]
    "Is candidate a compile table construct and return it."
    (try (get hy.compiler.-compile-table self.symbol)
         (except [e KeyError] None)))

  (defn macro? [self]
    "Is candidate a macro and return it."
    (hy.eval '(import hy.macros))  ; See https://github.com/hylang/hy/issues/1467
    (try (get hy.macros.-hy-macros None self.mangled)
         (except [e KeyError] None)))

  (defn shadow? [self]
    "Is candidate a shadowed operator and return it."
    (try (get (dir hy.core.shadow) self.mangled)
         (except [e KeyError] None)))

  (defn evaled? [self]
    "Is candidate evaluatable and return it."
    (try (builtins.eval self.mangled (globals))
         (except [e NameError] None)))

  (defn get-obj [self]
    "Get object for underlying candidate."
    (or (.macro? self) (.evaled? self))))

;; * Prefix

(defclass Prefix [object]
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
         (->> components last)]))

;; * Candidates

(defclass Candidates [object]
  (defn --init-- [self]
    (setv self.candidates (.-collect-candidates self)))

  #@(staticmethod
      (defn -collect-candidates []
        #t(->> hy.macros.-hy-macros
            (.values)
            (map dict.keys)
            (chain (.keys locals) hy.compiler.-compile-table)
            flatten
            (map #%(if (instance? str %1) %1 %1.--name--))
            (map hy-symbol-unmangle)
            distinct)))

  (defn -dir-of [self candidate]
    #t(some-> candidate
           (.evaled?)
           dir
           (map hy-symbol-unmangle)))

  (defn --call-- [self prefix]
    #t(some->> self.candidates
            (or (.dir-of self.prefix.candidate))
            (filter #%(.startswith %1 prefix.attr-prefix))
            (map #$(+ candidate ".")))))

;; * Annotations

(defclass Annotation [object]
  (defn --init-- [self candidate]
    (setv self.candidate candidate)
    (setv self.annotation (.annotate self candidate)))

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

  #@(classmethod
      (defn annotate [cls candidate]
        "Return annotation for a candidate."
        (setv obj
              (.evaled? candidate))

        ;; Ordered by lookup speed and expected frequency
        (cond [obj
               (cls.-translate-class obj.--class--.--name--)]
              [(.compiler? candidate)
               "compiler"]
              [(.shadow? candidate)
               "shadowed"]
              [(.macro? candidate)
               "macro"])))

  (defn --str-- [self]
    "Format an annotation for company display."
    (if self.annotation
        (.format "<{} {}>" self.annotation self.candidate)
        "")))

;; * Parameters

(defclass Paramater [object]
  (def --init-- [self symbol &optional default]
    (setv self.symbol symbol)
    (setv self.default default))

  (defn --str-- [self]
    (if self.default
        self.symbol
        (.format "[{} {}]" self.symbol self.default))))

;; * Signature

(defclass Signature [object]
  (defn --init-- [self func]
    (try (setv argspec
               (inspect.getfullargspec func))
         (except (raise TypeError "Unsupported callable for Signature.")))

    (setv [args defaults kwargs]
          ((juxt cls.-args-from cls.-defaults-from cls.-kwargs-from) argspec))

    (setv self.func func)
    (setv self.args args)
    (setv self.defaults defaults)
    (setv self.kwargs kwargs)
    (setv self.varargs (and argspec.varargs [argspec.varargs]))
    (setv self.varkw (and argspec.varkw [argspec.varkw])))

  #@(staticmethod
      (defn -args-from [argspec]
        #t(some->>
          (-> argspec.defaults len (drop-last argspec.args) list)
          (or argspec.args argspec.defaults)
          (map Parameter))))

  #@(classmethod
      (defn -defaults-from [cls argspec]
        #t(some->>
          (-> argspec cls.-args-from len (drop argspec.args) list)
          (or argspec.args argspec.defaults)
          (#%(map Parameter %1 argspec.defaults)))))

  #@(staticmethod
      (defn -kwargsonly-from [argspec]
        #t(some->>
           (remove #%(in %1 (.keys argspec.kwonlydefaults)) argspec.kwonlyargs)
           (or argspec.kwonlyargs argspec.kwonlydefaults)
           (map Parameter))))

  #@(staticmethod
      (defn -kwonlydefaults-from [argspec]
        #t(some->>
           argspec.kwonlydefaults
           (.items)
           (*map Parameter))))

  #@(classmethod
      (defn -kwargs-from [cls argspec]
        (-> argspec
           ((juxt cls.-kwargsonly-from cls.-kwonlydefaults-from) argspec)
           flatten)))

  #@(staticmethod
      (defn -format-args [args opener]
        (unless args
          (return ""))

        (setv opener
              (if opener (+ opener " ") ""))

        (->> args
           (.join " ")
           (+ opener))))

  #@(classmethod
      (defn -acc-lispy-repr [cls formatted-argspec [args opener]]
        (+ formatted-argspec
           (if (and formatted-argspec args) " " (str))
           (cls.-format-args args opener))))

  (defn --str-- [self]
    (reduce self.-acc-lispy-repr
            [[self.args None]
             [self.defaults "&optional"]
             [self.varargs "#*"]
             [self.varkw "#**"]
             [self.kwargs "&kwonly"]]
            "")))

;; * Introspect
;; ** Internal

(defclass Introspect [object]
  (defn --init-- [self obj]
    (setv self.obj obj))

  (defn -docs-first-line [self]
    (or (-> self.obj.--doc-- (.splitlines) first) ""))

  #@(property
      (defn obj-name [self]
        (if self.lambda?
            "<lambda>"
            (hy-symbol-unmangle func.--name--))))

  #@(property
      (defn -args-docs-delim [self]
        (or "" (and self.obj.--doc-- " - "))))

  (defn -cut-self-maybe [self docs]
    (when (or self.class? self.method-wrapper?)
      (-> docs
         (.replace "self " "")
         (.replace "self" "")))
    docs)

  (defn -cut-obj-name-maybe [self docs]
    (when self.method-wrapper?
      (+ "method-wrapper"
         (cut docs (.index docs ":"))))
    docs)

;; ** Properties

  #@(property
      (defn lambda? [self]
        "Is object a lambda?"
        (= self.obj.--name-- "lambda")))

  #@(property
      (defn class? [self]
        "Is object a class?"
        (inspect.isclass self.obj)))

  #@(property
      (defn method-wrapper? [self]
        "Is object of type 'method-wrapper'?"
        (instance? (type print.--str--) self.obj)))

  (defn signature [self]
    "Return object's signature if it exists."
    (try (Signature self.obj)
         (except [e TypeError] None)))

;; ** Eldoc

  (defn raw-eldoc [self]
    (setv signature
          (.signature self))

    (if signature
        (.format "{name}: ({args}){delim}{docs}"
                 :name self.obj-name
                 :args signature
                 :delim self.-args-docs-delim
                 :docs self.-docs-first-line)
        (builtin-docs-to-lispy-docs self.-docs-first-line)))

  (defn eldoc [self]
    (-> (.raw-eldoc self)
       self.-cut-self-maybe
       self.-cut-obj-name-maybe)))
