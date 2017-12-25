(require [src.utils.macros [*]])
(require [hy.extra.anaphoric [*]])
(import
  inspect

  hy
  [hy.lex.parser [hy-symbol-unmangle]]

  ;; [src.utils.docstrings [builtin-docs-to-lispy-docs]]
  )

;; * Parameters

(defclass Parameter [object]
  (defn --init-- [self symbol &optional default]
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
         (except [e TypeError]
           (raise (TypeError "Unsupported callable for Signature."))))

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
           (if (and formatted-argspec args) " " "")
           (cls.-format-args args opener))))

  (defn --str-- [self]
    (reduce self.-acc-lispy-repr
            [[self.args None]
             [self.defaults "&optional"]
             [self.varargs "#*"]
             [self.varkw "#**"]
             [self.kwargs "&kwonly"]]
            "")))

;; * Inspect
;; ** Internal

(defclass Inspect [object]
  (defn --init-- [self obj]
    (setv self.obj obj))

  (defn -docs-first-line [self]
    (or (-> self.obj.--doc-- (.splitlines) first) ""))

  #@(property
      (defn -args-docs-delim [self]
        (or (and self.obj.--doc-- " - ") "")))

  (defn -cut-obj-name-maybe [self docs]
    (if (or self.class? self.method-wrapper?)
        (-> docs
           (.replace "self " "")
           (.replace "self" ""))
        docs))

  (defn -cut-method-wrapper-maybe [self docs]
    (if self.method-wrapper?
        (+ "method-wrapper"
           (cut docs (.index docs ":")))
        docs))

  (defn -format-docs [self docs]
    (-> docs
       self.-cut-obj-name-maybe
       self.-cut-method-wrapper-maybe))

;; ** Properties

  #@(property
      (defn obj-name [self]
        (if self.lambda?
            "<lambda>"
            (hy-symbol-unmangle func.--name--))))

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

;; ** Actions

  (defn docs [self]
    (setv signature
          (.signature self))

    (self.-format-docs
      (if signature
          (.format "{name}: ({args}){delim}{docs}"
                   :name self.obj-name
                   :args signature
                   :delim self.-args-docs-delim
                   :docs self.-docs-first-line)
          (builtin-docs-to-lispy-docs self.-docs-first-line)))))
