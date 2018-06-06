(require [jedhy.macros [*]])
(import [jedhy.macros [*]])
(require [hy.extra.anaphoric [*]])
(import inspect hy)

(try
  (import [hy.lex.parser [hy-symbol-unmangle]])
  (except [e ImportError]
    (import [hy.lex.parser [unmangle :as hy-symbol-unmangle]])))



;; * Parameters

(defclass Parameter [object]
  (defn --init-- [self symbol &optional default]
    (setv self.symbol (hy-symbol-unmangle symbol))
    (setv self.default default))

  (defn --str-- [self]
    (if (none? self.default)
        self.symbol
        (.format "[{} {}]" self.symbol self.default))))

;; * Signature

(defclass Signature [object]
  (defn --init-- [self func]
    (try (setv argspec
               (inspect.getfullargspec func))
         (except [e TypeError]
           (raise (TypeError "Unsupported callable for hy Signature."))))

    (setv [args defaults kwargs]
          ((juxt self.-args-from self.-defaults-from self.-kwargs-from)
            argspec))

    (setv self.func func)
    (setv self.args args)
    (setv self.defaults defaults)
    (setv self.kwargs kwargs)
    (setv self.varargs (and argspec.varargs [(hy-symbol-unmangle argspec.varargs)]))
    (setv self.varkw (and argspec.varkw [(hy-symbol-unmangle argspec.varkw)])))

  #@(staticmethod
      (defn -parametrize [symbols &optional defaults]
        "Construct many Parameter for `symbols` with possibly defaults."
        (when symbols
          (tuple (map Parameter
                      symbols
                      (or defaults (repeat None)))))))

  #@(classmethod
      (defn -args-from [cls argspec]
        "Extract args without defined defaults from `argspec`."
        (setv symbols
              (drop-last (len (or argspec.defaults []))
                         argspec.args))

        (cls.-parametrize symbols)))

  #@(classmethod
      (defn -defaults-from [cls argspec]
        "Extract args with defined defaults from `argspec`."
        (setv args-without-defaults
              (cls.-args-from argspec))
        (setv symbols
              (drop (len args-without-defaults)
                    argspec.args))

        (cls.-parametrize symbols argspec.defaults)))

  #@(classmethod
      (defn -kwargsonly-from [cls argspec]
        "Extract kwargs without defined defaults from `argspec`."
        (setv kwargs-with-defaults
              (.keys (or argspec.kwonlydefaults {})))
        (setv symbols
              (remove #f(in kwargs-with-defaults) argspec.kwonlyargs))

        (cls.-parametrize symbols)))

  #@(classmethod
      (defn -kwonlydefaults-from [cls argspec]
        "Extract kwargs with defined defaults from `argspec`."
        (when argspec.kwonlydefaults
          (setv [symbols defaults]
                (zip #* (.items argspec.kwonlydefaults)))

          (cls.-parametrize symbols defaults))))

  #@(classmethod
      (defn -kwargs-from [cls argspec]
        "Chain kwargs with and without defaults, since `argspec` doesn't order."
        (->> argspec
          ((juxt cls.-kwargsonly-from cls.-kwonlydefaults-from))
          flatten
          (remove none?)
          tuple)))

  #@(staticmethod
      (defn -format-args [args opener]
        (unless args
          (return ""))

        (setv opener
              (if opener (+ opener " ") ""))

        (+ opener
           (.join " "
                  (map str args)))))

  #@(classmethod
      (defn -acc-lispy-repr [cls acc args-opener]
        (setv [args opener] args-opener)
        (setv delim
              (if (and acc args) " " ""))

        (+ acc delim (cls.-format-args args opener))))

  #@(property
      (defn -arg-opener-pairs [self]
        [[self.args None]
         [self.defaults "&optional"]
         [self.varargs "#*"]
         [self.varkw "#**"]
         [self.kwargs "&kwonly"]]))

  (defn --str-- [self]
    (reduce self.-acc-lispy-repr self.-arg-opener-pairs "")))

;; * Docstring conversion

(defn -split-docs [docs]
  "Partition docs string into pre/-/post-args strings."
  (setv [start-args end-args]
        [(inc (.index docs "("))
         (.index docs ")")])

  [(cut docs 0 start-args)
   (cut docs start-args end-args)
   (cut docs end-args)])

(defn -argstring-to-param [arg-string]
  "Convert an arg string to a Parameter."
  (unless (in "=" arg-string)
    (return (Parameter arg-string)))

  (setv [arg-name - default]
        (.partition arg-string "="))

  (if (= "None" default)
      (Parameter arg-name)
      (Parameter arg-name default)))

(defn -optional-arg-idx [args-strings]
  "First idx of an arg with a default in list of args strings."
  (defn -at-arg-with-default? [idx-arg]
    (when (in "=" (second idx-arg)) (first idx-arg)))

  (->> args-strings
    enumerate
    (map -at-arg-with-default?)
    (remove none?)  ; Can't use `some` since idx could be zero
    first))

(defn -insert-optional [args]
  "Insert &optional into list of args strings."
  (setv optional-idx
        (-optional-arg-idx args))

  (unless (none? optional-idx)
    (.insert args optional-idx "&optional"))

  args)

(defn builtin-docs-to-lispy-docs [docs]
  "Convert built-in-styled docs string into a lispy-format."
  ;; Check if docs is non-standard
  (unless (and (in "(" docs) (in ")" docs))
    (return docs))

  (setv replacements
        [["..." "#* args"]
         ["*args" "#* args"]
         ["**kwargs" "#** kwargs"]
         ["\n" "newline"]
         ["-->" "- return"]])

  (setv [pre-args - post-args]
        (.partition docs "("))

  ;; Format before args and perform unconditional conversions
  (setv [pre-args args post-args]
        (->> post-args
          (.format "{}: ({}" pre-args)
          (reduce (fn [s old-new]
                    (.replace s (first old-new) (second old-new)))
                  replacements)
          -split-docs))

  ;; Format and reorder args and reconstruct the string
  (+ pre-args
     (as-> args args
          (.split args ",")
          (map str.strip args)
          (list args)
          (-insert-optional args)
          (map (comp str -argstring-to-param) args)
          (.join " " args))
     post-args))

;; * Inspect
;; ** Internal

(defclass Inspect [object]
  (defn --init-- [self obj]
    (setv self.obj obj))

  #@(property
      (defn -docs-first-line [self]
        (or (and self.obj.--doc-- (-> self.obj.--doc-- (.splitlines) first)) "")))

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
        (hy-symbol-unmangle self.obj.--name--)))

  #@(property
      (defn lambda? [self]
        "Is object a lambda?"
        (= self.obj-name "<lambda>")))

  #@(property
      (defn class? [self]
        "Is object a class?"
        (inspect.isclass self.obj)))

  #@(property
      (defn method-wrapper? [self]
        "Is object of type 'method-wrapper'?"
        (instance? (type print.--str--) self.obj)))

;; ** Actions

  (defn signature [self]
    "Return object's signature if it exists."
    (try (Signature self.obj)
         (except [e TypeError] None)))

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
