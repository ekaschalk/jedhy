(import
  builtins inspect types

  hy hy.compiler hy.macros

  [src.utils.format [name-or-string]]
  ;; [hy.core.shadow [*]] [hy.core.language [*]]
  )

(hy.eval '(import hy.macros))  ; See https://github.com/hylang/hy/issues/1467


;; * Candidates

(defclass Candidate [object]
  (defn --init-- [self symbol]
    (setv self.symbol
          (hy-symbol-unmangle symbol))
    (setv self.mangled
          (hy-symbol-mangle symbol)))

  (defn compiler? [self]
    "Is candidate a compile table construct?"
    (in self.symbol hy.compiler.-compile-table))

  (defn macro? [self]
    "Is candidate a macro?"
    (in self.mangled (get hy.macros.-hy-macros None)))

  (defn shadow? [self]
    "Is candidate a shadowed operator?"
    (in self.mangled (dir hy.core.shadow)))

  (defn evaled [self]
    "Try to return evaluated candidate."
    (try (builtins.eval self.mangled (globals))
         (except [e NameError]
           None)))
  )

;; * Prefix

(defclass Prefix [object]
  (defn --init-- [self prefix]
    (setv self.prefix prefix))
  )


(defclass Candidates [object]
  (defn --init-- [self candidates]
    (setv self.candidates candidates))

  #@(classmethod
      (defn build [cls]
        (setv current-locals
              (.keys (locals)))
        (setv compiler-forms
              hy.compiler.-compile-table)
        (setv macros
              (->> hy.macros.-hy-macros
                 (.values)
                 (map dict.keys)))

        (setv candidates
              (->> (chain current-locals compiler-forms macros)
                 flatten
                 (map (comp hy-symbol-unmangle name-or-string))
                 distinct
                 list))

        (cls candidates)))

  (defn attrs-for [self candidate]
    (setv obj
          (.evaled candidate))

    (when obj
      #t(-> obj dir (map hy-symbol-unmangle)))))

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
              (.evaled candidate))

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
;; ** Internal

(defclass Signature [object]
  (defn --init-- [self func]
    (setv argspec
          (inspect.getfullargspec func))
    (setv [args defaults kwargs]
          ((juxt cls.-args-from cls.-defaults-from cls.-kwargs-from)
            argspec))

    (setv self.func func)
    (setv self.args args)
    (setv self.defaults defaults)
    (setv self.kwargs kwargs)
    (setv self.varargs argspec.varargs)
    (setv self.varkw argspec.varkw))

  #@(staticmethod
      (defn -args-from [argspec]
        (setv args
              (if (and argspec.args argspec.defaults)
                  (-> argspec.defaults len (drop-last argspec.args) list)
                  argspec.args))

        (when args
          #t(map Parameter args))))

  #@(classmethod
      (defn -defaults-from [cls argspec]
        (setv args-with-defaults
              (if (and argspec.args argspec.defaults)
                  (-> argspec cls.-args-from len (drop argspec.args) list)
                  argspec.defaults))

        (when args-with-defaults
          #t(map Parameter args-with-defaults argspec.defaults))))

  #@(staticmethod
      (defn -kwargsonly-from [argspec]
        (setv kwonlyargs
              (if (and argspec.kwonlyargs argspec.kwonlydefaults)
                  (->> argspec.kwonlyargs
                     (remove (fn [x] (in x (.keys argspec.kwonlydefaults))))
                     list)
                  argspec.kwonlyargs))

        (when argspec.kwonlyargs
          #t(map Parameter kwonlyargs))))

  #@(staticmethod
      (defn -kwonlydefaults-from [argspec]
        (when argspec.kwonlydefaults
          #t(*map Parameter (.items argspec.kwonlydefaults)))))

  #@(classmethod
      (defn -kwargs-from [cls argspec]
        (-> argspec
           ((juxt cls.-kwargsonly-from cls.-kwonlydefaults-from) argspec)
           flatten)))

;; ** Formatting

  #@(staticmethod
      (defn -format-args [args opener]
        (if args
            (+ (if opener
                   (+ opener " ")
                   (str))
               (.join " " args))
            (str))))

  #@(classmethod
      (defn -acc-lispy-repr [cls formatted-argspec [args opener]]
        ;; Want list of all None to fail on conditionals just like single None
        (when args
          (setv args
                #t(remove none? args)))

        (+ formatted-argspec
           (if (and formatted-argspec args) " " (str))
           (cls.-format-args args opener))))

;; ** Exposes

  (defn lispy-format [self]
    (reduce self.-acc-lispy-repr
            [[self.args None]
             [self.defaults "&optional"]
             [[self.varargs] "#*"]
             [[self.varkw] "#**"]
             [self.kwargs "&kwonly"]]
            (str))))
