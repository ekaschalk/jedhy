(import
  builtins inspect types

  hy hy.compiler hy.macros

  ;; [hy.core.shadow [*]] [hy.core.language [*]]
  )

;; See https://github.com/hylang/hy/issues/1467
(hy.eval '(import hy.macros))


;; * Predicates

(defn module?)
(defn class?)
(defn method?)
(defn function?)
(defn macro?)

;; * Source

(defn getdoc)
(defn getfile)
(defn getmodule)
(defn getsourcelines)

;; * Inspection

(defclass Paramater [object]
  (def --init-- [self name &optional default]
    (setv self.name name)
    (setv self.default default))

  (defn --str-- [self]
    (if self.default
        self.name
        (.format "[{} {}]" self.name self.default))))

;; * Signature
;; ** Internal

(defclass Signature [object]
  (defn --init-- [self name args defaults kwargs varargs varkw]
    (setv self.func func)
    (setv self.args args)
    (setv self.defaults defaults)
    (setv self.kwargs kwargs)
    (setv self.varargs varargs)
    (setv self.varkw varkw))

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
            (str)))

  #@(classmethod
      (defn build [cls func]
        (setv argspec
              (inspect.getfullargspec func))
        (setv [args defaults kwargs]
              ((juxt cls.-args-from cls.-defaults-from cls.-kwargs-from)
                argspec))

        (cls func args defaults kwargs
             argspec.varargs argspec.varkw)))

  )
