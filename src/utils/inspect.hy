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

(defclass Signature [object]
  (defn --init-- [self args kwargs varargs varkw]
    (setv self.args args)
    (setv self.kwargs kwargs)
    (setv self.varargs varargs)
    (setv self.varkw varkw))

  #@(staticmethod
      (defn -args-no-defaults-from [argspec]
        (setv args
              (if (and argspec.args argspec.defaults)
                  (-> argspec.defaults len (drop-last argspec.args) list)
                  argspec.args))

        (when args
          #t(map Parameter args))))

  #@(classmethod
      (defn -args-with-defaults-from [cls argspec]
        (setv args-with-defaults
              (if (and argspec.args argspec.defaults)
                  (-> argspec cls.-args-from len (drop argspec.args) list)
                  argspec.defaults))

        (when args-with-defaults
          #t(map Parameter args-with-defaults argspec.defaults))))

  #@(staticmethod
      (defn -args-from [argspec]))

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

  )
