"Convert formatting of builtins docstrings first-line to lispy representation."

(require [src.utils.macros [*]])

;; TODO Requires `Parameter`

(defn -split-docs [docs]
  "Partition docs string into pre/-/post-args strings."
  (setv [start-args
         end-args]
        [(inc (.index docs "("))
         (.index docs ")")])

  [(cut docs 0 start-args)
   (cut docs start-args end-args)
   (cut docs end-args)])

(defn -argstring-to-param [arg]
  "Convert an arg string to a Parameter."
  (unless (in "=" arg)
    (return (Parameter arg)))

  (setv [arg-name - default]
        (.partition arg "="))
  (if (= "None" default)
      (Parameter arg-name)
      (Parameter arg-name default)))

(defn -optional-arg-idx [args]
  "First idx of an arg with a default in list of args strings."
  (defn -at-arg-with-default? [[idx arg]]
    (and (in "=" arg) idx))

  (some -at-arg-with-default? (enumerate args)))

(defn -insert-optional [args]
  "Insert &optional into list of args strings."
  (setv optional-idx
        (-optional-arg-idx args))
  (unless (none? optional-idx)
    (.insert args optional-idx "&optional"))
  args)

(defn builtin-docs-to-lispy-docs [docs]
  "Convert built-in-styled docs string into a lispy-format."
  (setv [pre-args - post-args]
        (.partition docs "("))

  (setv docs
        (.format "{}: ({}" pre-args post-args))
  (setv docs
        (*map docs.replace
              (->
                [["..." "#* args"]
                 ["*args" "#* args"]
                 ["**kwargs" "#** kwargs"]
                 ["\n" "newline"]
                 ["-->" "return"]]
                zip chain.from-iterable)))

  (setv [pre-args args post-args]
        (-split-docs docs))

  (setv formatted-args
        (->> (.split args ",")
           (map str.strip)
           list
           -insert-optional
           (map -argstring-to-param)
           (map str)
           (#$(str.join " "))))

  (+ pre-args args post-args))
