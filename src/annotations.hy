(require [src.utils.macros [*]])

(defn -translate-class [klass]
  "Return annotation given a name of a class."
  (cond [(in klass ["function" "builtin_function_or_method"])
         "def"]
        [(= klass "type")
         "class"]
        [(= klass "module")
         "module"]
        [True
         "instance"]))

(defn annotate [candidate]
  "Return annotation for a candidate."
  (setv obj
        (.evaled? candidate))

  ;; Ordered by lookup speed and rough expected frequency
  (setv annotation
        (cond [obj
               (-translate-class obj.--class--.--name--)]
              [(.compiler? candidate)
               "compiler"]
              [(.shadow? candidate)
               "shadowed"]
              [(.macro? candidate)
               "macro"]))

  (.format "<{} {}>" annotation candidate))
