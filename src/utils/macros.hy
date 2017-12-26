(require [hy.extra.anaphoric [*]])

(import functools)

;; * Tag Macros

(deftag t [form]
  "Cast evaluated form to a tuple. Useful via eg. #t(-> x f1 f2 ...)."
  `(tuple ~form))

(deftag $ [form]
  "Partially apply a form eg. (#$(map inc) [1 2 3])."
  `(functools.partial ~@form))

;; * FP Macros

(defmacro fn-> [&rest code]
  "Thread first an anonymous function."
  `#%(-> %1 ~@code))

(defmacro fn->> [&rest code]
  "Thread last an anonymous function."
  `#%(->> %1 ~@code))

;; * Some-> and Some->> threads

(defmacro -opener-or-none-first [opener]
  `(fn [&rest x &kwargs y]
     (if (none? (first x))
         None
         ;; Fancy way of handling the dot-dsl
         ;; Just (~opener #* x #** y) will cause a compiler error
         ;; as we can't unpack the .method
         (-> x first (~opener #* (rest x) #** y)))))

(defmacro -opener-or-none-last [opener]
  `(fn [&rest x &kwargs y]
     (if (none? (last x))
         None
         (-> x first (~opener #* (rest x) #** y)))))

(defmacro some-> [head &rest forms]
  (setv evaled `(~head))
  (setv ret `(if (none? ~@evaled) None ~@evaled))

  (unless (none? `(~ret))
    (for [node forms]
      (unless (isinstance node HyExpression)
        (setv node `(~node)))

      (.insert node 1 ret)

      (setv ret
            `((-opener-or-none-first ~(first node)) ~@(rest node)))))
  ret)

(defmacro some->> [head &rest forms]
  (setv evaled `(~head))
  (setv ret `(if (none? ~@evaled) None ~@evaled))

  (unless (none? `(~ret))
    (for [node forms]
      (unless (isinstance node HyExpression)
        (setv node `(~node)))

      (.append node ret)

      (setv ret
            `((-opener-or-none-last ~(first node)) ~@(rest node)))))
  ret)
