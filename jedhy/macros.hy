(require [hy.extra.anaphoric [*]])

(import functools
        [toolz.curried :as tz])

;; * Tag Macros

(deftag t [form]
  "Cast evaluated form to a tuple. Useful via eg. #t(-> x f1 f2 ...)."
  `(tuple ~form))

(deftag $ [form]
  "Partially apply a form eg. (#$(map inc) [1 2 3])."
  `(functools.partial ~@form))

(deftag f [form]
  "Flipped #$."
  `(tz.flip ~@form))

;; * FP Macros

(defmacro fn-> [&rest code]
  "Thread first an anonymous function."
  `#%(-> %1 ~@code))

(defmacro fn->> [&rest code]
  "Thread last an anonymous function."
  `#%(->> %1 ~@code))

;; * Misc

(defn -allkeys [d &kwonly [parents (,)]]
  "In-order tuples of keys of nested, variable-length dict."
  (if (isinstance d (, list tuple))
      []
      #t(->> d
         (tz.keymap (fn [k] (+ parents (, k))))
         dict.items
         (*map (fn [k v]
                 (if (isinstance v dict)
                     (-allkeys v :parents k)
                     [k])))
         tz.concat)))

(defn allkeys [d]
  (->> d
    -allkeys
    (map last)
    tuple))
