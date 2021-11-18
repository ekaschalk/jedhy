"Some general purpose imports and code."

;; * Imports

(require [hy.extra.anaphoric [*]])

(import functools
        itertools
        [toolz.curried :as tz]

        [hy.lex [unmangle
                 mangle :as unfixed-mangle]])


;; * 1.0a3 workaround
(defn second [form]
  (get form 1))

(defn first [form]
  (if form (get form 0)))

(defn none? [form]
  (is form None))

(defn string? [form]
  (isinstance form str))

;; * Hy Overwrites

;; We have different requirements in the empty string case
(defn mangle [s]
  (if (!= s "") (unfixed-mangle s) (str)))

;; * Tag Macros

(defmacro "#t" [form]
  "Cast evaluated form to a tuple. Useful via eg. #t(-> x f1 f2 ...)."
  `(tuple ~form))

(defmacro "#$" [form]
  "Partially apply a form eg. (#$(map inc) [1 2 3])."
  `(functools.partial ~@form))

(defmacro "#f" [form]
  "Flipped #$."
  `(tz.flip ~@form))

;; * Misc

(defn -allkeys [d * [parents (,)]]
  "In-order tuples of keys of nested, variable-length dict."
  (if (isinstance d (, list tuple))
      []
      #t(->> d
         (tz.keymap (fn [k] (+ parents (, k))))
         dict.items
         (itertools.starmap
           (fn [k v]
             (if (isinstance v dict)
               (-allkeys v :parents k)
               [k])))
         tz.concat)))

(defn drop [count coll]
  "Drop `count` elements from `coll` and yield back the rest."
  (islice coll count None))

(defn first [coll]
  "Return first item from `coll`."
  (next (iter coll) None))

(defn last [coll]
  "Return last item from `coll`."
  (get (tuple coll) -1))

(defn allkeys [d]
  (->> d -allkeys (map last) tuple))

(defn juxt [f #* fs]
  "Return a function applying each `fs` to args, collecting results in a list."
  (setv fs (+ (, f) fs))
  (fn [#* args #** kwargs]
    (lfor f fs (f #* args #** kwargs))))

(setv chain itertools.chain
      islice itertools.islice
      reduce functools.reduce
      remove itertools.filterfalse
      repeat itertools.repeat)
