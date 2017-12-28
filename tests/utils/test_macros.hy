"Tests for the some-> macros I've implemented, pending merging into Hy."

(require [src.utils.macros [*]])
(require [hy.extra.anaphoric [*]])
(require [tests.hytest [*]])
(import [tests.hytest [*]])


;; * Some threads
;; ** Simple cases

(defn test-some-no-forms []
  (assert= None (some-> None))
  (assert= 1 (some-> 1))
  (assert= 1 (some->> 1)))

(defn test-some-some-forms []
  (assert= 2 (some-> 1 inc))
  (assert= 3 (some-> 1 inc inc))
  (assert= 2 (some->> 1 inc))
  (assert= 3 (some->> 1 inc inc)))

(defn test-some-short-circuit []
  (assert (none? (some-> None inc)))
  (assert (none? (some-> 1 ((constantly None)) inc)))
  (assert (none? (some->> None inc)))
  (assert (none? (some->> 1 ((constantly None)) inc))))

(defn test-some-with-nonliteral-head []
  (assert= (-> (+ 1 2) inc)
           (some-> (+ 1 2) inc)))

(defn test-some-with-dot-dsl []
  (setv x [])
  (some-> x (.append 1))
  (assert 1 (first x))

  (setv x [])
  (some->> 1 (.append x))
  (assert 1 (first x)))

;; FAIL
;; (defn test-some-with-dot-dsl-many-attrs []
;;   (assert (some-> [] (. append --call--))))

;; ** Specific signatures

(defn test-some-func-just-has-rest []
  (defn fn-just-rest [&rest x]
    (- #* x))

  (assert= (-> 1 (fn-just-rest 2 3))
           (some-> 1 (fn-just-rest 2 3)))
  (assert= (->> 1 (fn-just-rest 2 3))
           (some->> 1 (fn-just-rest 2 3))))


(defn test-some-func-arg-plus-rest []
  (defn fn-args-plus-rest [a &rest x]
    (+ a (- #* x)))

  (assert= (-> 1 (fn-args-plus-rest 2 3))
           (some-> 1 (fn-args-plus-rest 2 3)))
  (assert= (->> 1 (fn-args-plus-rest 2 3))
           (some->> 1 (fn-args-plus-rest 2 3))))

(defn test-some-arg-plus-optional []
  (defn fn-args-plus-optional [a &optional b [c 10]]
    (+ a (- b c)))

  (assert= (-> 1 (fn-args-plus-optional 2))
           (some-> 1 (fn-args-plus-optional 2)))
  (assert= (->> 1 (fn-args-plus-optional 2))
           (some->> 1 (fn-args-plus-optional 2)))

  (assert= (-> 1 (fn-args-plus-optional 2 3))
           (some-> 1 (fn-args-plus-optional 2 3)))
  (assert= (->> 1 (fn-args-plus-optional 2 3))
           (some->> 1 (fn-args-plus-optional 2 3))))

(defn test-some-with-kwonly []
  (defn fn-kwonly [a b &kwonly [c 10]]
    (+ (- a b) c))

  (assert= (-> 1 (fn-kwonly 2))
           (some-> 1 (fn-kwonly 2)))
  (assert= (->> 1 (fn-kwonly 2))
           (some->> 1 (fn-kwonly 2)))

  (assert= (-> 1 (fn-kwonly 2 :c 5))
           (some-> 1 (fn-kwonly 2 :c 5)))
  (assert= (->> 1 (fn-kwonly 2 :c 5))
           (some->> 1 (fn-kwonly 2 :c 5))))
