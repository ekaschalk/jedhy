(import pytest)


;; * Asserts

(defn assert= [x y]
  (assert (= x y)))

(defn assert-in [x y]
  (assert (in x y)))

(defn assert-all-in [x y]
  (assert (->> x (map (fn [z] (in z y))) all)))

;; * Pytest Fixtures

(defmacro deffixture [fn-name docstring params &rest body]
  "Pytest parametrize reader."
  `(with-decorator
     (pytest.fixture :params ~params :scope "module")
     (defn ~fn-name [request]
       ~docstring
       (setv it request.param)
       ~@body)))

(defmacro with-fixture [fixture fn-name args &rest body]
  `(defn ~fn-name [~fixture]
     (setv ~args ~fixture)
     ~@body))

(defmacro assert-raises [error &rest code]
  `(with [(pytest.raises ~error)] ~@code))
