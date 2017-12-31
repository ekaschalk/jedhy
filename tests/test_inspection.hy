(require [src.utils.macros [*]])
(require [hy.extra.anaphoric [*]])
(require [tests.hytest [*]])
(import [tests.hytest [*]])

(import
  [src.inspection [Parameter
                   Signature
                   Inspect
                   builtin-docs-to-lispy-docs]])

;; * Argspec Extraction
;; ** Maximal Cases

(defn test-maximal-argspec []
  (defn func [a b
           &optional c [d 0]
           &kwonly e [f 1]
           &rest args
           &kwargs kwargs])

  (assert= "a b &optional c [d 0] #* args #** kwargs &kwonly e [f 1]"
           (-> func Signature str)))


(defn test-maximal-argspec-minus-kwonly []
  (defn func [a b
           &optional c [d 0]
           &rest args
           &kwargs kwargs])

  (assert= "a b &optional c [d 0] #* args #** kwargs"
           (-> func Signature str)))


(defn test-maximal-argspec-minus-kwargs []
  (defn func [a b
           &optional c [d 0]
           &kwonly e [f 1]
           &rest args])

  (assert= "a b &optional c [d 0] #* args &kwonly e [f 1]"
           (-> func Signature str)))


(defn test-maximal-argspec-minus-normal-args []
  (defn func [&optional c [d 0]
           &kwonly e [f 1]
           &rest args
           &kwargs kwargs])

  (assert= "&optional c [d 0] #* args #** kwargs &kwonly e [f 1]"
           (-> func Signature str)))

;; ** Optional/Kwonly with/without defaults

(defn test-optional-without-defaults []
  (defn func [&optional c
           &kwonly e [f 1]])

  (assert= "&optional c &kwonly e [f 1]"
           (-> func Signature str)))


(defn test-optional-with-only-defaults []
  (defn func [&optional [c 0] [d 0]
           &kwonly e [f 1]])

  (assert= "&optional [c 0] [d 0] &kwonly e [f 1]"
           (-> func Signature str)))


(defn test-kwonly-without-defaults []
  (defn func [&kwonly f g])

  (assert= "&kwonly f g"
           (-> func Signature str)))


(defn test-kwonly-with-only-defaults []
  (defn func [&kwonly [f 1] [g 1]])

  (assert= "&kwonly [f 1] [g 1]"
           (-> func Signature str)))

;; ** Trivial cases

(defn test-no-sig []
  (defn func [])

  (assert= ""
           (-> func Signature str)))


(defn test-simplest-sig []
  (defn func [a])

  (assert= "a"
           (-> func Signature str)))

;; * Builtin Docs to Lispy Formatting
;; ** Standard Cases

(defn test-builtin-docstring-conversion-maximal-case []
  (assert=
    "print: (value #* args &optional [sep ' '] [end 'newline'] [file sys.stdout] [flush False])"
    (builtin-docs-to-lispy-docs
      "print(value, ..., sep=' ', end='\n', file=sys.stdout, flush=False)")))


(defn test-builtin-docstring-conversion-optional-only-with-defaults []
  (assert=
    "tee: (iterable &optional [n 2]) - return tuple of n independent iterators."
    (builtin-docs-to-lispy-docs
      "tee(iterable, n=2) --> tuple of n independent iterators.")))


(defn test-builtin-docstring-conversion-optional-only-without-defaults []
  (assert=
    "x: (foo &optional bar) - x"
    (builtin-docs-to-lispy-docs
      "x(foo, bar=None) - x")))


(defn test-builtin-docstring-conversion-no-docs []
  (assert=
    "x: (foo &optional bar)"
    (builtin-docs-to-lispy-docs
      "x(foo, bar=None)")))


(defn test-builtin-docstring-conversion-no-optionals []
  (assert=
    "combinations: (iterable r) - return combinations object"
    (builtin-docs-to-lispy-docs
      "combinations(iterable, r) --> combinations object")))


(defn test-builtin-docstring-conversion-single-optional []
  (assert=
    "int: (&optional [x 0]) -> integer"
    (builtin-docs-to-lispy-docs
      "int(x=0) -> integer")))


(defn test-builtin-docstring-conversion-fails-nonstandard-args-opener []
  (assert=
    "foo[ok] bar"
    (builtin-docs-to-lispy-docs
      "foo[ok] bar")))

;; * Inspection
;; ** Formatting

(defn test-inspect-cut-self-maybe []
  (defn a-func-so-dont-trim [x])
  (assert= "foo: (self x)"
           (-> a-func-so-dont-trim Inspect (.-cut-obj-name-maybe "foo: (self x)")))

  (defclass Foo [])
  (assert= "foo: (x)"
           (-> Foo Inspect (.-cut-obj-name-maybe "foo: (self x)")))
  (assert= "foo: ()"
           (-> Foo Inspect (.-cut-obj-name-maybe "foo: (self)"))))


(defn test-inspect-cut-method-wrapper []
  (defclass Foo [])
  (assert= "method-wrapper: ..."
           (-> Foo.--call-- Inspect (.-cut-method-wrapper-maybe "foo: ..."))))

;; ** Properties

(defn test-inspect-obj-name-mangled []
  (defn func_foo [])
  (assert= "func-foo"
           (-> func_foo Inspect (. obj-name))))


(defn test-inspect-class-and-method-wrappers []
  (defclass Foo [])
  (assert= "Foo"
           (-> Foo Inspect (. obj-name)))
  (assert (-> Foo Inspect (. class?)))
  (assert (-> Foo.--call-- Inspect (. method-wrapper?))))


(defn test-inspect-lambdas []
  (assert (-> (fn []) Inspect (. lambda?))))

;; ** Actions

(defn test-inspect-docs-of-class []
  (defclass Foo [object] "A class\nDetails..." (defn --init-- [self x y]))
  (assert= "Foo: (x y) - A class"
           (-> Foo Inspect (.docs))))


(defn test-inspect-docs-of-builtin []
  (assert=
    "print: (value #* args &optional [sep ' '] [end '\\n'] [file sys.stdout] [flush False])"
    (-> print Inspect (.docs))))


(defn test-inspect-docs-of-module []
  (assert=
    "Functional tools for creating and using iterators."
    (-> itertools Inspect (.docs))))


(defn test-inspect-docs-of-module-function []
  (assert=
    "tee: (iterable &optional [n 2]) - return tuple of n independent iterators."
    (-> itertools.tee Inspect (.docs))))


(defn test-inspect-docs-instance []
  (setv x 1)
  (assert= "int: (&optional [x 0]) -> integer"
           (-> x Inspect (.docs))))
