(import inspect
        [src.jedhy
         [--HYCOMPANY-split-prefix
          --HYCOMPANY-obj-candidates
          --HYCOMPANY-get-macros
          --HYCOMPANY-get-globals
          --HYCOMPANY-trim-candidates
          --HYCOMPANY

          --HYANNOTATE-annotate-builtin
          --HYANNOTATE-compiler?
          --HYANNOTATE-macro?
          --HYANNOTATE-shadow?
          --HYANNOTATE

          --HYDOC-args
          --HYDOC-defaults
          --HYDOC-kwonlyargs
          --HYDOC-kwonlydefaults
          --HYDOC-kwargs

          --HYDOC-extract-lispy-argspec
          --HYDOC-extract-docstring
          --HYDOC-extract-eldoc-string

          --HYDOC-get-macro-obj

          --HYDOC-builtin-docstring-to-lispy

          --HYDOC-extract-macro-eldoc
          --HYDOC-extract-python-eldoc
          --HYDOC
          ]])

;; * Asserts

(defn assert= [x y]
  (assert (= x y)))

(defn assert-in [x y]
  (assert (in x y)))

(defn assert-not-in [x y]
  (assert (not (in x y))))

(defn assert-all= [x y]
  (assert (->> (zip-longest x y) (*map =) all)))

(defn assert-all-in [x y]
  (assert (->> x (map (fn [z] (in z y))) all)))

;; * Tests
;; ** Formatting

(defn test-split-prefix []
  (assert-all= (--HYCOMPANY-split-prefix "ob")
               ["" "ob"])
  (assert-all= (--HYCOMPANY-split-prefix "ob.j")
               ["ob" "j"])
  (assert-all= (--HYCOMPANY-split-prefix "ob.j.attr")
               ["ob.j" "attr"])

  (assert-all= (--HYCOMPANY-split-prefix "ob.j.")
               ["ob.j" ""])
  (assert-all= (--HYCOMPANY-split-prefix "")
               ["" ""]))

;; ** Candidates
;; *** Objects

(defn test-candidates-built-in-func []
  (assert-in "--call--"
             (--HYCOMPANY-obj-candidates "print"))
  (assert-in "--call--"
             (--HYCOMPANY-obj-candidates "str.format")))

(defn test-candidates-class []
  (assert-in "format"
             (--HYCOMPANY-obj-candidates "str")))

(defn test-candidates-module []
  (assert-in "eval"
             (--HYCOMPANY-obj-candidates "builtins")))

;; *** Macros

(defn test-candidates-macros []
  (assert-all-in ["->" "for"]
                 (--HYCOMPANY-get-macros)))

(defn test-candidates-misc []
  (assert-all-in ["+=" "/=" "HySet"]
                 (--HYCOMPANY-get-macros)))

(defn test-candidates-compile-table []
  (assert-all-in ["yield" "require" "import" "list-comp"]
                 (--HYCOMPANY-get-macros)))

;; *** Globals

(defn test-candidates-globals []
  (assert-all-in ["first" "in" "+" "even?"]
                 (--HYCOMPANY-get-globals)))

;; ** Pipeline

(defn test-candidates-trimmed []
  (setv candidates
        (--HYCOMPANY-get-globals))

  (assert-all-in ["first" "even?"] candidates)

  (setv trimmed-candidates
        (--HYCOMPANY-trim-candidates candidates "fi"))

  (assert-in "first" trimmed-candidates)
  (assert-not-in "even?" trimmed-candidates))

(defn test-candidates-formatted []
  (assert-in "builtins"
             (--HYCOMPANY "built"))
  (assert-in "builtins.eval"
             (--HYCOMPANY "builtins.e"))
  (assert-in "builtins.eval.--call--"
             (--HYCOMPANY "builtins.eval.")))

;; * Annotations

(defn test-annotations-builtins []
  (setv annotations
        ["def"  ; builtin_function_or_method
         "def"  ; function
         "module"
         "class"
         "instance"
         None])
  (setv objects
        ["print"
         "--HYANNOTATE-annotate-builtin"
         "builtins"
         "itertools.chain"
         "hy.macros.-hy-macros"
         "doesnt exist"])

  (assert-all= annotations
               (list (map --HYANNOTATE-annotate-builtin objects))))

(defn test-annotations-search-compiler []
  (assert (--HYANNOTATE-compiler? "try")))

(defn test-annotations-search-macros []
  (assert (--HYANNOTATE-macro? "->")))

(defn test-annotations-search-shadows []
  (assert (--HYANNOTATE-shadow? "+")))

;; * Hydoc
;; ** Argspec Extraction
;; *** Maximal Cases

(defn test-hydoc-maximal-argspec []
  (defn func [a b
           &optional c [d 0]
           &kwonly e [f 1]
           &rest args
           &kwargs kwargs])

  (assert= "a b &optional c [d 0] #* args #** kwargs &kwonly e [f 1]"
           (--HYDOC-extract-lispy-argspec func)))

(defn test-hydoc-maximal-argspec-minus-kwonly []
  (defn func [a b
           &optional c [d 0]
           &rest args
           &kwargs kwargs])

  (assert= "a b &optional c [d 0] #* args #** kwargs"
           (--HYDOC-extract-lispy-argspec func)))

(defn test-hydoc-maximal-argspec-minus-kwargs []
  (defn func [a b
           &optional c [d 0]
           &kwonly e [f 1]
           &rest args])

  (assert= "a b &optional c [d 0] #* args &kwonly e [f 1]"
           (--HYDOC-extract-lispy-argspec func)))

(defn test-hydoc-maximal-argspec-minus-normal-args []
  (defn func [&optional c [d 0]
           &kwonly e [f 1]
           &rest args
           &kwargs kwargs])

  (assert= "&optional c [d 0] #* args #** kwargs &kwonly e [f 1]"
           (--HYDOC-extract-lispy-argspec func)))

;; *** Optional/Kwonly with/without defaults

(defn test-hydoc-optional-without-defaults []
  (defn func [&optional c
           &kwonly e [f 1]])

  (assert= "&optional c &kwonly e [f 1]"
           (--HYDOC-extract-lispy-argspec func)))

(defn test-hydoc-optional-with-only-defaults []
  (defn func [&optional [c 0] [d 0]
           &kwonly e [f 1]])

  (assert= "&optional [c 0] [d 0] &kwonly e [f 1]"
           (--HYDOC-extract-lispy-argspec func)))

(defn test-hydoc-kwonly-without-defaults []
  (defn func [&kwonly f g])

  (assert= "&kwonly f g"
           (--HYDOC-extract-lispy-argspec func)))

(defn test-hydoc-kwonly-with-only-defaults []
  (defn func [&kwonly [f 1] [g 1]])

  (assert= "&kwonly [f 1] [g 1]"
           (--HYDOC-extract-lispy-argspec func)))

;; *** Trivial case

(defn test-hydoc-no-sig []
  (defn func [])

  (assert= ""
           (--HYDOC-extract-lispy-argspec func)))

(defn test-hydoc-simplest-sig []
  (defn func [a])

  (assert= "a"
           (--HYDOC-extract-lispy-argspec func)))

;; ** Docstring Extraction

(defn test-hydoc-docstring-extraction []
  (defn func [a] "First line.\n Another line." "String for implicit return.")

  ;; (setv obj
  ;;       None)
  ;; ;; (setv obj
  ;; ;;       (--HYDOC-get-macro-obj "->"))

  ;; (print "\nobj is \n" obj)
  ;; (print "-----------------")
  ;; (print "its docs \n" obj.--doc--)
  ;; (print "-----------------")
  ;; (print "result \n" (--HYDOC-extract-docstring obj))
  ;; (print "---")

  (setv objs
        [print                         ; Builtin implemented in C
         +                             ; Shadowed operator
         itertools                     ; A module
         itertools.chain               ; A class
         func                          ; A function
         None                         ; Edge Case

         ;; (--HYDOC-get-macro-obj "if")  ; A hy.core.bootstrap macro
         ;; (--HYDOC-get-macro-obj "->")  ; A mangled macro
         ])

  (setv docstrings
        ["print(value, ..., sep=' ', end='\\n', file=sys.stdout, flush=False)"
         "Shadowed `+` operator adds `args`."
         "Functional tools for creating and using iterators."
         "chain(*iterables) --> chain object"
         "First line."
         ""

         ;; "Conditionally evaluate alternating test and then expressions."
         ;; "Thread `head` first through the `rest` of the forms."
         ])

  (assert-all= docstrings
               (list (map --HYDOC-extract-docstring objs))))

;; ** Python Eldoc Extraction

(defn test-hydoc-builtin-docstring-conversion-maximal-case []
  (assert=
    "print: (value #* args &optional [sep ' '] [end 'newline'] [file sys.stdout] [flush False])"
    (--HYDOC-builtin-docstring-to-lispy
      "print(value, ..., sep=' ', end='\n', file=sys.stdout, flush=False)")))

(defn test-hydoc-builtin-docstring-conversion-optional-only-with-defaults []
  (assert=
    "tee: (iterable &optional [n 2]) - return tuple of n independent iterators."
    (--HYDOC-builtin-docstring-to-lispy
      "tee(iterable, n=2) --> tuple of n independent iterators.")))

(defn test-hydoc-builtin-docstring-conversion-optional-only-without-defaults []
  (assert=
    "x: (foo &optional bar) - x"
    (--HYDOC-builtin-docstring-to-lispy
      "x(foo, bar=None) - x")))

(defn test-hydoc-builtin-docstring-conversion-no-docs []
  (assert=
    "x: (foo &optional bar)"
    (--HYDOC-builtin-docstring-to-lispy
      "x(foo, bar=None)")))

(defn test-hydoc-builtin-docstring-conversion-no-optionals []
  (assert=
    "combinations: (iterable r) - return combinations object"
    (--HYDOC-builtin-docstring-to-lispy
      "combinations(iterable, r) --> combinations object")))

(defn test-hydoc-extract-python-eldoc []
  ;; (print
  ;;   (--HYDOC-extract-python-eldoc
  ;;     ;; "print"
  ;;     ;; "Xa"
  ;;     ;; "itertools.tee"
  ;;     ;; "print.--call--"
  ;;     ;; "print.--eq--"
  ;;     ;; "itertools.product"
  ;;     "np.bincount"
  ;;     ;; "np.histogram.__str__"
  ;;     ;; "__str__"
  ;;     ;; "itertools.combinations"
  ;;     ))

  ;; TODO itertools.product has *iterables -> want #* iterables
  ;; TODO - inspect.cleandoc might be useful
  ;; TODO - the argspec formatting is lispy version of inspect.signature
  ;; TODO strip (or add) end period

  ;; (print
  ;;   (--HYDOC-extract-python-eldoc
  ;;     "print"
  ;;     ))
  )

;; ** Macro Eldoc Extraction
