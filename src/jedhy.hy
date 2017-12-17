"Autocompletion, annotations, and documentation introspection tools for Hy.

Provides three entry points: --HYDOC, --HYCOMPANY, and --HYANNOTATE.
For Eldoc, Company candidates, and annotations for a candidate respectively.

Naming scheme is prefixed with private identifiers since these functions
are all being evaluated within the internal Hy process and thus would
interfere with completion. Eventually the approach jedi takes, namely a
separate package that Emacs installs behinds the scenes, will be done.

This entire file is sent as a raw string to the hy process. The * imports
are there for namespacing Hy's core."

;; TODO For testing atm
(import [numpy :as np])
(defclass Xa [object]
  "hi there"
  (defn --init-- [self a]
    "bi there"
    ))

(import
  builtins inspect types

  hy hy.compiler hy.macros
  [hy.lex.parser [hy-symbol-unmangle hy-symbol-mangle]]
  [hy.core.shadow [*]] [hy.core.language [*]]  ; To complete the namespace
  )

;; See https://github.com/hylang/hy/issues/1467
(hy.eval '(import hy.macros))

;; * Company
;; ** Formatting

(defn --HYCOMPANY-split-prefix [prefix]
  "Split prefix on last dot accessor, returning an obj, attr pair."
  (setv components
        (.split prefix "."))

  [(->> components butlast (.join "."))
   (->> components last)])

(defn --HYCOMPANY-obj-string [obj]
  "Return obj if a string, otherwise its name."
  (if (instance? str obj) obj obj.--name--))

;; ** Extraction

(defn --HYCOMPANY-obj-candidates [obj]
  "Try to retrieve unmangled attrs list for given (python) obj."
  (try
    (->> obj
       builtins.eval
       dir
       (map hy-symbol-unmangle)
       list)
    (except [e Exception]
      [])))

(defn --HYCOMPANY-get-macros []
  "Extract macro names from all namespaces and compile-table symbols."
  (->> hy.macros.-hy-macros
     (.values)
     (map dict.keys)
     (chain hy.compiler.-compile-table)
     flatten
     (map (comp hy-symbol-unmangle --HYCOMPANY-obj-string))
     distinct
     list))

(defn --HYCOMPANY-get-globals []
  "Extract unmangled globals."
  (->> (globals)
     (.keys)
     (map hy-symbol-unmangle)
     list))

(defn --HYCOMPANY-all-candidates []
  "All global and macro candidates."
  (->> (--HYCOMPANY-get-globals)
     (chain (--HYCOMPANY-get-macros))
     flatten
     distinct
     list))

;; ** Pipeline

(defn --HYCOMPANY-candidates [obj]
  "Return candidates for possibly None obj."
  (if obj
      (--HYCOMPANY-obj-candidates obj)
      (--HYCOMPANY-all-candidates)))

(defn --HYCOMPANY-trim-candidates [candidates attr]
  "Limit list of candidates to those starting with attr."
  (->> candidates
     (filter (fn [x] (.startswith x attr)))
     list))

(defn --HYCOMPANY-format-candidates [candidates obj]
  "Modify candidates for full prefix rather, not just the attr completions."
  (if obj
      (->> candidates
         (map (fn [x] (+ obj "." x)))
         list)
      candidates))

;; ** Driver

(defn --HYCOMPANY [prefix]
  "Extract candidates for a given prefix."
  (setv [obj attr]
        (--HYCOMPANY-split-prefix prefix))

  (-> obj
     --HYCOMPANY-candidates
     (--HYCOMPANY-trim-candidates attr)
     (--HYCOMPANY-format-candidates obj)))

;; * Annotations

(defn --HYANNOTATE-class-annotation [klass]
  "Return annotation given a name of a class."
  (cond [(in klass ["function" "builtin_function_or_method"])
         "def"]
        [(= klass "type")
         "class"]
        [(= klass "module")
         "module"]
        [True
         "instance"]))

(defn --HYANNOTATE-annotate-builtin [candidate]
  "Try to extract annotation searching builtins."
  (try
    (-> candidate
       hy-symbol-mangle
       builtins.eval
       (. --class--)
       (. --name--)
       --HYANNOTATE-class-annotation)
    (except [e Exception]
      None)))

(defn --HYANNOTATE-compiler? [candidate]
  "Is candidate a compile table construct?"
  (in candidate hy.compiler.-compile-table))

(defn --HYANNOTATE-macro? [candidate]
  "Is candidate a macro?"
  (in (hy-symbol-mangle candidate) (get hy.macros.-hy-macros None)))

(defn --HYANNOTATE-shadow? [candidate]
  "Is candidate a shadowed operator?"
  (in (hy-symbol-mangle candidate) (dir hy.core.shadow)))

(defn --HYANNOTATE-format-annotation [annotation candidate]
  "Format an annotation for company display."
  (if annotation
      (.format "<{} {}>" annotation x)
      ""))

(defn --HYANNOTATE [candidate]
  "Return annotation for a candidate."
  (-> (cond [(--HYANNOTATE-annotate-builtin candidate)]
           [(--HYANNOTATE-shadow? candidate)
            "shadowed"]
           [(--HYANNOTATE-compiler? candidate)
            "compiler"]
           [(--HYANNOTATE-macro? candidate)
            "macro"])
     (--HYANNOTATE-format-annotation candidate)))

;; * Hydoc
;; ** Argspec Extraction

(defn --HYDOC-args [argspec]
  "Extract standard positional arguments from argspec."
  (if (and argspec.args argspec.defaults)
      (-> argspec.defaults
         len
         (drop-last argspec.args)
         list)
      argspec.args))

(defn --HYDOC-defaults [argspec]
  "Extract &optional arguments from argspec with possibly their default."
  (when argspec.defaults
    (->> (if (and argspec.args argspec.defaults)
           (-> argspec
              --HYDOC-args
              len
              (drop argspec.args)
              list)
           argspec.defaults)
       (zip argspec.defaults)
       (*map (fn [default arg]
               (if (none? default)
                   arg
                   (.format "[{} {}]" arg default))))
       list)))

(defn --HYDOC-kwonlyargs [argspec]
  "Extract :keyword arguments without a default from argspec."
  (if (and argspec.kwonlyargs argspec.kwonlydefaults)
      (->> argspec.kwonlyargs
         (remove (fn [x] (in x (.keys argspec.kwonlydefaults))))
         list)
      argspec.kwonlyargs))

(defn --HYDOC-kwonlydefaults [argspec]
  "Extract :keyword arguments with their default from argspec."
  (if (and argspec.kwonlyargs argspec.kwonlydefaults)
      (->> argspec.kwonlydefaults
         (.items)
         (*map (fn [k v] (.format "[{} {}]" k v)))
         list)
      argspec.kwonlydefaults))

(defn --HYDOC-kwargs [argspec]
  (-> argspec
     ((juxt --HYDOC-kwonlyargs --HYDOC-kwonlydefaults))
     flatten))

;; ** Format Argspec

(defn --HYDOC-acc-formatted-argspec [formatted-argspec [args arg-opener]]
  "Accumulator for adding formatted argspec components."
  (when args
    ;; Want list of all None to fail on conditionals just like single None
    (setv args
          (->> args (remove none?) list)))

  (+ formatted-argspec
     (if (and formatted-argspec args) " " "")
     (if args
         (+ (if arg-opener (+ arg-opener " ") "")
            (.join " " args))
         "")))

(defn --HYDOC-extract-lispy-argspec [func]
  "Lispy version of formatted getfullargspec covering all defun kwords."
  (setv argspec
        (inspect.getfullargspec func))
  (setv [args defaults kwargs]
        ((juxt --HYDOC-args --HYDOC-defaults --HYDOC-kwargs) argspec))

  (reduce --HYDOC-acc-formatted-argspec
          [""
           [args None]
           [defaults "&optional"]
           [[argspec.varargs] "#*"]
           [[argspec.varkw] "#**"]
           [kwargs "&kwonly"]]))

;; ** Extract Eldoc String

(defn --HYDOC-first-line [s]
  "Get first line of a given string."
  (-> s (.splitlines) first))

(defn --HYDOC-butfirst-line [s]
  "Get but the first line of a given string."
  (-> s (.splitlines) rest))

(defn --HYDOC-extract-docstring [func &optional full]
  "Format a docstring (first line) for Eldoc or hyconda buffer (full)."
  (cond [(not func.--doc--)
         ""]

        ;; TODO Why is this here again?
        [full
         (->> func.--doc-- (.splitlines) (.join "\n") (+ "\n"))]

        [True
         (--HYDOC-first-line func.--doc--)]))

;; ** Builtin to Lispy Formatting

(defn --HYDOC-builtin-docstring-to-lispy [docs]
  "Converts a builtin's --doc-- first-line formatted args to lispy style."
  (defn split-docs [docs]
    (setv [start-args end-args]
          [(inc (.index docs "(")) (.index docs ")")])

    [(cut docs 0 start-args)
     (cut docs start-args end-args)
     (cut docs end-args)])

  (defn builtin-arg-to-lispy [arg]
    (if (not-in "=" arg)
        arg

        (do (setv [arg-name - default]
                  (.partition arg "="))
            (if (= "None" default)
                arg-name
                (.format "[{} {}]" arg-name default)))))

  (defn first-optional-arg-idx [args]
    (some (fn [[i x]] (and (in "=" x) i))
       (enumerate args)))

  (defn insert-optional [args]
    (setv optional-idx
          (first-optional-arg-idx args))
    (when (not (none? optional-idx))
      (.insert args (first-optional-arg-idx args)
               "&optional"))
    args)

  (setv [pre-args - post-args]
        (.partition docs "("))
  (setv docs
        (.format "{}: ({}" pre-args post-args))

  (setv docs
        (-> docs
           (.replace "..." "#* args")
           (.replace "*args" "#* args")
           (.replace "**kwargs" "#** kwargs")
           (.replace "\n" "newline")
           (.replace "-->" "- return")  ; eg. itertools.tee/product..
           ))

  (setv [pre-args args post-args]
        (split-docs docs))

  (setv formatted-args
        (->> (.split args ",")
             (map str.strip)
             list
             insert-optional
             (map builtin-arg-to-lispy)
             ((fn [x] (.join " " x)))))

  (+ pre-args formatted-args post-args))

;; ** Eldoc formatting

(defn --HYDOC-docs-delim [func]
  "Determine delimiter for a callable from its docs in an eldoc string."
  (try (if func.--doc-- " - " "")
       (except [e KeyError] "")))

(defn --HYDOC-extract-eldoc-string [func &optional full]
  "Extract an eldoc string for a callable."
  (setv func-name
        (if (= func.--name-- "lambda")
            "<lambda>"
            (hy-symbol-unmangle func.--name--)))

  (setv docs
        (try (do (inspect.getfullargspec func)  ; Check func supported callable
                 (.format "{name}: ({args}){docs_delim}{docs}"
                          :name func-name
                          :args (--HYDOC-extract-lispy-argspec func)
                          :docs_delim (--HYDOC-docs-delim func)
                          :docs (--HYDOC-extract-docstring func :full full)))

             ;; Not defined by "def" or "lambda" in python
             (except [e TypeError]
               (-> func
                  (--HYDOC-extract-docstring :full full)
                  --HYDOC-builtin-docstring-to-lispy))))

  (when (inspect.isclass func)
    (setv docs
          (-> docs
             (.replace "self " "")
             (.replace "self" ""))))

  (when (instance? (type print.--str--) func)  ; "method-wrapper" class
    (setv docs
          (-> docs
             (.replace "self " "")
             (.replace "self" "")))
    (setv docs
          (+ "method-wrapper"
             (cut docs (.index docs ":")))))
  docs)

;; ** Macro Eldoc

(defn --HYDOC-get-macro-obj [obj-name]
  "Get the lambda for the given macro-name."
  (->> obj-name
     hy-symbol-mangle
     (get hy.macros.-hy-macros None)))

(defn --HYDOC-get-python-obj [obj-name]
  "Try to builtins.evaluate given obj-name."
  (try
    (-> obj-name
       hy-symbol-mangle
       (builtins.eval (globals)))
    (except [e NameError] None)))

(defn --HYDOC-extract-macro-eldoc [obj &optional full]
  "Get eldoc string for a macro."
  (when (--HYANNOTATE-macro? obj)
    (--HYDOC-format-eldoc-string obj (--HYDOC-get-macro-obj obj) :full full)))

;; ** Python Eldoc

(defn --HYDOC-extract-python-eldoc [obj-name &optional full]
  "Build eldoc string for python string.

Not all defuns can be argspeced - eg. C defuns."
  (-> obj-name
     --HYDOC-get-python-obj
     (--HYDOC-extract-eldoc-string :full full)))

        ;; (except [e TypeError]
        ;;   (->> obj
        ;;      inspect.getdoc
        ;;      --HYDOC-first-line
        ;;      (+ (.format "(Builtin) {}: " obj.--name--)))
        ;;   ;; (when full
        ;;   ;;   "")
        ;;   ))
            ;; (setv doc
            ;;       (+ doc
            ;;          "\n"
            ;;          (->> full-doc
            ;;             --HYDOC-butfirst-line
            ;;             (.join "")))))))

;; ** Driver

(defn --HYDOC [obj &optional full]
  "Get eldoc string for any obj."
  (cond [(--HYDOC-extract-macro-eldoc obj :full full)]
        [(--HYDOC-extract-python-eldoc obj :full full)]
        [True ""]))
