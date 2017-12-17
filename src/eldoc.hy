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
