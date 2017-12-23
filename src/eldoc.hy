;; * Hydoc
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

(defn --HYDOC-extract-macro-eldoc [obj &optional full]
  "Get eldoc string for a macro."
  (when (--HYANNOTATE-macro? obj)
    (->> obj
       --HYDOC-get-macro-obj
       (--HYDOC-extract-eldoc-string :full full))))

;; ** Python Eldoc

(defn --HYDOC-extract-python-eldoc [obj-name &optional full]
  "Build eldoc string for python string.

Not all defuns can be argspeced - eg. C defuns."
  (-> obj-name
     --HYDOC-get-python-obj
     (--HYDOC-extract-eldoc-string :full full)))

;; ** Driver

(defn --HYDOC [obj &optional full]
  "Get eldoc string for any obj."
  (cond [(--HYDOC-extract-macro-eldoc obj :full full)]
        [(--HYDOC-extract-python-eldoc obj :full full)]
        [True ""]))
