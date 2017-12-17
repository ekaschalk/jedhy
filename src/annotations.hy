
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
