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
