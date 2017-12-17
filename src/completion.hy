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
