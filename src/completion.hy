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
