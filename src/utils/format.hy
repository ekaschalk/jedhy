(defn split-prefix [prefix]
  "Split prefix on last dot accessor, returning an obj, attr pair."
  (setv components
        (.split prefix "."))

  [(->> components butlast (.join "."))
   (->> components last)])

(defn name-or-string [obj]
  "Return obj if a string, otherwise its name."
  (if (instance? str obj)
      obj
      obj.--name--))
