(ns mobdap.utils)

(defn to-int [value]
  (if (int? value)
    value
    (Integer/parseInt value)))

(defn map-over-map [fun m]
  (reduce-kv (fn [m k v] (assoc m k (fun k v))) {} m))

(defn float-to-string [n]
  (.replaceAll (format "%.16f" n) "\\.?0*$" ""))
