(ns run-length-encoding-notebook.core)

(defn run-length-encode
  [s]
  (let [runs (partition-by identity s)] ;; group by value
    (apply str ;; combine counts and lengths into single string
           (remove #(= % 1) ;; remove any single length run counts
                   (interleave ;; combine the corresponding count and value
                    (map count runs) ;; count the length of each run
                    (map first runs)))))) ;; isolate the value of each run

(defn- component-to-run [component]
  (let [length (Integer/parseInt (first (re-seq #"\d+" component)))
        value (first (re-seq #"[^\d]+" component))
        run (apply str (repeat length (get value 0)))]
    (if (<= 1 (count value))
      (str run (apply str (rest value)))
      run)))

(defn run-length-decode
  "decodes a run-length-encoded string"
  [cipher-text]
  (let [components (re-seq #"\d+[^\d]+" cipher-text)]
    (if (nil? components)
      cipher-text
      (apply str (map #(component-to-run %) components)))))