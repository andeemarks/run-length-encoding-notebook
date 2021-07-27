(ns run-length-encoding-notebook.core)

(defn run-length-encode
  [s]
  (let [runs (partition-by identity s)] ;; group by value
    (apply str ;; combine counts and lengths into single string
           (remove #(= % 1) ;; remove any single length run counts
                   (interleave ;; combine the corresponding count and value
                    (map count runs) ;; count the length of each run
                    (map first runs)))))) ;; isolate the value of each run

(defn run-length-decode
  [s]
  (let [components (re-seq #"[1-9]*[a-zA-Z ]" s)]
    (apply str (map #(let [length-raw (apply str (drop-last %))
                           length (if (= 0 (count length-raw)) 1 (Integer. length-raw))
                           value (last %)]
                       (apply str (repeat length value))) components))))
