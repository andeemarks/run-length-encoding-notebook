;; gorilla-repl.fileformat = 1

;; **
;;; # Run Length Encoding (RLE) in Clojure
;;; 
;;; When you ask an experienced developer with occasional experience in Clojure to write an implementation of RLE, you get a solution that works... but looks like it's been written by a "developer with occasional experience in Clojure".
;;; 
;;; Ladies and gentlemen, I present my solution to the RLE problem in the Clojure track of [exercism.io](https://exercism.io)
;;; 
;; **

;; @@
(ns run-length-encoding)

(defn- group-by-runs [plain-text] (partition-by identity plain-text))
(defn- remove-repeated-chars [runs] (map dedupe runs))
(defn- count-run-length [runs] (doall (map count runs)))
(defn- abbrev-single-char-runs [runs] (remove #(= % 1) runs))

(defn- match-run-length-with-run [run-lengths runs]
  (apply str
         (flatten
          (abbrev-single-char-runs
           (interleave run-lengths runs)))))

(defn run-length-encode
  "encodes a string with run-length-encoding"
  [plain-text]
  (let [runs (group-by-runs plain-text)]
    (match-run-length-with-run
     (count-run-length runs)
     (remove-repeated-chars runs))))

(defn- run-length [component] (Integer/parseInt (first (re-seq #"\d+" component))))
(defn- run-value [component] (first (re-seq #"[^\d]+" component)))
(defn- run [length value] (apply str (repeat length (get value 0))))

(defn- component-to-run [component]
  (let [length (run-length component)
        value (run-value component)
        run (run length value)]
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
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;run-length-encoding/run-length-decode</span>","value":"#'run-length-encoding/run-length-decode"}
;; <=

;; **
;;; These are the tests provided by Exercism to help guide the solution, including defining the public interface of ```run-length-encode``` and ```run-length-decode```.
;; **

;; @@
(ns run-length-encoding-test
  (:require [clojure.test :refer :all]
            [run-length-encoding :as rle]))

;;Tests for run-length-encoding exercise

(deftest encode-empty-string
         (testing "encode an empty string"
                  (is (= (rle/run-length-encode "") ""))))

(deftest encode-single-characters-without-count
         (testing "encode single characters without count"
                  (is (= (rle/run-length-encode "XYZ") "XYZ"))))

(deftest encode-string-with-no-single-characters
         (testing "encode string with no single characters"
                  (is (= (rle/run-length-encode "AABBBCCCC") "2A3B4C"))))

(deftest encode-string-with-single-and-mixed-characters
         (testing "encode string with single and mixed characters"
                  (is (= (rle/run-length-encode "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB") "12WB12W3B24WB"))))

(deftest encode-multiple-whitespace
         (testing "encode string with whitespace characters mixed in it"
                  (is (= (rle/run-length-encode "  hsqq qww  ") "2 hs2q q2w2 "))))

(deftest encode-lowercase
         (testing "encode string with lowercase characters"
                  (is (= (rle/run-length-encode "aabbbcccc") "2a3b4c"))))

(deftest decode-empty-string
         (testing "decode empty string"
                  (is (= (rle/run-length-decode "") ""))))

(deftest decode-single-characters
         (testing "decode string with single characters only"
                  (is (= (rle/run-length-decode "XYZ") "XYZ"))))

(deftest decode-no-single-characters
         (testing "decode string with no single characters"
                  (is (= (rle/run-length-decode "2A3B4C") "AABBBCCCC"))))

(deftest decode-single-and-repeated-characters
         (testing "decode string with single and repeated characters"
                  (is (= (rle/run-length-decode "12WB12W3B24WB") "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB"))))

(deftest decode-lowercase
         (testing "decode string with lowercase characters"
                  (is (= (rle/run-length-decode "2a3b4c") "aabbbcccc"))))

(deftest decode-mixed-whitespace
         (testing "decode string with mixed whitespace characters in it"
                  (is (= (rle/run-length-decode "2 hs2q q2w2 ") "  hsqq qww  "))))

(deftest consistency
         (testing "Encode a string and then decode it. Should return the same one."
                  (is (= (rle/run-length-decode (rle/run-length-encode "zzz ZZ  zZ")) "zzz ZZ  zZ"))))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;run-length-encoding-test/consistency</span>","value":"#'run-length-encoding-test/consistency"}
;; <=

;; **
;;; Now I was fairly happy with my implementation when I submitted it to Exercism and one of the great aspects of that platform is the ability to view community solutions to the same problem you have just completed.  You can quickly see how other developers have implemented the same solution, and how similar (or different) their solutions are to your own.  Sometimes this comparison is affirming, sometimes it is demoralising.  
;;; 
;;; With Clojure in particular, heavily idiomatic solutions sometimes seem quite foreign to me, and this was the situation I found when looking at the community solutions with the highest number of stars...
;; **

;; @@
(ns run-length-encoding)

(defn run-length-encode
  [s]
  (->> s
       (partition-by identity)
       (mapcat #(if (= 1 (count %))
                  [(first %)]
                  [(count %) (first %)]))
       (apply str)))

(defn run-length-decode
  [s]
  (->> s
       (re-seq #"[1-9]*[a-zA-Z ]")
       (mapcat #(if (= 1 (count %))
                  %
                  (repeat (->> % drop-last (apply str) Integer.) (last %))))
       (apply str)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;run-length-encoding/run-length-decode</span>","value":"#'run-length-encoding/run-length-decode"}
;; <=

;; **
;;; For people unfamiliar with Clojure, it can produce very dense solutions in the hands of people confident with the depth and breadth of the language.  This solution is half the length of mine by line count yet passes the same set of supplied tests.  The solution does this using only a single function for each of the main decoding and encoding requirements, whereas my solution used a total of 11 functions.  More strikingly, the solution uses 0 local variables and relies heavily on chaining multiple function calls together (another typical Clojure idiom) while I still default to a more traditional style of explicit local variables defined with ```let``` function calls (5) plus more explicit naming through my 9 additional named functions. 
;;; 
;;; So in short:
;;; 
;;; | | My solution | Other solution |
;;; |-----|-----|
;;; |Lines of code|40|19|
;;; |Functions|9|2|
;;; |Local variables|5|0|
;;; 
;;; 
;;; 
;; **

;; @@

;; @@
