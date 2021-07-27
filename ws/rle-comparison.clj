;; gorilla-repl.fileformat = 1

;; **
;;; # Run Length Encoding (RLE) in Clojure
;;; 
;;; When you ask an experienced developer with occasional experience in Clojure to write an implementation of RLE, you get a solution that works... but looks like it's been written by a "developer with occasional experience in Clojure".
;;; 
;;; ## My Solution
;;; 
;;; Ladies and gentlemen, I present my solution to the RLE problem in the Clojure track of [exercism.io](https://exercism.io)!
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
;;; ## Requirements
;;; 
;;; To understand how this code works, look at these tests provided by Exercism to help guide the solution, including defining the public interface of ```run-length-encode``` and ```run-length-decode```.
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
;;; Now I was fairly happy with my implementation when I submitted it and/but one of the great aspects of Exercism is the ability to view community solutions to the same problem you have just completed.  You can quickly see how other developers have implemented the same solution, and how similar (or different) their solutions are to your own.  Sometimes this comparison is affirming, sometimes it is demoralising.  
;;; 
;;; ## An idiomatic solution
;;; 
;;; With Clojure in particular, heavily idiomatic solutions sometimes seem quite foreign to me, and this was the situation I found when looking at one of the highest rated community solutions...
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
;;; ## Comparison
;;; 
;;; For people unfamiliar with Clojure, it can produce very dense solutions in the hands of people confident with the depth and breadth of the language.  The idiomatic solution is half the length of mine by line count yet passes the same set of supplied tests.  The solution does this using only a single function for each of the main decoding and encoding requirements, in comparison to the 11 functions in my solution.  More strikingly, the solution uses 0 local variables and relies heavily on chaining multiple function calls together (another typical Clojure idiom) while I default to a more traditional, non-FP style of explicit local variables defined with ```let``` function calls (x5) plus more explicit naming through my 9 additional named functions. 
;;; 
;;; So in short:
;;; 
;;; | | My solution | Other solution |
;;; |-----|-----|
;;; |Lines of code|40|19|
;;; |Functions|11|2|
;;; |Local variables|5|0|
;;; 
;;; ## Can I get there from here?
;;; 
;;; The challenge I have set myself is to refactor my solution to the idiomatic version, or something close to it.  I know this is technically feasible, but I wanted to see what direction I would take knowing my destination.  I'm prepared to bump up against some of my own preferences for readable code along the way and probably be diving for the Clojure API regularly, but I'm curious to see how this will feel...
;;; 
;;; ### Step 1 - Inlining functions
;;; 
;;; None of the functions I've created are used in multiple places, so they are not helping with duplication, just helping me (and hopefully future versions of me) understand the code via the intent in their names.  So I could technically inline all of these functions and cut down a lot of code.  
;;; 
;;; Let's see how that looks.
;;; 
;; **

;; @@
(ns run-length-encoding)

(defn- abbrev-single-char-runs [runs] (remove #(= % 1) runs))

(defn- match-run-length-with-run [run-lengths runs]
  (apply str
         (flatten
          (abbrev-single-char-runs
           (interleave run-lengths runs)))))

(defn run-length-encode
  "encodes a string with run-length-encoding"
  [plain-text]
  (let [runs (partition-by identity plain-text)]
    (match-run-length-with-run
     (doall (map count runs))
     (map dedupe runs))))

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

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;run-length-encoding/run-length-decode</span>","value":"#'run-length-encoding/run-length-decode"}
;; <=

;; **
;;; So I cheated a little bit and only inlined the six low hanging fruit functions that took little-to-no mental effort to process.  They were all single line implementations, so I've only shaved off seven lines.  The code is now in a strange limbo world where it is a combination of raw Clojure function calls _and_ a few higher level functions.  This version nags at me because of the lack of [Structural Symmetry](https://odetocode.com/blogs/scott/archive/2011/02/07/the-value-of-symmetry.aspx).  
;;; 
;;; ### Step 2 - Encoding
;;; 
;;; Next step is to complete the inlining for ```run-length-encode```.
;; **

;; @@
(defn run-length-encode
  "encodes a string with run-length-encoding"
  [plain-text]
  (let [runs (partition-by identity plain-text)]
    (apply str
           (flatten
            (remove #(= % 1)
                    (interleave (doall (map count runs)) (map dedupe runs)))))))

(run-length-encode "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;12WB12W3B24WB&quot;</span>","value":"\"12WB12W3B24WB\""}
;; <=

;; **
;;; I'm down to 26 lines now (also with the overhead of the two function comments).  I've shaved quite a bit off my original solution and if you compare this version with the original idiomatic equivalent, there is a lot less immediate difference in both size and length:
;; **

;; @@
;;; My version
(defn run-length-encode
  "encodes a string with run-length-encoding"
  [plain-text]
  (let [runs (partition-by identity plain-text)]
    (apply str
           (flatten
            (remove #(= % 1)
                    (interleave (doall (map count runs)) (map dedupe runs)))))))

;;; Idiomatic version
(defn run-length-encode
  [s]
  (->> s
       (partition-by identity)
       (mapcat #(if (= 1 (count %))
                  [(first %)]
                  [(count %) (first %)]))
       (apply str)))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;run-length-encoding-notebook.core/run-length-encode</span>","value":"#'run-length-encoding-notebook.core/run-length-encode"}
;; <=

;; **
;;; To make the differences/similarities more stake, I'll remove the function comment and use the same name for the parameter to each version:
;;; 
;; **

;; @@
;;; My version
(defn run-length-encode
  [s]
  (let [runs (partition-by identity s)]
    (apply str
           (flatten
            (remove #(= % 1)
                    (interleave (doall (map count runs)) (map dedupe runs)))))))

;;; Idiomatic version
(defn run-length-encode
  [s]
  (->> s
       (partition-by identity)
       (mapcat #(if (= 1 (count %))
                  [(first %)]
                  [(count %) (first %)]))
       (apply str)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;run-length-encoding-notebook.core/run-length-encode</span>","value":"#'run-length-encoding-notebook.core/run-length-encode"}
;; <=

;; **
;;; #### Encoding - Common elements
;;; 
;;; There are certainly common elements here, although not always easy to spot if you aren't used to reading a lot of lisp-like languages.  Both solutions:
;;; * Use ```partition-by identity``` as an opening gambit to split a collection (which includes a string) into groups based on their value/identity
;;; * Use ```apply str``` to turn each element of the return value into a string
;;; * Use ```count``` to determine the run length
;;; 
;;; Here are quick examples of how those functions work using test data from this problem set:
;; **

;; @@
(partition-by identity "AABBBCCCC")
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\A</span>","value":"\\A"},{"type":"html","content":"<span class='clj-char'>\\A</span>","value":"\\A"}],"value":"(\\A \\A)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\B</span>","value":"\\B"},{"type":"html","content":"<span class='clj-char'>\\B</span>","value":"\\B"},{"type":"html","content":"<span class='clj-char'>\\B</span>","value":"\\B"}],"value":"(\\B \\B \\B)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\C</span>","value":"\\C"},{"type":"html","content":"<span class='clj-char'>\\C</span>","value":"\\C"},{"type":"html","content":"<span class='clj-char'>\\C</span>","value":"\\C"},{"type":"html","content":"<span class='clj-char'>\\C</span>","value":"\\C"}],"value":"(\\C \\C \\C \\C)"}],"value":"((\\A \\A) (\\B \\B \\B) (\\C \\C \\C \\C))"}
;; <=

;; **
;;; Using ```partition-by identity``` gives us a nicely grouped nested structure based on the value of each item in the original string.  Way back when I first learned how to program at univerisity, this type of high level processing logic was referred to as a [Control Break](https://en.wikipedia.org/wiki/Control_break).
;; **

;; @@
(apply str 2 "A" 3 "B" 4 "C")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;2A3B4C&quot;</span>","value":"\"2A3B4C\""}
;; <=

;; **
;;; Using ```apply str``` is the idiomatic Clojure way to join a number of values into a single string.
;;; 
;;; #### Encoding - Omitting single value run counts
;;; 
;;; The combination of these two sets of function gives us the top and tail of the overall solution.  The middle is all concerned with counting the length of each run.  There is a slight complication of needing to remove explicit references to runs of length 1 - after all, we are trying to reduce the size of the encoded text and "1A" is longer than just "A".  Each version solves this complication in different ways; my version naively encodes single length runs and then removes them from the result using the list comprehension ```remove #(= % 1)``` with the idiomatic version choosing not to create them in the first place via the ```if (= 1 (count %))``` phrase.
;; **

;; @@
(remove #(= % 1) '(2 1 3 4 1 5))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"}],"value":"(2 3 4 5)"}
;; <=

;; **
;;; #### Encoding - High level approach
;;; 
;;; There is a higher level difference between the two solutions that we touched on above. The idiomatic solution iteratively steps through each of the groups produced from ```partition-by identity``` and then uses ```count``` to calculate the length for each groups.  My solution uses ```(doall (map count runs))``` to create collection of all the group lengths, and then matches the corresponding run length to the value with ```interleave``` .  There may be a performance difference between these two approaches, but certainly not one that is relevant given the small size of the source text provided in the test cases.
;;; 
;;; The following snippets show how these functions interact to produce the intended result.
;; **

;; @@
(def runs `((\A \A) (\B \B \B) (\C \C \C \C)))
(def values (map dedupe runs))
(def lengths (doall (map count runs)))
runs
values
lengths
(interleave lengths values)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>2</span>","value":"2"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\A</span>","value":"\\A"}],"value":"(\\A)"},{"type":"html","content":"<span class='clj-unkown'>3</span>","value":"3"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\B</span>","value":"\\B"}],"value":"(\\B)"},{"type":"html","content":"<span class='clj-unkown'>4</span>","value":"4"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\C</span>","value":"\\C"}],"value":"(\\C)"}],"value":"(2 (\\A) 3 (\\B) 4 (\\C))"}
;; <=

;; **
;;; #### Encoding - Threading macros
;;; 
;;; The other significant part of the idiomatic solution is the use of the [thread-last macro](https://clojure.org/guides/threading_macros) (```->>```) to create a pipeline of calls requiring the parameter ```s``` as a value.  I have a love-hate relationship with threading macros in Clojure.  I _love_ them when I see an opportunity to use them because I consider them to be highly idiomatic, but I also _hate_ them because my personal style of using lots of intention revealing ```let``` calls often makes it hard to use them.
;;; 
;;; The *big* potential advantage of threading macros is that they invert the typical way you read Clojure code and a natural top-to-bottom interpretation is much more applicable.
;;; 
;;; The *big* potential disadvantage of threading macros is that there is a lot of hidden flow that will be bewildering to novice and occasional Clojure developers.
;;; 
;;; After a quick experiment, it doesn't look like there is a ready way to use threading in my solution due to my high level approach of creating parallel collection of run values and lengths and then combining them.  The threading solution is a far better fit for the idiomatic solution that build the solution piece-by-piece.
;;; 
;;; #### Encoding - Optimisation
;;; 
;;; Looking at my solution in comparison to the idiomatic one more closely, I can see another opportunity to reduce the variation between the two by using ```first``` rather than ```dedupe``` to extract the value for each group - ```dedupe``` seems like overkill now given I have already know each element in the groups produced by ```partition-by identity``` are guaranteed to be the same.  A quick experiment shows that although the result from ```dedupe``` to ```first``` is slightly different, those differences are still handled by the combination of ```interleave``` and ```apply str``` *and* I can remove the call to ```flatten``` in my solution.
;; **

;; @@
(def runs `((\A \A) (\B \B \B) (\C \C \C \C)))
(def values (map first runs))
(def lengths (doall (map count runs)))
runs
values
lengths
(def result (interleave lengths values))
result
(apply str result)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;2A3B4C&quot;</span>","value":"\"2A3B4C\""}
;; <=

;; **
;;; #### Encoding - WTF?!?
;;; 
;;; Another thing I spotted in this close comparison is that my use of ```do-all``` was completely unnecessary.  It certainly didn't cause any problems, but it can happily be omitted without breaking the solution.  I'm sure it made sense to me at the time that I added it in :-)
;;; 
;;; ### Encoding - Final version
;; **

;; @@
;;; Idiomatic version
(defn run-length-encode
  [s]
  (->> s
       (partition-by identity)
       (mapcat #(if (= 1 (count %))
                  [(first %)]
                  [(count %) (first %)]))
       (apply str)))

;;; My (refactored) version
(defn run-length-encode
  [s]
  (let [runs (partition-by identity s)] ;; group by value
    (apply str ;; combine counts and lengths into single string
           (remove #(= % 1) ;; remove any single length run counts
                   (interleave ;; combine the corresponding count and value
                    (map count runs) ;; count the length of each run
                    (map first runs)))))) ;; isolate the value of each run
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;run-length-encoding-notebook.core/run-length-encode</span>","value":"#'run-length-encoding-notebook.core/run-length-encode"}
;; <=

;; **
;;; 
;;; The conclusion from all these minor changes is this version which I have split onto more lines to support clearer line-by-line documention. Unfortunately this documentation does not _flow_ from top to bottom given the deeply nested nature of idiomatic Clojure solutions.
;;; 
;;; Returning to the original criteria for comparison I used at the top of this article, _but just for the encoding_:
;;; 
;;; | | My solution | Other solution |
;;; |-----|-----|
;;; |Lines of code|20 -> 8|8|
;;; |Functions|6 -> 1|1|
;;; |Local variables|1 -> 1|0|
;;; 
;;; I've extended this comparison to look at the common and unique functions used in each solution.  There are now 7 common functions, 4 unique functions in my solution and 2 unique functions in the idiomatic solution.  The use of the powerful ```mapcat``` (a Clojure function I'm aware of, but never reach for) is likely doing some heavy lifting here that I could use instead of ```map``` and ```interleave``` and the high level choice to incrementally build the solution value-by-value means that the idiomatic solution does not need to rely on the use of ```remove``` to cleanup single length run counts after the fact.
;;; 
;;; | My solution | Other solution | Both solutions |
;;; |-----|-----|-----|
;;; |```let``` |```->>```|```partition-by``` |
;;; |```remove``` |```mapcat```|```identity``` | 
;;; |```interleave``` ||```apply```|
;;; |```map```||```str``` |
;;; |||```count``` | 
;;; |||```first``` |
;;; |||```=``` |
;;; 
;;; I am happy with this solution _for now_.  It is clear and as intentional as I think I can make it without resorting to more named variables (via ```let```) or functions.  I hope this clarity remains after I return to look at this code in the weeks/months to come.
;; **

;; **
;;; ### Step 3 - Decoding
;;; 
;;; We've spent enough time picking at the low hanging fruit - it's time to give some attention to the decoding part of the problem.  
;; **

;; @@
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
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;run-length-encoding-notebook.core/run-length-decode</span>","value":"#'run-length-encoding-notebook.core/run-length-decode"}
;; <=

;; **
;;; When we left my version of decoding, I still had a single extra function that I was struggling to inline because it was relatively complex itself and would blow out the overall complexity of the main ```run-length-decode```.  
;;; 
;;; Technically, the inlining is relatively trivial though, so if you want to see the monstrosity that it would produce...
;; **

;; @@
(defn run-length-decode
  [s]
  (let [components (re-seq #"\d+[^\d]+" s)]
    (if (nil? components)
      s
      (apply str (map #(let [length (Integer/parseInt (first (re-seq #"\d+" %)))
                             value (first (re-seq #"[^\d]+" %))
                             run (apply str (repeat length (get value 0)))]
                         (if (<= 1 (count value))
                           (str run (apply str (rest value)))
                           run)) components)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;run-length-encoding-notebook.core/run-length-decode</span>","value":"#'run-length-encoding-notebook.core/run-length-decode"}
;; <=

;; **
;;; compared to...
;; **

;; @@
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;run-length-encoding-notebook.core/run-length-decode</span>","value":"#'run-length-encoding-notebook.core/run-length-decode"}
;; <=

;; **
;;; Unsurprisingly, you can see a lot of common syntactic elements in the encoding and decoding functions in the idiomatic solution; thread last macro (twice), ```mapcat``` with an associated ```if``` in particular.
;;; 
;;; Other things of note in the idiomatic solution:
;;; * The regular expression seems to be doing more heavy lifting here compared to my solution
;;; * There is no specific logic to handle the ```(nil? components)``` check I have in my version... I wonder how the author got around this?
;;; * The nested thread-last macros is very hard for me to comprehend in the broader scope of the solution.  This might be an easy read for an experienced Clojure developer though
;;; 
;;; Because I'm on a journey of discovery, let's see if we can demystify those points above, starting with the regex...
;; **

;; @@
(re-seq #"\d+[^\d]+" "2A3B4CD") ;; my version
(re-seq #"[1-9]*[a-zA-Z ]" "2A3B4CD") ;; idiomatic version
(re-seq #"\d+[^\d]+" "XYZ") ;; my version
(re-seq #"[1-9]*[a-zA-Z ]" "XYZ") ;; idiomatic version
(re-seq #"\d+[^\d]+" "12WB12W3B24WB") ;; my version
(re-seq #"[1-9]*[a-zA-Z ]" "12WB12W3B24WB") ;; idiomatic version
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;12W&quot;</span>","value":"\"12W\""},{"type":"html","content":"<span class='clj-string'>&quot;B&quot;</span>","value":"\"B\""},{"type":"html","content":"<span class='clj-string'>&quot;12W&quot;</span>","value":"\"12W\""},{"type":"html","content":"<span class='clj-string'>&quot;3B&quot;</span>","value":"\"3B\""},{"type":"html","content":"<span class='clj-string'>&quot;24W&quot;</span>","value":"\"24W\""},{"type":"html","content":"<span class='clj-string'>&quot;B&quot;</span>","value":"\"B\""}],"value":"(\"12W\" \"B\" \"12W\" \"3B\" \"24W\" \"B\")"}
;; <=

;; **
;;; You can see immediately that the idiomatic version handles the single length run (without the value) whereas my version does not.  _Also_, the idiomatic version returns a value result for the condition where I needed a specific ```if``` handler!
;;; 
;;; #### Decoding - One regex to rule them all
;;; 
;;; I'm keen to change my solution to use the more advanced regex, _but_ the surface area of the change is broader than I would like as I have the subsequent ```if``` condition to handle the nil return scenario from my original regex *and* another regex later that assumed there would be an integer part of each component returned from the original regex.  The simplest change I can do to incorporate the more powerful regex allows me to remove the initial ```if``` but it then forces my hand to add a new ```if``` to guard against an implicit single character run length.  See below.
;; **

;; @@
(defn run-length-decode
  [s]
  (let [components (re-seq #"[1-9]*[a-zA-Z ]" s)]
    (apply str (map #(let [length-raw (re-seq #"\d+" %)
                           length (if (nil? length-raw) 1 (Integer/parseInt (first length-raw)))
                           value (first (re-seq #"[^\d]+" %))
                           run (apply str (repeat length (get value 0)))]
                       run) components))))

(run-length-decode "12WB12W3B24WB")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB&quot;</span>","value":"\"WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB\""}
;; <=

;; **
;;; Still, I am happy with this solution in terms of length, and although I cannot look at the ```map``` body without wanting to pull a new function out of it, I can probably iterate a little further.
;;; 
;;; Let's see whether we can unravel the nested thread-last macro in the idiomatic solution to see what it does...
;;; 
;; **

;; @@
(->> "2A" drop-last (apply str) Integer.)
(->> "12W" drop-last (apply str) Integer.)
(repeat (->> "2A" drop-last (apply str) Integer.) (last "2A"))
(repeat (->> "12W" drop-last (apply str) Integer.) (last "12W"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>(\\W \\W \\W \\W \\W \\W \\W \\W \\W \\W \\W \\W)</span>","value":"(\\W \\W \\W \\W \\W \\W \\W \\W \\W \\W \\W \\W)"}
;; <=

;; **
;;; I like the use of ```drop-last``` here to isolate the run value from the length + value pair.  I've used another regex for this, but I have enough confidence in the format of that data, I could certainly use ```drop-last``` as well.
;; **

;; @@
(defn run-length-decode
  [s]
  (let [components (re-seq #"[1-9]*[a-zA-Z ]" s)]
    (apply str (map #(let [length-raw (re-seq #"\d+" %)
                           length (if (nil? length-raw) 1 (Integer/parseInt (first length-raw)))
                           value (last %) ;;; using last instead of regex
                           run (apply str (repeat length value))] ;;; no longer a need to use get
                       run) components))))

(run-length-decode "12WB12W3B24WB")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB&quot;</span>","value":"\"WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB\""}
;; <=

;; **
;;; This change had the added benefit of simplifying my approach to passing ```value``` to my ```repeat``` function call because it is now a single value rather than a collection that I need to access via ```get```.  Line #7 is now simple enough that I can return it via the body of the ```let``` rather than binding it to a variable which I then return.
;; **

;; @@
(defn run-length-decode
  [s]
  (let [components (re-seq #"[1-9]*[a-zA-Z ]" s)]
    (apply str (map #(let [length-raw (re-seq #"\d+" %)
                           length (if (nil? length-raw) 1 (Integer/parseInt (first length-raw)))
                           value (last %)]
                       (apply str (repeat length value))) components)))) ;;; inlined the old run variable here

(run-length-decode "12WB12W3B24WB")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB&quot;</span>","value":"\"WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB\""}
;; <=

;; **
;;; The one final change I'm keen to look at is doing away with the regex on Line #4 via ```drop-last```.  While I am there, I can also use the shorter but no less readable ```Integer.``` instead of my original ```Integer/parseInt```.
;; **

;; @@
(defn run-length-decode
  [s]
  (let [components (re-seq #"[1-9]*[a-zA-Z ]" s)]
    (apply str (map #(let [length-raw (apply str (drop-last %))
                           length (if (= 0 (count length-raw)) 1 (Integer. length-raw))
                           value (last %)]
                       (apply str (repeat length value))) components))))

(run-length-decode "12WB12W3B24WB")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB&quot;</span>","value":"\"WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB\""}
;; <=

;; **
;;; #### Decoding - Removing all let blocks
;;; 
;;; For the sake of the experiment, I want to see what my solution looks like when I remove all my ```let``` blocks.  I'm expecting an unreadable mess and some duplication around the ```length-raw``` expression, but this is a good time to refactor _beyond_ where I would stop when writing production code.
;; **

;; @@
(defn run-length-decode
  [s]
  (apply str (map #(apply
                    str
                    (repeat
                     (if (= 0 (count (apply str (drop-last %))))
                       1
                       (Integer. (apply str (drop-last %))))
                     (last %)))
                  (re-seq #"[1-9]*[a-zA-Z ]" s))))

(run-length-decode "12WB12W3B24WB")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB&quot;</span>","value":"\"WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB\""}
;; <=

;; **
;;; Don't be confused by the higher number of lines - I deliberately split several of the lines for clarity that wasn't needed when I was using my trusty ```let``` blocks.  As I suspect, this version is beyond my tolerance for density in Clojure.  Time to revert to the previous version.
;;; 
;;; ### Decoding - Final version
;; **

;; @@
;;; Idiomatic version
(defn run-length-decode
  [s]
  (->> s
       (re-seq #"[1-9]*[a-zA-Z ]")
       (mapcat #(if (= 1 (count %))
                  %
                  (repeat (->> % drop-last (apply str) Integer.) (last %))))
       (apply str)))

;;; My (refactored) version
(defn run-length-decode
  [s]
  (let [components (re-seq #"[1-9]*[a-zA-Z ]" s)]
    (apply str (map #(let [length-raw (apply str (drop-last %))
                           length (if (= 0 (count length-raw)) 1 (Integer. length-raw))
                           value (last %)]
                       (apply str (repeat length value))) components))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;run-length-encoding-notebook.core/run-length-decode</span>","value":"#'run-length-encoding-notebook.core/run-length-decode"}
;; <=

;; **
;;; | | My solution | Other solution |
;;; |-----|-----|
;;; |Lines of code|15 -> 7|8|
;;; |Functions|2 -> 1|1|
;;; |Local variables|4 -> 4|0|
;;; 
;;; Comparing the function usage between the two versions, there is a much higher amount of commonality between the two.  There is also the same number of functions used in both solutions.
;;; 
;;; | My solution | Other solution | Both solutions |
;;; |-----|-----|-----|
;;; |```map``` |```->>```|```re-seq``` |
;;; |```let``` |```mapcat```|```count``` | 
;;; |||```apply```|
;;; |||```str``` |
;;; |||```count``` | 
;;; |||```drop-last``` |
;;; |||```first``` |
;;; |||```if``` |
;;; |||```=``` |
;; **

;; @@

;; @@
