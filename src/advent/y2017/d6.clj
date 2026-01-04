(ns advent.y2017.d6
  (:use clojure.test))

;; return the index of the maximum value
(defn max-idx
  ([v] (max-idx v 0 -1 Long/MIN_VALUE))
  ([v i max-i max-v]
   (if (= (count v) i) max-i
       (if (< max-v (nth v i))
         (recur v (inc i) i (nth v i))
         (recur v (inc i) max-i max-v)))))

;; suspect we want to transient this for perf, let's see
(defn inc-from-i-n-times-wrapping [v i n]
  (if (= n 0) v
      (let [i (if (= i (count v)) 0 i)] ; wrap if necessary
        (recur (assoc v i (inc (nth v i)))
               (inc i)
               (dec n)))))
    
(defn redistribute [v]
  (let [maximum-index (max-idx v)
        maximum-value (nth v maximum-index)]
    (inc-from-i-n-times-wrapping (assoc v maximum-index 0) (inc maximum-index) maximum-value)))

(def day-6-test [0 2 7 0])

(deftest check-redistribution
  (is (= [2 4 1 2] (redistribute day-6-test))))

;; this now includes the redistribution which fails the check to enable part 2
;; so you need to take one off your count to answer part 1!
(defn redistributed-result-seq
  ([v] (redistributed-result-seq v #{}))
  ([v s] (if (s v) (vector v)
             (cons v (lazy-seq (redistributed-result-seq (redistribute v) (conj s v)))))))

(deftest check-redistributed-seq
  (is (= 5 (dec (count (redistributed-result-seq day-6-test))))))

(def day-6-input [4 10 4 1 8 4 9 14 5 1 14 15 0 15 3 5])

(defn day-6-1 []
  (dec (count (redistributed-result-seq day-6-input))))

(defn day-6-2 []
  (let [distributions (redistributed-result-seq day-6-input)
        loop-value (last distributions)
        loop-start (some #(if (= loop-value (second %)) (first %))
                         (map-indexed #(vector %1 %2) distributions))]
    (- (dec (count distributions)) loop-start)))


