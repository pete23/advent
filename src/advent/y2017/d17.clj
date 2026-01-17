(ns advent.y2017.d17
  (:use clojure.test))

(defn insert-at [v i n]
  (let [[before after] (split-at i v)]
       (vec (concat before (vector n) after))))

(defn spin [n step [pos buf]]
  (let [new-pos (inc (rem (+ step pos) (count buf)))
        new-buf (insert-at buf new-pos n)]
    [new-pos new-buf]))

(defn part-1
  ([] (part-1 386))
  ([step] (let [[pos buffer] (reduce #(spin %2 step %1) [0 [0]] (range 1 2018))]
            (nth buffer (inc pos)))))

(deftest part-1-check
  (is (= 638 (part-1 3))))

;; we don't need to actually hold the buffer, just to do the maths
;; n is always == len it turns out... this is actually faster than
;; (reduce + (range 1 50000001)) :-)
(defn fast-spin [^long n ^long step ^long pos ^long after-zero]
  (if (== n 50000001) after-zero
      (let [new-pos (inc (rem (+ step pos) n))
            new-after-zero (if (== new-pos 1) n after-zero)]
        (recur (inc n) step new-pos new-after-zero))))
        
(defn part-2 [] (fast-spin 1 386 0 0))
