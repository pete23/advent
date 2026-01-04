(ns advent.y2024.d1
  (:require [advent.core :refer [string->grid]]))



(def test-input "3   4
4   3
2   5
1   3
3   9
3   3")

(defn part-1 [s]
  (let [input (string->grid s)]
    (reduce + (map abs (map - (sort (map first input)) (sort (map second input)))))))

(part-1 (slurp "resources/advent/y2024/day_1.txt"))

(defn part-2 [s]
  (let [input (string->grid s)
        left (map first input)
        freqs (frequencies (map second input))]
    (reduce + (map #(* % (or (freqs %) 0)) left))))

(part-2 (slurp "resources/advent/y2024/day_1.txt"))
