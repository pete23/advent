(ns advent.y2024.day-7
  (:require [advent.core :refer [atol]]))

(def test-input "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(def input (slurp "resources/advent/y2024/day_7.txt"))

(defn parse [input]
  (map #(map atol (re-seq #"\d+" %)) (clojure.string/split-lines input)))

(defn reducer [ops acc b]
  (mapcat (fn [op] (map #(op % b) acc)) ops))

(defn check-line [ops [check f & r]]
  (if ((set (reduce (partial reducer ops) [f] r)) check)
    check))


(defn solve [ops input]
  (reduce + (filter identity (map (partial check-line ops) (parse input)))))
(defn part-1 [input]
  (solve [* +] input))
  

(part-1 input)

(defn concat-op [a b]
  (atol (str a b)))

(defn part-2 [input]
  (solve [* + concat-op] input))

(part-2 input)
