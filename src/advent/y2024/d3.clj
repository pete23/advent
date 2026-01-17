(ns advent.y2024.d3
  (:require [advent.core :refer [atol]]
            [clojure.test :refer :all]))

(def test-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
(def input (slurp "resources/y2024/day_3.txt"))

(defn parse-input [s]
  (map #(map atol (rest %)) (re-seq #"mul\((\d+),(\d+)\)" s)))

(defn part-1 [input]
  (reduce + (map #(apply * %) (parse-input input))))

(deftest part-1-example
  (is (= '((2 4) (5 5) (11 8) (8 5))
         (parse-input test-input)))
  (is (= 161 (part-1 test-input))))

(def test-input-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn parse-input-2 [s]
  (map #(filter identity (rest %))
       (re-seq #"(do)\(\)|(don't)\(\)|(mul)\((\d+),(\d+)\)" test-input-2)))

(defn part-2 [input]
  (reduce (fn [acc [op x y]]
            (cond
              (= "do" op) (assoc acc :doing true)
              (= "don't" op) (assoc acc :doing false)
              (:doing acc) (update acc :total + (* (atol x) (atol y)))
              :else acc))
          {:doing true :total 0}
          (parse-input-2 input)))

(deftest part-2-example
  (is (= 48 (:total (part-2 test-input-2)))))
