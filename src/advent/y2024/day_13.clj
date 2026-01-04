(ns advent.y2024.day-13
  (:require [clojure.core.matrix.linear :refer [solve]]
            [clojure.core.matrix :refer [set-current-implementation]]
            [clojure.math :refer [round]]
            [advent.core :refer [atol]]))

(def test-input "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

(defn parse-1 [input]
  (let [[ax ay bx by x y] (mapv atol (re-seq #"[0-9]+" input))]
    [[[ax bx] [ay by]] [(+ 10000000000000 x) (+ 10000000000000 y)]]))

(defn parse [input]
  (map parse-1 (clojure.string/split input #"\n\n")))

(defn integery? [x]
  (let [error 0.0001
        integer (clojure.math/round x)]
    (println (abs (- x integer)))
    (println integer)
    (println x)
    (when (< (abs (- x integer)) error)
      integer)))

(defn check [[a b] [[ax bx] [ay by]] [x y]]
  (and (= (+ (* a ax) (* b bx)) x)
       (= (+ (* a ay) (* b by)) y)))

(defn part-1 [input]
  (reduce (fnil + 0 0)
          (map #(let [[a b] (map clojure.math/round (apply solve %))]
                  (when (apply check [a b] %)
                    (+ (* 3 a) b))) (parse input))))


(def input (slurp "resources/advent/y2024/day_13.txt"))
