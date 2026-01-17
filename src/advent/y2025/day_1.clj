(ns advent.y2025.d1
  (:require [advent.core :refer [lines atol]]
            [dom-top.core :refer [loopr]]))

(defn parse-input [input]
  (map (fn [line]
         (let [mag (atol (subs line 1))]
           (if (= \L (first line))
             (- mag)
             mag))) (lines input)))

(def input (slurp "resources/advent/y2025/day_1.txt"))

(def test-input "L68
L130
R148
L5
R60
L55
L1
L99
R14
L82")

(def size 100)

(defn add [a b]
  (mod (+ a b) size))




(defn part-1 [input]
  (count (filter zero? (reductions add 50 (parse-input input)))))

(part-1 input)

(defn part-2 [input]
  (loopr
      [position 50 zero-passes 0]
      [turn (parse-input input)]
      (let [whole-turns (quot (abs turn) size)
            remaining-turn (rem turn size)
            next-position (add position turn)]
        (recur next-position
               (+ zero-passes
                  whole-turns
                  (if (and (not= position 0)
                           (not= remaining-turn 0)
                           (or (= next-position 0)
                               (not= next-position (+ position remaining-turn))))
                    1 0))))
    zero-passes))

