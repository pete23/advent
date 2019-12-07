(ns day-7
  (:use clojure.test)
  (:require [intcode]
            [clojure.math.combinatorics :as combo]))

(def input (intcode/load-file "day-7.input"))

(defn run-program [program signal phase]
  (first (:output (intcode/run-program program (vector phase signal)))))

(defn run-phases [program p]
  (reduce (partial run-program program) 0 p))

(deftest test-first-signal
  (is (= 43210 (run-phases  [3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0] [4 3 2 1 0]))))

(defn find-max-signal [program]
  (apply max (map (partial run-phases program) (combo/permutations [0 1 2 3 4]))))

(deftest test-examples
  (is (= 43210 (find-max-signal [3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0])))
  (is (= 54321 (find-max-signal [3 23 3 24 1002 24 10 24 1002 23 -1 23 101 5 23 23 1 24 23 23 4 23 99 0 0]))))

(defn part-1 []
  (find-max-signal input))
