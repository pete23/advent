(ns advent.y2019.d5
  (:use clojure.test)
  (:require [advent.y2019.intcode :as intcode]))

(def input (intcode/load-file "y2019/day-5.input"))

(defn part-1 []
  (intcode/run-program input [1]))

(defn part-2 []
  (intcode/run-program input [5]))
