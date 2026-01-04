(ns advent.y2019.d9
  (:use clojure.test)
  (:require [advent.y2019.intcode :as intcode]))

(def input (intcode/load-file "y2019/day-9.input"))

(defn part-1 []
  (:output (intcode/run-program input [1])))

(defn part-2 []
  (:output (intcode/run-program input [2])))

