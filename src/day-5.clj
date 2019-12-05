(ns day-5
  (:use clojure.test)
  (:require intcode))

(def input (intcode/load-file "day-5.input"))

(defn part-1 []
  (intcode/run-program input [1]))

(defn part-2 []
  (intcode/run-program input [5]))
