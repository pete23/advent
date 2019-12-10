(ns day-9
  (:use clojure.test)
  (:require [intcode]))

(def input (intcode/load-file "day-9.input"))

(defn part-1 []
  (:output (intcode/run-program input [1])))

(defn part-2 []
  (:output (intcode/run-program input [2])))

