(ns day-2
  (:use clojure.test)
  (:require intcode))

(def input (intcode/load-file "day-2.input"))

(defn try-program [noun verb]
  (first
   (intcode/run-program
    (assoc input 1 noun 2 verb))))

(defn part-1 []
  (try-program 12 2))

(defn check-noun-verb [[noun verb]]
  (if (= (try-program noun verb) 19690720) [noun verb]))

(defn part-2 []
  (some check-noun-verb (for [noun (range 0 99)
                              verb (range 0 99)]
                          [noun verb])))
