(ns advent.y2018.d1
  (:require [clojure.test :refer :all]))

(def input (-> "y2018/day-1.input"
               clojure.java.io/resource
               slurp
               (clojure.string/split #"\n")))

(defn atol [s] (Long/parseLong s))

(defn part-1 []
  (reduce + (map atol input)))

(defn find-second-occurrence [a v]
  (if (a v) (reduced v)
      (conj a v)))

(defn first-freq-twice [input]
  (let [frequencies (reductions + (cycle input))]
    (reduce find-second-occurrence #{0} frequencies)))

(deftest part-2-test
  (is (= 0 (first-freq-twice [1 -1])))
  (is (= 10 (first-freq-twice [3 3 4 -2 -4])))
  (is (= 5 (first-freq-twice [-6 3 8 5 -6])))
  (is (= 14 (first-freq-twice [7 7 -2 -7 -4]))))
  
(defn part-2 []
  (first-freq-twice (map atol input)))

