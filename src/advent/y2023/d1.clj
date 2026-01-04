(ns advent.y2023.d1
  (:require [advent.core :refer [ctoi]]))

(defn calc [digits]
  (+ (* 10 (first digits)) (last digits)))

(def matches
  [["1" 1]
   ["2" 2]
   ["3" 3]
   ["4" 4]
   ["5" 5]
   ["6" 6]
   ["7" 7]
   ["8" 8]
   ["9" 9]
   ["one" 1]
   ["two" 2]
   ["three" 3]
   ["four" 4]
   ["five" 5]
   ["six" 6]
   ["seven" 7]
   ["eight" 8]
   ["nine" 9]])

(defn match-1 [s]
  (some (fn [[k v]]
          (when (clojure.string/starts-with? s k) v)) matches))

(defn parse-line-1 [l]
  (filter identity (map ctoi l)))

(defn part [parse-fn]
  (reduce +
          (map calc
               (map #(filter identity (parse-fn %))
                    (clojure.string/split
                     (slurp "resources/advent/y2023/d1.txt") #"\n")))))
(defn part-1 []
  (part #(map ctoi %)))

(defn substs
  ([s] (substs [] s))
  ([a s]
   (if (= s "") a
       (recur (conj a s) (subs s 1)))))

(defn part-2 []
  (part #(map match-1 (substs %))))
