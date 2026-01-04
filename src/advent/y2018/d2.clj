(ns advent.y2018.d2
  (:require [clojure.test :refer :all]))

(def input (-> "y2018/day-2.input"
               clojure.java.io/resource
               slurp
               (clojure.string/split #"\n")))

(def test-input ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"])

(defn checksum [input]
  ;; checksum is number of strings with an occurrence of two letters times that with three
  (let [letter-counts (map #(set (map second (frequencies %))) input)]
    (* (count (filter #(% 2) letter-counts))
       (count (filter #(% 3) letter-counts)))))

(deftest part-1-test
  (is (= 12 (checksum test-input))))

(defn part-1 []
  (checksum input))

(defn common-chars [a b]
  (filter identity (map #(if (= %1 %2) %1) a b)))

(defn part-2 []
  (let [full-length (count (first input))]
    (some #(if (= (count %) (dec full-length)) %)
          (for [x input y input]
            (common-chars x y)))))

