(ns advent.y2018.d5
  (:require [clojure.test :refer :all]))

(defn read-input [filename]
  (-> filename
      clojure.java.io/resource
      slurp))

(def input (read-input "y2018/day-5.input"))
(def test-input "dabAcCaCBAcCcaDA")

(defn atol [a] (Long/parseLong a))

(defn different-case-same-letter [left right]
  (and (= (Character/toLowerCase left) (Character/toLowerCase right))
       (not (= (Character/isLowerCase left) (Character/isLowerCase right)))))

(defn react [^StringBuilder sb c]
  (let [l (.length sb)]
    (if (and (> l 0) (different-case-same-letter (.charAt sb (dec l)) c))
      (.deleteCharAt sb (dec l))
      (.append sb c))))

(defn process [input]
  (reduce #(react %1 %2) (StringBuilder.) input))

(deftest part-one-test
  (is (= "dabCBAcaDA" (apply str (process "dabAcCaCBAcCcaDA")))))

(defn part-one []
  (count (process input)))

;; i tried to think about decomposing it but then i just wrote it
;; sorry/not sorry
(defn part-two []
  (apply min-key second
       (for [c "abcdefghijklmnopqrstuvwxyz"]
         (let [C (Character/toUpperCase c)]
           [c (count (process (filter #(and (not= c %) (not= C %)) input)))]))))
