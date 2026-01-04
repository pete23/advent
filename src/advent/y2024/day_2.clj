(ns advent.y2024.d2
  (:require [advent.core :refer [string->grid]]
            [clojure.test :refer :all]
            [clojure.math :refer [signum]]))

(def test-input "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(def test-grid (string->grid test-input))

(def grid (string->grid (slurp "resources/advent/y2024/day_2.txt")))

(defn safe? [report]
  (let [diffs (map - report (rest report))
        signs (map signum diffs)
        absdf (map abs diffs)]
    (and (every? #(= (first signs) %) (rest signs))
         (every? #(< 0 % 4) absdf))))

(deftest example
  (is (= [true false false false false true]
         (mapv safe? test-grid))))

(defn part-1 [input]
  (count (filter safe? input)))

(deftest example-answer
  (is (= 2 (part-1 test-grid))))

(part-1 grid)

(defn with-one-removed [line]
  (let [l (count line)]
    (reduce #(conj %1 (into [] (concat (take %2 line) (drop (inc %2) line))))  [] (range 0 l))))

(defn safe-with-skip? [line]
  (some safe? (with-one-removed line)))

(deftest part-2-example
  (is (= [true nil nil true true true]
         (map safe-with-skip? test-grid))))

(defn part-2 [input]
  (count (filter safe-with-skip? input)))

(part-2 grid)

