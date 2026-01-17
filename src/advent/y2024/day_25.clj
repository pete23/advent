(ns advent.y2024.day-25
  (:require [clojure.test :refer :all]))

(defn parse-heights [lines]
  (reduce (fn [acc line]
            (map #(if (= %1 \#) (inc %2) %2) line acc))
          [0 0 0 0 0]
          lines))

(defn parse [input]
  (let [locks-and-keys (mapv clojure.string/split-lines (clojure.string/split input #"\n\n"))]
    (reduce (fn [acc lock-or-key]
              (if (= "#####" (first lock-or-key))
                (update acc :locks conj (parse-heights (rest lock-or-key)))
                (update acc :keys conj (parse-heights (rest (reverse lock-or-key))))))
            {:locks [] :keys []}
            locks-and-keys)))

(def test-input "#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####")

(def input (slurp "resources/advent/y2024/day_25.txt"))

(deftest check-parse
  (is (= {:locks [[0,5,3,4,3] [1,2,0,5,3]]
          :keys [[5,0,2,1,3] [4,3,4,0,2] [3,0,2,0,1]]}
         (parse test-input))))

(defn part-1 [input]
  (let [{:keys [keys locks]} (parse input)]
    (count
     (filter identity
             (for [lock locks key keys]
               (every? #(< % 6) (map + lock key)))))))

(deftest part-1-test
  (is (= 3 (part-1 test-input))))

(part-1 input)
