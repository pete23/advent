(ns advent.y2023.d2
  (:require [clojure.string :refer [split split-lines]]))

(def test-input (split-lines (slurp "resources/advent/y2023/d2-test.txt")))
(def input (split-lines (slurp "resources/advent/y2023/d2.txt")))

(defn atol [a]
  (Long/parseLong a))

(defn parse-count [c]
  (let [[n colour] (split c #" ")]
    {(keyword colour) (atol n)}))
   
(defn parse-draw [s]
  ;; 3 blue, 4 red
  ;; {:blue 3 :red 4}
  (into {} (map parse-count (split s #", "))))

(defn parse-line [s]
  ;; Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
  ;; {1 [{:blue 3 :red 4} {:red 1 :green 2 :blue 6} {:green 2}]}
  (let [[game draws] (split s #": ")
        game-number (atol (second (split game #" ")))
        draws (split draws #"; ")]
    {game-number (mapv parse-draw draws)}))

(defn parse-input [i]
  (apply merge (map parse-line i)))

(defn max-cubes-used [game]
  (apply merge-with max game))

(defn possible? [counters game]
  (let [mcu (max-cubes-used game)
        pred (fn [turn]
               (every? (fn [[colour n]]
                         (<= n (get counters colour 0))) turn))]
    (every? pred game)))

(def counters {:red 12 :green 13 :blue 14})

(defn part-1 [input]
  (reduce + (map first (filter (fn [[_ game]] (possible? counters game)) (parse-input input)))))

(defn part-2 [input]
  (reduce + 
          (map #(reduce * (map second %)) (map max-cubes-used (map second (parse-input input))))))

