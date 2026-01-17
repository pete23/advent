(ns advent.y2024.day-8
  (:require [advent.core :refer [parse-grid cat squares add sub]]))

(def test-input "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(def single-test "..........
..........
..........
....a.....
..........
.....a....
..........
..........
..........
..........")

(def input (slurp "resources/advent/y2024/day_8.txt"))

(defn locate-stations [grid]
  (let [station-locations (reduce (fn [acc coord]
                                    (let [c (cat grid coord)]
                                      (if (= c \.)
                                        acc
                                        (update acc c conj coord))))
                                  {}
                                  (squares grid))]
    (assoc grid :stations station-locations)))

(defn pairs [s]
  (mapcat (fn [p] (map #(vector p %) (filter #(not= p %) s))) s))

(defn calculate-antinode [grid [p1 p2]]
  (let [node (add p1 (sub p1 p2))]
    (when (cat grid node) node)))

(defn calculate-antinodes [{:keys [stations] :as grid}]
  (reduce (fn [acc station]
            (assoc acc (first station) (filter identity (map (partial calculate-antinode grid) (pairs (second station))))))
          {}
          stations))

(defn part-1 [input]
  (->> input
       parse-grid
       locate-stations
       calculate-antinodes
       vals
       (reduce into #{})
       count))

(defn calculate-antinode-2 [grid [p1 p2]]
  (let [vect (sub p1 p2)]
    (loop [acc [p1] p p1] ;; paired stations are automatically antinodes
      (let [next-p (add p vect)]
        (if (cat grid next-p)
          (recur (conj acc next-p) next-p)
          acc)))))

(defn calculate-antinodes-2 [{:keys [stations] :as grid}]
  (reduce (fn [acc station]
            (assoc acc (first station) (filter identity (mapcat (partial calculate-antinode-2 grid) (pairs (second station))))))
          {}
          stations))

(defn part-2 [input]
  (->> input
       parse-grid
       locate-stations
       calculate-antinodes-2
       vals
       (reduce into #{})
       count))

(part-2 input)
(input-to-station-locations test-input)
(defn part-1 [input])

(deftest part-1-test
  (is (= (part-1 test-input)
         12345)))

(defn part-2 [input])

(deftest part-2-test
  (is (= (part-2 test-input)
         12345)))
