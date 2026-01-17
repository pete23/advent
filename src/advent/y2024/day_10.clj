(ns advent.y2024.day-10
  (:require [advent.core :refer [parse-grid cat find-char ctol add]]
            [dom-top.core :refer [loopr]]))

(defn parse [input]
  (update (parse-grid input) :grid #(mapv (fn [s] (mapv ctol s)) %)))

(def test-input "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

(def directions [[1 0] [-1 0] [0 1] [0 -1]])

(defn build-path [grid path]
  (let [next-step (count path)
        coordinate (nth path (dec next-step))]
    (map #(conj path %)
         (filter #(= next-step (cat grid %))
                 (map (partial add coordinate) directions)))))
        
(defn build-paths [grid paths]
  ;;(println "iteration" (count (first paths)))
  ;;(doseq [path paths] (println path))
  ;;(println)
  (if (= (count (first paths)) 10)
    paths
    (recur grid (set (mapcat (partial build-path grid) paths)))))


(defn text->paths [input]
  (let [grid (parse input)
        start-points (find-char grid 0)
        start-paths (map vector start-points)]
    (build-paths grid start-paths)))

(defn part-1 [input]
  (count (set (map (juxt first last) (text->paths input)))))

(def input (slurp "resources/advent/y2024/day_10.txt"))

(defn part-2 [input]
  (count (set (text->paths input))))

(part-2 input)
