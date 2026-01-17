(ns advent.y2024.day-15
  (:require [advent.core :refer [parse-grid cat add find-char]]))

(def test-input "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<")

(defn parse [input]
  (let [[input-grid input-inst] (clojure.string/split input #"\n\n")
        grid (update (parse-grid input-grid) :grid #(mapv vector %))
        robot (first (find-char grid \@))]
        
    {:grid grid
     :inst input-inst
     :robot robot}))

(parse test-input)

