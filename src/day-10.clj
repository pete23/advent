(ns day-10
  (:use clojure.test))

(defn line->vec [line]
  (mapv { \. 0 \# 1 } line))

(def input (->> "day-10.input"
                clojure.java.io/resource
                slurp
                clojure.string/split-lines
                (mapv line->vec)))

;; take all stars and make relative to current star
;; calculate angles of stars
;; set
;; count

(defn cartesian->theta [x y]
  (Math/atan2 y x))

(defn grid->coords [grid]
  (for [x (range (count (first grid)))
        y (range (count grid))
        :let [v ((grid y) x)]
        :when (> v 0)]
    [x y]))

(defn calculate-visible [[x y] coords]
  (let [others (remove #(= % [x y]) coords)
        relative (map (fn [[x1 y1]] [(- x1 x) (- y1 y)]) others)
        thetas (map (fn [[x y]] (cartesian->theta x y)) relative)]
    (count (set thetas))))

(defn visible-from-grid [grid]
  (let [coords (grid->coords grid)
        visible-counts (map (fn [[x y]] [[x y] (calculate-visible [x y] coords)]) coords)]
    (reduce #(assoc-in %1 (reverse (first %2)) (second %2)) grid visible-counts)))

(deftest example
  (is (= (visible-from-grid [[0 1 0 0 1][0 0 0 0 0][1 1 1 1 1][0 0 0 0 1][0 0 0 1 1]])
         [[0 7 0 0 7][0 0 0 0 0][6 7 7 7 5][0 0 0 0 7][0 0 0 8 7]])))

(defn part-1 []
  (let [grid (visible-from-grid input)]
    (reduce #(reduce max %1 %2) 0 grid)))

