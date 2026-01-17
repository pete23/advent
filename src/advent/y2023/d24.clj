(ns advent.y2024.d24
  (:require [clojure.string :refer [split-lines split]]))

(defn atol [a]
  (Long/parseLong a))

(def test-input (slurp "resources/y2023/d24-test.txt"))

(defn intersect?
  "Checks if two points moving through 2D space will intersect."
  [point1 point2]
  (let [[[x1 y1] [vx1 vy1]] point1
        [[x2 y2] [vx2 vy2]] point2
        dx (- x2 x1)
        dy (- y2 y1)
        dvx (- vx2 vx1)
        dvy (- vy2 vy1)]
    (if (and (zero? dvx) (zero? dvy))
      (and (zero? dx) (zero? dy))
      (let [t (- (/ dx dvx))]
        (and (= dx (* dvx t))
             (= dy (* dvy t)))))))
