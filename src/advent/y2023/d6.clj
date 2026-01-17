(ns advent.y2023.d6
  (:require [clojure.math]))

(def test-input [[7 15 30] [9 40 200]])

(def input [[40     70     98     79]
            [215   1051   2147   1005]])
  
;; r = time of race
;; a = time spent accelerating
;; d = distance travelled
;; d = a * (r - a)
;; 0 = ra - a^2 - d

(defn quadratic [a b c]
  (let [part (clojure.math/sqrt (- (* b b) (* 4 a c)))]
    [(/ (- part b) (* 2 a))
     (/ (- (- part) b) (* 2 a))]))

(defn solve [r d]
  (let [[from to] (quadratic -1 r (- d))]
    [(long (clojure.math/ceil from)) (long (clojure.math/floor to))]))

(defn how-many-ways? [r d]
  (let [[from to] (solve r d)]
    (- to from -1)))

(def input2 [[40709879]
             [215105121471005]])

(defn part-1 [i]
  (reduce * (let [[times distances] i] (mapv how-many-ways? times distances))))
