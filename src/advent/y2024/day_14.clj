(ns advent.y2024.day-14
  (:require [clojure.test :refer :all]
            [advent.core :refer [atol]]))

(def test-input "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3")

(def input (slurp "resources/advent/y2024/day_14.txt"))

(defn parse-robot [line]
  (partition 2 (map atol (map second (re-seq #"(-?\d+)" line)))))

(deftest parse-robot-test
  (is (= [[9 5] [-3 -3]]
         (parse-robot "p=9,5 v=-3,-3"))))

(defn parse [input]
  (let [robots (mapv parse-robot (clojure.string/split-lines input))]
    {:positions (mapv first robots)
     :vectors (mapv second robots)}))

(defn step [[limit-x limit-y] {:keys [positions vectors] :as robots}]
  (assoc robots :positions
         (map (fn [[x y] [vx vy]]
                [(mod (+ x vx) limit-x) (mod (+ y vy) limit-y)])
              positions vectors)))

(defn quadrant [[limit-x limit-y] [x y]]
  (let [x-centre (unchecked-divide-int limit-x 2)
        y-centre (unchecked-divide-int limit-y 2)]
    (cond
      (and (< x-centre x) (< y-centre y)) 1
      (and (> x-centre x) (< y-centre y)) 2
      (and (< x-centre x) (> y-centre y)) 3
      (and (> x-centre x) (> y-centre y)) 4)))

(defn part-1 [size input]
  (let [robots (parse input)
        hundredth-step (nth (iterate (partial step size) robots) 100)]
    (reduce * (vals (dissoc (frequencies (map (partial quadrant size) (:positions hundredth-step))) nil)))))

(defn rasterize [[x y] {:keys [positions]}]
  (apply str
         (interpose "\n" (map #(apply str %)
                              (reduce (fn [raster [x y]]
                                        (assoc-in raster [y x] \X))
                                      (vec (repeat y (apply vector (repeat x \.))))
                                      positions)))))
  

(defn distinct-robots? [{:keys [positions]}]
  (= (count (set positions)) (count positions)))

(defn part-2 [size input]
  (let [robots (parse input)]
    (reduce (fn [step robots]
              (if (distinct-robots? robots)
                (do (println step)
                    (println (rasterize size robots))
                    (inc step))
                (inc step))) 0 (iterate (partial step size) robots))))

