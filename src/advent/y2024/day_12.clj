(ns advent.y2024.day-12
  (:require [advent.core :refer [parse-grid squares cat add]]))

(def small-test-grid (parse-grid "AAAA
BBCD
BBCC
EEEC"))

(def weird-test-grid (parse-grid "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO"))

(def larger-test-grid (parse-grid "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"))

(def input (parse-grid (slurp "resources/advent/y2024/day_12.txt")))

(defn up [[x y]] [x (dec y)])

(defn left [[x y]] [(dec x) y])

(defn grid->regions [grid]
  (dissoc
   (reduce (fn [square->region square]
             (let [c (cat grid square)
                   left (left square)
                   up (up square)
                   left-c (cat grid left)
                   up-c (cat grid up)
                   left-r (square->region left)
                   up-r (square->region up)]
               (cond
                 ;; may need to merge two regions
                 (and (= c left-c) (= c up-c) (not= left-r up-r))
                 (update-vals (assoc square->region square left-r) #(if (= % up-r) left-r %))
                       
                 (= c left-c)
                 (assoc square->region square left-r)
                 
                 (= c up-c)
                 (assoc square->region square up-r)
                 
                 :new-region 
                 (update (assoc square->region square (:next-id square->region)) :next-id inc))))
           {:next-id 0} (squares grid))
   :next-id))

(defn invert-map-to-set [m]
  (update-vals (reduce-kv (fn [acc k v]
                            (update acc v conj k)) {} m)
               set))

(defn around [[x y]]
  [[(dec x) y] [(inc x) y]
   [x (dec y)] [x (inc y)]])

(defn count-neighbours [squares square]
  (count (filter squares (around square))))

(defn perimeter [squares square]
  (let [neighbours (count-neighbours squares square)]
    (- 4 neighbours)))
     
(defn total-perimeter [region]
  (reduce #(+ % (perimeter region %2)) 0 region))
  
(defn price [region]
  (* (total-perimeter region) (count region)))

  
(defn price-grid [grid pricer]
  (->> grid
       grid->regions
       invert-map-to-set
       vals
       (map pricer)
       (reduce +)))
      
(defn part-1 []
  (price-grid input price))

(def turn-right {[0 1] [-1 0]
                 [1 0] [0 1]
                 [0 -1] [1 0]
                 [-1 0] [0 -1]})

(def turn-left {[0 1] [1 0]
                [1 0] [0 -1]
                [0 -1] [-1 0]
                [-1 0] [0 1]})
                  

(defn follow-edge [valid? edge]
  (loop [edge edge
         explored #{}]
    (let [explored (conj explored edge)
          [pos normal] edge
          next-pos (add pos (turn-left normal))
          right-hand-path (add normal next-pos)]
      (cond
        (not (valid? next-pos))
        [[pos (turn-left normal)] explored]
        
        (valid? right-hand-path)
        [[right-hand-path (turn-right normal)] explored]
        
        :else
        (recur next-pos explored)))))

(def directions [[0 1] [1 0] [0 -1] [-1 0]])

(defn edges [squares]
  (into (sorted-set)
        (for [square squares
              direction directions
              :when (not (squares (add square direction)))]
          [square direction])))

(defn count-sides [squares]
  (let [edges (edges squares)
        initial-edge (first edges)]
    (loop [edge initial-edge
           edges edges
           sides 1]
      (let [[next-edge visited] (follow-edge squares edge)
            edges (clojure.set/difference edges visited)]
        (println edge next-edge)
        (cond
          (and (= initial-edge edge) (empty? edges))
          sides
          (and (= initial-edge edge))
          (recur (first edges) edges (inc sides))
          :else
          (recur next-edge edges (inc sides)))))))


(defn price-2 [squares]
  (* (count-sides squares) (count squares)))

(def e-test (parse-grid "EEEEE
EXXXX
EEEEE
EXXXX
EEEEE"))

(def this-test (parse-grid "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA"))
