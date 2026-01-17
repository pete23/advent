(ns advent.y2023.d10
  (:require [clojure.string :refer [split split-lines]]))

(def input (slurp "resources/advent/y2023/d10.txt"))

;; build map of coords to (from, to) coord pairs
;; filter map to remove any connections without reciprocal connection
;; -> left with the loop
;; coordinates Y X

(def n [-1 0])
(def s [1 0])
(def e [0 1])
(def w [0 -1])

(def pipes
  {\| [n s]
   \- [e w]
   \L [n e]
   \J [n w]
   \7 [s w]
   \F [s e]})

(defn add [[y1 x1] [y2 x2]]
  [(+ y1 y2) (+ x1 x2)])

(defn parse-square [position c]
  (cond
    (= c \S) :start
    (pipes c) (into #{} (mapv (partial add position) (pipes c)))))

(defn parse [i]
  (into {}
        (filter identity
                (apply concat
                       (map-indexed
                        (fn [y l]
                          (map-indexed (fn [x c]
                                         (when-let [square (parse-square [y x] c)]
                                           [[y x] square])) l)) (split-lines i))))))

(def test-1 ".....
.S-7.
.|.|.
.L-J.
.....")

(parse test-1)

(def test-2 "-L|F7
7S-7|
L|7||
-L-J|
L|-JF")

(defn connected? [m pos connections]
  (if (= connections :start)
    true
    (every? (fn [connection]
              (when-let [cons-cons (m connection)]
                (or (= cons-cons :start) (cons-cons pos)))) connections)))

(defn filter-loops
  ([m] (let [new-m (reduce (fn [m [pos connections]]
                             (if (connected? m pos connections)
                               m
                               (dissoc m pos))) m m)]
         (if (= new-m m)
           m
           (recur new-m)))))

(defn find-start [m]
  (ffirst (filter #(= (second %) :start) m)))

(defn find-surrounds [m k]
  (filter #((second %) k) m))

(defn loop-away-from
  ([m from k] (loop-away-from [k] m from k))
  ([acc m from k]
   (let [next (m k)]
     (if (= next :start)
       acc
       (let [not-from (first (filter #(not= from %) next))]
         (recur (conj acc not-from) m k not-from))))))
  
