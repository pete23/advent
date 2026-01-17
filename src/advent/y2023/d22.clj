(ns advent.y2023.d22
  (:require [clojure.string :refer [split-lines split]]))

(def test-input "1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9")

(defn add-coords [[a b c] [x y z]]
  [(+ a x) (+ b y) (+ c z)])

(defn sub-coords [[a b c] [x y z]]
  [(- a x) (- b y) (- c z)])

(defn unit [a]
  (cond
    (= 0 a) 0
    (> 0 a) -1
    (< 0 a) 1))

(defn unit-coords [[a b c]]
  [(unit a) (unit b) (unit c)])

(defn atol [s]
  (Long/parseLong s))

(defn parse-coords [s]
  (mapv atol(split s #",")))
  
(defn parse-block [s]
  (mapv parse-coords (split s #"~")))

(defn parse-input [s]
  (map parse-block (split-lines s)))
