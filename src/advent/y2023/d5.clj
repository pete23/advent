(ns advent.y2023.d5
  (:require [clojure.string :refer [split split-lines]]
            [clojure.set :refer [intersection]]))

(def test-input (slurp "resources/advent/y2023/d5-test.txt"))
(def input (slurp "resources/advent/y2023/d5.txt"))

(defn atol [a]
  (Long/parseLong a))

(defn parse-numbers [ns]
  (mapv atol (re-seq #"[0-9]+" ns)))

(defn try-map [n [to from len]]
  (when (<= from n (+ from len -1))
    ;(println blurb n "-" (+ n (- to from)) from to len)
    (+ n (- to from))))

(defn apply-map [n the-map]
  (or (some (partial try-map n) the-map) n))

(defn parse-map [map-text]
  (partition 3 (parse-numbers map-text)))
  
(defn parse-input [i]
  (let [[seed-text & map-texts] (split i #"\n\n")
        maps (mapv parse-map map-texts)]
    {:seeds (parse-numbers seed-text)
     :maps maps}))

(defn part-1 [i]
  (let [{:keys [seeds maps]} (parse-input i)]
    (apply min (map #(reduce apply-map % maps) seeds))))


(defn overlap [[x1 l1] [x2 l2]]
  (let [y1 (+ x1 l1 -1)
        y2 (+ x2 l2 -1)
        xo (max x1 x2)
        yo (min y1 y2)]
    (when (<= xo yo)
      [xo (- yo xo -1)])))

(defn map-range [[dest source len] to-map]
  (when-let [overlap (overlap to-map [source len])]
    (update overlap 0 + (- dest source))))

(defn left-remainder [[x1 l1] [x2 _]]
    (when (< x1 x2)
      [x1 (min l1 (- x2 x1))]))

(defn right-remainder [[x1 l1] [x2 l2]]
  (let [y1 (+ x1 l1 -1)
        y2 (+ x2 l2 -1)]
    (when (< y2 y1)
      [(max x1 y2) (min l1 (- y1 y2))])))

(defn remainders [[dest & source] range]
  [(left-remainder range source) (right-remainder range source)])

(defn try-range [[mapped unmapped] mapper]
  (let [mapped (into mapped (filter identity (map (partial map-range mapper) unmapped)))
        unmapped (filter identity (mapcat (partial remainders mapper) unmapped))]
    [mapped unmapped]))
  
;; ranges x map -> mapped ranges, remaining ranges
(defn apply-range-map [ranges the-map]
  (apply into (reduce try-range [[] ranges] the-map)))

(defn part-2 [i]
  (let [{:keys [seeds maps]} (parse-input i)
        seed-ranges (partition 2 seeds)]
    (apply min (map first (reduce apply-range-map seed-ranges maps)))))

