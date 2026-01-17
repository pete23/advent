(ns advent.y2023.d8
  (:require [clojure.string :refer [split split-lines]]))

(def test-input (slurp "resources/advent/y2023/d8-test.txt"))
(def input (slurp "resources/advent/y2023/d8.txt"))

(defn parse-map-line [acc line]
  (let [[from left right] (split line #"[^A-Z0-9]+")]
    (-> acc
        (update \L assoc from left)
        (update \R assoc from right))))

(defn parse-map [map-str]
  (reduce parse-map-line {} (split-lines map-str)))
                 
(defn parse-input [i]
  (let [[dir-str map-str] (split i #"\n\n")]
    {:directions dir-str
     :map (parse-map map-str)}))

(defn step [map path direction]
  (let [location (last path)
        next (get-in map [direction location])
        path (conj path next)]
    (if (= next "ZZZ")
      (reduced path)
      path)))

(defn part-1 [i]
  (let [{:keys [directions map]} (parse-input i)
        direction-seq (cycle directions)]
    (dec (count (reduce (partial step map) ["AAA"] direction-seq)))))

(defn ends-with? [location char]
  (= (nth location 2) char))

(defn multi-step [the-map [steps locations] direction]
  (let [dir-map (get the-map direction)
        steps (inc steps)
        locations (mapv dir-map locations)]
    (if (every? #(ends-with? % \Z) locations)
      (reduced steps)
      [steps locations])))

(defn part-2 [i]
  (let [{:keys [directions map]} (parse-input i)
        direction-seq (cycle directions)
        starts (filter #(ends-with? % \A) (keys (map \L)))]
    (reduce (partial multi-step map) [0 starts] direction-seq)))
