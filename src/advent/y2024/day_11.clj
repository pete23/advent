(ns advent.y2024.day-11
  (:require [advent.core :refer [atol]]))

(def input [77 515 6779622 6 91370 959685 0 9861])

(def test-input [0 1 10 99 999])

(defn odd? [^long stone]
  (= 1 (bit-and stone 1)))

(defn even? [^long stone]
  (not (odd? stone)))

(defn blink-1 [stone]
  (let [stonestr (str stone)
        stonelen (.length stonestr)
        half (unchecked-divide-int stonelen 2)]
    (cond
      (= 0 stone) [1]
      (even? stonelen)
      [(atol (subs stonestr 0 half))
       (atol (subs stonestr half))]
      :else [(* stone 2024)])))

(defn blink [stones]
  (flatten (map blink-1 stones)))

(defn part-1 [stones]
  (count (nth (iterate blink stones) 25)))

(defn bucket-blink [stones]
  (reduce (fn [acc [stone freq]]
            (reduce (fn [acc stone]
                      (update acc stone (fnil + 0) freq)) acc (blink-1 stone))) {} stones))

(defn part-2 [stones]
  (reduce + (vals (nth (iterate bucket-blink (frequencies stones)) 25))))

(time (part-1 input))
               
