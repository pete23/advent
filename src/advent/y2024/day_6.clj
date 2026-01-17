(ns advent.y2024.day-6
  (:require [advent.y2024.d4 :refer [cat find-char add]]))

(def test-input "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(def input (slurp "resources/advent/y2024/day_6.txt"))

(defn parse2 [s]
  (let [g (mapv vec (clojure.string/split-lines s))
        x (count (first g))
        y (count g)]
    {:grid g :xs x :ys y}))

(def directions (cycle [[0 -1] [1 0] [0 1] [-1 0]]))

(defn path-to-exit
  ([g start] (path-to-exit g (list start) directions))
  ([g path directions]
   (let [new-loc (add (first path) (first directions))
         c (cat g new-loc)]
     (if (nil? c) path
         (recur g
                (if (or (= c \^) (= c \.))
                  (conj path new-loc) path)
                (if (= c \#)
                  (rest directions) directions))))))

(defn part-1 [input]
  (let [grid (parse2 input)]
    (count (set (path-to-exit grid (first (find-char grid \^)))))))
     ;; if . then add to path continue
     ;; if # then rest directions
;; if nil then no recur

(defn path-to-exit-with-loop-detection
  ([g start] (path-to-exit-with-loop-detection g (list start) #{} directions))
  ([g path loop-check? directions]
   (let [loc (first path)
         direction (first directions)]
     (if (loop-check? [loc direction])
       :loop
       (let [new-loc (add loc direction)
             c (cat g new-loc)]
         (if (nil? c) path
             (recur g
                    (if (or (= c \^) (= c \.))
                      (conj path new-loc) path)
                    (conj loop-check? [loc direction])
                    (if (= c \#)
                      (rest directions) directions))))))))

(defn place-obstacle [g [x y]]
  (assoc-in g [:grid y x] \#))

(defn part-2 [input]
  (let [ur-grid (parse2 input)
        start (first (find-char ur-grid \^))
        path (set (path-to-exit ur-grid start))]
    (filter
     identity
     (map #(if (= :loop (path-to-exit-with-loop-detection %2 start)) %1)
          path
          (map (partial place-obstacle ur-grid) path)))))

(count (part-2 input))

