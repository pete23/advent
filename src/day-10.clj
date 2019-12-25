(ns day-10
  (:use clojure.test))

(defn line->vec [line]
  (mapv { \. 0 \# 1 \X 1} line))

(def input (->> "day-10.input"
                clojure.java.io/resource
                slurp
                clojure.string/split-lines
                (mapv line->vec)))

;; take all stars and make relative to current star
;; calculate angles of stars
;; set
;; count

(defn cartesian->both [[x y v]]
  {:r (Math/sqrt (+ (* x x) (* y y)))
   :theta (Math/atan2 y x)
   :x x
   :y y})

(defn grid->coords [grid]
  (for [x (range (count (first grid)))
        y (range (count grid))
        :let [v ((grid y) x)]
        :when (> v 0)]
    [x y v]))

(defn calculate-coordinates-relative-to [[x y] coords]
  (let [relative (map (fn [[x1 y1]] [(- x1 x) (- y1 y)]) coords)]
    (filter #(> (:r %) 0) (map cartesian->both relative))))

(defn calculate-visible [[x y] coords]
  (count (set (map :theta (calculate-coordinates-relative-to [x y] coords)))))

(defn visible-from-grid [grid]
  (let [coords (grid->coords grid)
        visible-counts (map (fn [[x y]] [[x y] (calculate-visible [x y] coords)]) coords)]
    (reduce #(assoc-in %1 (reverse (first %2)) (second %2)) grid visible-counts)))

(deftest example
  (is (= (visible-from-grid
          [[0 1 0 0 1][0 0 0 0 0][1 1 1 1 1][0 0 0 0 1][0 0 0 1 1]])
         [[0 7 0 0 7][0 0 0 0 0][6 7 7 7 5][0 0 0 0 7][0 0 0 8 7]])))


(defn part-1 []
  (let [coords (grid->coords (visible-from-grid input))]
    (apply max-key last coords)))

(defn explodonate-closest [[exploded targets] aim]
  (let [[boom & surviving] (sort-by :r (targets aim))
        exploded (conj exploded boom)]
    (if (nil? surviving)
      (vector exploded (dissoc targets aim))
      (vector exploded (assoc targets aim surviving)))))

(deftest explodonator
  (is (= [[{:r 0}] {1 '({:r 1})}] (explodonate-closest [[] {1 '({:r 1}{:r 0})}] 1))))

;;; PART TWO WIP

(defn list-of-ordered-targets [[x y] grid]
   (let [coords (grid->coords grid)
         targets (into (sorted-map)
                       (group-by :theta (calculate-coordinates-relative-to [x y] coords)))
         theta-of-first-target (:theta (cartesian->both [0 -1])) ;; 12 o'clock
         first-target-batch (drop-while #(< % theta-of-first-target) (keys targets))]
     (loop [acc []
            target-batch first-target-batch
            targets targets]
       (let [[new-acc new-targets] (reduce explodonate-closest (vector acc targets) target-batch)]
         (if (empty? new-targets) new-acc
             (recur new-acc (keys new-targets) new-targets))))))

(def test-input (mapv line->vec [".#....#####...#.."
                                 "##...##.#####..##"
                                 "##...#...#.#####."
                                 "..#.....X...###.."
                                 "..#.#.....#....##"]))

(defn part-2 []
  (nth (list-of-ordered-targets [17 23] input) 199))

;; uhm then add the 17 and 23 back on and stuff
