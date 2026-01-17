(ns advent.y2024.day-20
  (:require [advent.core :refer [parse-grid cat add find-char manhattan-distance]]
            [clojure.test :refer :all]))

(def test-input "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############")

(defn routes [grid from]
  (let [directions [[-1 0] [1 0] [0 -1] [0 1]]
        possibilities (map (partial add from) directions)
        valid-square? #(#{\E \. \S} (cat grid %))]
    (filter valid-square? possibilities)))
    
(defn path [input]
  (let [grid (parse-grid input)
        start (first (find-char grid \S))
        end (first (find-char grid \E))]
    (loop [prev nil
           this start
           path []]
      (let [path (conj path this)
            [next] (filter #(not= prev %) (routes grid this))]
        (if (nil? next)
          path
          (recur this next path))))))

(defn path->distances [path]
  (into {} (map-indexed #(vector %2 %1) (reverse path))))

(def cheat-moves [[-2 0] [0 -2] [-1 -1] [1 1] [-1 1] [1 -1] [2 0] [0 2]])

(defn build-cheat [p->d from cheat-move]
  (let [to (add from cheat-move)
        distance (p->d from)]
    (when-let [new-distance (p->d to)]
      ;; need to take off 2 because the cheat costs 2 moves
      (let [improvement (- distance new-distance 2)]
        (if (> improvement 0)
          [from to improvement])))))

(defn build-cheats [p->d]
  (reduce (fn [acc [from dist]]
            (reduce (fn [acc cheat-move]
                      (if-let [cheat (build-cheat p->d from cheat-move)]
                        (conj acc cheat)
                        acc)) acc cheat-moves))
            []
            p->d))

(def part-1-model-answer {2 14 4 14 6 2 8 4 10 2 12 3 20 1 36 1 38 1 40 1 64 1})

(deftest part-1-test
  (is (= part-1-model-answer
         (frequencies (map #(nth % 2) (build-cheats (path->distances (path test-input))))))))

(defn part-1 [input]
  (->> input
       path
       path->distances
       build-cheats
       (filter #(>= (nth % 2) 100))
       count))

(def input (slurp "resources/advent/y2024/day_20.txt"))

(part-1 input)


(defn cross-product-cheats [max-distance p->d]
  (reduce (fn [acc [from f-path-dist]]
            (reduce (fn [acc [to t-path-dist]]
                      (let [m-dist (manhattan-distance from to)
                            path-dist-improvement (- f-path-dist t-path-dist m-dist)]
                        (if (and (<= m-dist max-distance)
                                 (> path-dist-improvement 0))
                          (update acc path-dist-improvement (fnil inc 0))
                          acc)))
                    acc p->d))
            {}
            p->d))

(defn part-2
  ([input]
   (part-2 20 input))
  ([max-distance input]
   (->> input
        path
        path->distances
        (cross-product-cheats-revised max-distance))))

(part-2 2 test-input)

(def part-2-model-answer {50 32 52 31 54 29 56 39 58 25 60 23 62 20 64 19 66 12 68 14 70 12 72 22 74 4 76 3})

(deftest part-2-test
  (is (= part-1-model-answer (part-2 2 test-input)))
  (is (= part-2-model-answer (apply dissoc (part-2 test-input) (range 50)))))

(defn real-part-2 [input]
  (let [part-2 (apply dissoc (part-2 input) (range 100))]
    (reduce + (vals part-2))))

(defn cross-product-cheats-revised [max-distance p->d]
  (reduce (fn [acc [from f-path-dist]]
            (reduce (fn [acc [to t-path-dist]]
                      (let [m-dist (manhattan-distance from to)
                            path-dist-improvement (- f-path-dist t-path-dist m-dist)]
                        (if (and (<= m-dist max-distance)
                                 (>= path-dist-improvement 100))
                          (inc acc)
                          acc)))
                    acc p->d))
            0
            p->d))
