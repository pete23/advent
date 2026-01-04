(ns advent.y2024.day-19
  (:require [clojure.test :refer :all]))

(def test-input "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb")

(def input (slurp "resources/advent/y2024/day_19.txt"))

(defn parse [input]
  (let [[towel-str pattern-str] (clojure.string/split input #"\n\n")]
    {:towels (clojure.string/split towel-str #", ")
     :patterns (clojure.string/split-lines pattern-str)}))

(defn match-1 [towel pattern]
  (if (clojure.string/starts-with? pattern towel)
    (subs pattern (.length towel))
    nil))

(defn match [towels pattern]
  (loop [patterns [pattern]]
    (let [next-patterns (set (filter identity
                                     (mapcat #(map (partial match-1 %) patterns) towels)))]
      (cond
        (empty? next-patterns) nil
        (some String/.isEmpty next-patterns) pattern
        :else (recur next-patterns)))))

(defn part-1 [input]
  (let [{:keys [towels patterns]} (parse input)]
    (count (filter identity (map (partial match towels) patterns)))))

(deftest part-1-test
  (is (= 6 (part-1 test-input))))

(defn count-matches [towels patterns]
  (loop [patterns (reduce (fn [acc p] (assoc acc p 1)) {} patterns)
         matches 0]
    (let [next-patterns (reduce (fn [acc [^String pattern paths]]
                                  (reduce (fn [acc ^String towel]
                                            (if (clojure.string/starts-with? pattern towel)
                                              (update acc (subs pattern (.length towel)) (fnil + 0) paths)
                                              acc))
                                           acc
                                           towels))
                                {}
                                patterns)
          matches (+ matches (or (next-patterns "") 0))
          next-patterns (dissoc next-patterns "")]
      (if (empty? next-patterns) matches
          (recur next-patterns matches)))))

(defn part-2 [input]
  (let [{:keys [towels patterns]} (parse input)]
    (count-matches towels patterns)))

(deftest part-2-test
  (is (= 16 (part-2 test-input))))
      
