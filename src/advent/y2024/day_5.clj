(ns advent.y2024.day-5
  (:require [advent.core :refer [atol]]))

(def test-input "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(def input (slurp "resources/advent/y2024/day_5.txt"))

(defn parse-input [i]
  (let [[rules books] (mapv clojure.string/split-lines (clojure.string/split i #"\n\n"))]
    {:rules (map #(map atol (clojure.string/split % #"\|")) rules)
     :books (map #(map atol (clojure.string/split % #",")) books)}))

(defn build-rule-sets [{:keys [rules]}]
  (update-vals (reduce (fn [acc rule] (update acc (second rule) conj (first rule)))
        {}
        rules) set))

(defn test-book [rule-sets book]
  (reduce (fn [acc page]
            (if (acc page)
              (reduced nil)
              (clojure.set/union acc (rule-sets page))))
          #{}
          book))

(defn middle [book]
  (let [c (count book)]
    (nth book (/ c 2))))

(defn part-1 [input]
  (let [i (parse-input input)
        r (build-rule-sets i)
        b (:books i)]
    (reduce + (map middle (filter (partial test-book r) b)))))

(defn comparator [rule-set]
  (fn [a b]
    (if ((or (rule-set a) #{}) b) 1 -1)))

(defn part-2 [input]
  (let [i (parse-input input)
        r (build-rule-sets i)
        b (remove (partial test-book r) (:books i))
        c (comparator r)
        b (map #(sort-by identity c %) b)]
    (reduce + (map middle b))))
