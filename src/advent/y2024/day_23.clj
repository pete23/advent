(ns advent.y2024.day-23
  (:require [dom-top.core :refer [loopr]]))

(def test-input "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn")

(def input (slurp "resources/advent/y2024/day_23.txt"))

(defn parse [input]
  (map #(clojure.string/split % #"-") (clojure.string/split-lines input)))

(defn build-map [raw]
  (reduce (fn [acc [n1 n2]]
            (-> acc
                (update n1 (fnil conj #{}) n2)
                (update n2 (fnil conj #{}) n1)))
          {}
          raw))

(defn part-1 [input]
  (let [network-map (build-map (parse input))
        t-nodes (set (filter #(clojure.string/starts-with? % "t") (keys network-map)))
        t-sets (loopr [three-sets #{}]
                   [a t-nodes
                    b (network-map a)
                    c (network-map b)]
                   (recur (if ((network-map c) a)
                            (conj three-sets #{a b c})
                            three-sets)))]
    t-sets))

(count (part-1 input))
                     
(defn part-2 [input]
  (let [network-map (build-map (parse-input))]))

(count (parse input))

;; set intersection between the pairs
;; creates the pair -> triple map

(defn refine-1 [network-map refining-set]
  (loopr [refined-set #{}]
      [a refining-set
       b (apply clojure.set/intersection (map network-map a))]
      (recur (conj refined-set (conj a b)))))

(defn refine [network-map]
  (loop [refining-set (map (fn [x] #{x}) (keys network-map))]
    (if (<= (count refining-set) 1)
      (first refining-set)
      (recur (refine-1 network-map refining-set)))))

(defn part-2 [input]
  (let [network-map (build-map (parse input))
        biggest-party (refine network-map)]
    (apply str (interpose "," (sort biggest-party)))))

