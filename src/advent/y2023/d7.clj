(ns advent.y2023.d7
  (:require [clojure.string :refer [split split-lines]]))

(def test-input (split-lines (slurp "resources/advent/y2023/d7-test.txt")))
(def input (split-lines (slurp "resources/advent/y2023/d7.txt")))

(def cards
  {\K 13 \Q 12 \J 11 \T 10
   \9 9 \8 8 \7 7 \6 6 \5 5 \4 4 \3 3 \2 2
   \A 14})

(def joker-cards (assoc cards \J 1))

(defn atol [a]
  (Long/parseLong a))

(defn compare-seqs [[a & as] [b & bs]]
  (let [c (compare a b)]
    (cond (not= c 0) c
          (= a b nil) 0
          :else (recur as bs))))

(defn score [hand]
  ;; scoring with the sorted frequencies allows us to just compare sequences
  (sort > (map second (frequencies hand))))

(defn score-with-jokers [hand]
  ;; when we create the score seq, add the number of jokers to the most frequent
  (let [freqs (frequencies hand)
        jokers (or (get freqs 1) 0)
        ;; handle five of a kind JJJJJ
        freqs-no-jokers (if (< jokers 5) (dissoc freqs 1) {1 0})]
    (update (into [] (sort > (map second freqs))) 0 + jokers)))

(defn parse-line [cards l]
  (let [[hand-str bid-str] (split l #" ")]
    {:hand (mapv cards hand-str)
     :bid (atol bid-str)}))

(defn create-sort-key [freqs x]
  (assoc x :sort (concat (freqs (:hand x)) (:hand x))))

(defn order-hands [i score-fn cards]
  (->> i
       (map (partial parse-line cards))
       (map (partial create-sort-key score-fn))
       (sort-by :sort compare-seqs)))

(defn solve [i score-fn vals]
  (reduce + (map * (map :bid (order-hands i score-fn vals)) (range 1 10000))))

(defn part-1 [i]
  (solve i score cards))
              
(defn part-2 [i]
  (solve i score-with-jokers joker-cards))
