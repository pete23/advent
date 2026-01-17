(ns advent.y2023.d4
  (:require [clojure.string :refer [split split-lines]]
            [clojure.set :refer [intersection]]))

(def test-input (split-lines (slurp "resources/advent/y2023/d3-test.txt")))
(def input (split-lines (slurp "resources/advent/y2023/d3.txt")))

(defn atol [a]
  (Long/parseLong a))

(defn parse-numbers [ns]
  (into #{} (map atol (re-seq #"[0-9]+" ns))))

(defn parse-line [l]
  (let [[game winners numbers] (split l #"[:|]")]
        {:game (atol (re-find #"[0-9]+" game))
         :winners (parse-numbers winners)
         :numbers (parse-numbers numbers)}))

(defn score-1 [{:keys [winners numbers] :as game}]
  (let [matches (intersection winners numbers)
        n-matches (count matches)]
    (assoc game
           :n-matches n-matches
           :score (if (= n-matches 0)
                    0
                    (bit-shift-left 1 (dec n-matches))))))

  
(defn part-1 [input]
  (reduce + (map (comp :score score-1 parse-line) input)))

(defn init-cards [g]
  (assoc g :cards 1))

(defn adjust-cards [games from n cards]
  (cond (= n 0) games
        (= n (count games)) games
        :true (recur (update-in games [from :cards] + cards) (inc from) (dec n) cards)))

(defn score-2 [games]
  (reduce (fn [games {:keys [game n-matches]}]
            (let [cards (:cards (games (dec game)))]
              (adjust-cards games game n-matches cards))) games games))

(defn part-2 [input]
  (let [games (score-2 (mapv (comp init-cards score-1 parse-line) input))]
    (reduce + (map :cards games))))
