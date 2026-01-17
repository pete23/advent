(ns advent.y2024.day-22
  (:require [clojure.test :refer :all]
            [advent.core :refer [atol]]
            [dom-top.core :refer [loopr]]))

(defn prune [^long l]
  (bit-and 16777215 l))

(defn mix [^long x ^long y]
  (bit-xor x y))

(deftest verify-bitrithmetic
  (is (= (bit-shift-left 1000 6) (* 64 1000)))
  (is (= (bit-shift-right 1000 5) (long (/ 1000 32))))
  (is (= (bit-shift-left 1000 11) (* 1000 2048))))

(defn step [^long l]
  (let [l (prune (mix l (bit-shift-left l 6)))
        l (prune (mix l (bit-shift-right l 5)))]
    (prune (mix l (bit-shift-left l 11)))))

(defn twothousandth [start]
  (nth (iterate step start) 2000))

(deftest stepper
  (is (= [123 15887950 16495136 527345 704524 1553684 12683156 11100544 12249484 7753432 5908254]
         (take 11 (iterate step 123))))
  (is (= (map twothousandth [1 10 100 2024])
         '(8685429 4700978 15273692 8667524))))

(defn parse [input]
  (mapv atol (clojure.string/split-lines input)))

(def input (slurp "resources/advent/y2024/day_22.txt"))

(defn part-1 [input]
  (reduce + (map twothousandth (parse input))))

(part-1 input)
  
(def monkeys (parse input))

(defn monkey-prices [monkey]
  (mapv #(mod % 10) (take 2001 (iterate step monkey))))

(defn prices->sequence-payoff-map [[p0 p1 p2 p3 & prices]]
  (loopr [prev-price p3
          curr-seq [nil (- p1 p0) (- p2 p1) (- p3 p2)]
          seq->payoff {}
          n 4]
      [price prices]
      (let [curr-seq (subvec (conj curr-seq (- price prev-price)) 1)
            seq->payoff (if (seq->payoff curr-seq)
                          seq->payoff
                          (assoc seq->payoff curr-seq price))]
        (recur price curr-seq seq->payoff (inc n)))
    seq->payoff))
    
(defn part-2 [input]
  (let [monkeys (parse input)
        monkey-prices (mapv monkey-prices monkeys)
        sequence->payoffs (mapv prices->sequence-payoff-map monkey-prices)]
    (->> sequence->payoffs
         (apply merge-with +)
         vals
         (apply max))))
       
(deftest part-2-example
  (is (= (part-2 "1\n2\n3\n2024") 23)))

(time (part-2 input))
        
        

