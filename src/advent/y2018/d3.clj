(ns advent.y2018.d3
  (:require [clojure.test :refer :all]))

(def input (-> "y2018/day-3.input"
               clojure.java.io/resource
               slurp
               (clojure.string/split #"\n")))

(def test-input ["#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"])

(defn atol [a] (Long/parseLong a))

(defn parse [input-string]
  (map atol (filter not-empty (clojure.string/split input-string #"[# @,:x]"))))

(defn make-empty-set-square-vector [size]
  (let [row (vec (repeat size #{}))]
    (vec (repeat size row))))

(defn claimed-coordinates [x y w h]
  (for [x (range x (+ x w))
        y (range y (+ y h))]
    [x y]))

(defn claim
  ([fabric [id x y w h]]
   (reduce #(claim %1 id %2) fabric (claimed-coordinates x y w h)))
  ([fabric id coords] (update-in fabric coords conj id)))

(defn contested-claims [input]
  (filter #(> (count %) 1)
          (flatten
           (reduce claim (make-empty-set-square-vector 1000) (map parse input)))))

(deftest part-one-test
  (is (= 4 (count (contested-claims test-input)))))

(defn part-one []
  (count (contested-claims input)))

(defn uncontested-claim-ids [input]
  (let [all-claim-ids (apply hash-set (map first (map parse input)))]
    (apply clojure.set/difference all-claim-ids (contested-claims input))))

(deftest part-two-test
  (is (= #{3} (uncontested-claim-ids test-input))))

(defn part-two []
  (uncontested-claim-ids input))
