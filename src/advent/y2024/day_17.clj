(ns advent.y2024.day-17
  (:require [clojure.math]
            [clojure.test :refer :all]
            [dom-top.core :refer [loopr]]))

(def input
  {:a 25358015
   :b 0
   :c 0
   :program [2,4,1,1,7,5,0,3,4,7,1,6,5,5,3,0]})

(run-machine input)

(reduce (fn [acc nxt]
          (bit-or (bit-shift-left acc 3)
                  (bit-xor nxt 7 (bit-and acc 7))))
        0
        (reverse [2,4,1,1,7,5,0,3,4,7,1,6,5,5,3,0]))

;; 192830012331175
;; 260262795108776
;; 265134834257309
;; 7 5 / set c to shift a right by b bits
;; 2 4 / bst 4 / low bits of a into b
;; 1 1 / bxl 1 / xor b by 1
;; 0 3 / shift a right by 3 bits
;;     / xor b by c
;; 1 6 / xor b by 6
;; 5 5 / output low 3 bits of b
;; 3 0 / jump if not zero to start

(defn one-digit [a b]
  (loopr
      [a 0]
      [b (reverse [2,4,1,1,7,5,0,3,4,7,1,6,5,5,3,0])]
      (let [b (bit-xor b 7 c)
            a (bit-or (bit-shift-left a 3) b)
            c (bit-shift-right a 3)]
        (recur a c))
    a))
    
    
(def test-input
  {:a 729
   :b 0
   :c 0
   :program [0,1,5,4,3,0]})

(defn initialise-machine [machine]
  (assoc machine :ip 0 :output []))

(defn combo [{:keys [a b c]} operand]
  (cond (< operand 4) operand
        (= operand 4) a
        (= operand 5) b
        (= operand 6) c
        (= operand 7) nil)) ;; reserved

(defn bxl [{:keys [b] :as machine} operand]
  (assoc machine :b (bit-xor b operand)))

(defn bst [machine operand]
  (let [operand (combo machine operand)]
    (assoc machine :b (bit-and operand 7))))

(defn jnz [{:keys [a] :as machine} operand]
  (if (= a 0)
    machine
    (assoc machine :ip (- operand 2))))

(defn bxc [{:keys [c] :as machine} _]
  (update machine :b bit-xor c))

(defn out [machine operand]
  (let [operand (bit-and (combo machine operand) 7)]
    (update machine :output conj operand)))

(defn dv-fn [target {:keys [a] :as machine} operand]
  (let [operand (combo machine operand)]
    (assoc machine target (bit-shift-right a operand))))

(defn machine-step [{:keys [program ip] :as machine}]
  (let [instruction (nth program ip)
        operand (nth program (inc ip))
        step (case instruction
               0 (dv-fn :a machine operand)
               1 (bxl machine operand)
               2 (bst machine operand)
               3 (jnz machine operand)
               4 (bxc machine operand)
               5 (out machine operand)
               6 (dv-fn :b machine operand)
               7 (dv-fn :c machine operand))]
    (update step :ip + 2)))

(defn run-machine [machine]
  (let [run-length (count (:program machine))]
    (loop [machine (initialise-machine machine)]
      (println machine)
      (if (>= (:ip machine) run-length)
        machine
        (recur (machine-step machine))))))

(deftest testes
  (is (= 1 (:b (run-machine {:a 0 :b 0 :c 9 :ip 0 :program [2 6]}))))
  (is (= [0 1 2] (:output (run-machine {:a 10 :program [5 0 5 1 5 4]}))))
  (let [machine (run-machine {:a 2024 :program [0 1 5 4 3 0]})]
    (is (= [4 2 5 6 7 7 7 7 3 1 0] (:output machine)))
    (is (= 0 (:a machine))))
  (is (= [4 2 5 6 7 7 7 7 3 1 0] (:output (run-machine {:a 2024 :program [0 1 5 4 3 0]}))))
  (is (= 26 (:b (run-machine {:b 29 :program [1 7]}))))
  (is (= 44354 (:b (run-machine {:b 2024 :c 43690 :program [4 0]})))))

(run-machine input)
