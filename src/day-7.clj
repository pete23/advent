(ns day-7
  (:use clojure.test)
  (:require [intcode]
            [clojure.math.combinatorics :as combo]))

(def input (intcode/load-file "day-7.input"))

(defn run-program [program signal phase]
  (first (:output (intcode/run-program program (vector phase signal)))))

(defn run-phases [program p]
  (reduce (partial run-program program) 0 p))

(deftest test-first-signal
  (is (= 43210 (run-phases  [3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0] [4 3 2 1 0]))))

(defn find-max-signal [program]
  (apply max (map (partial run-phases program) (combo/permutations [0 1 2 3 4]))))

(deftest test-examples
  (is (= 43210 (find-max-signal [3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0])))
  (is (= 54321 (find-max-signal [3 23 3 24 1002 24 10 24 1002 23 -1 23 101 5 23 23 1 24 23 23 4 23 99 0 0]))))

(defn part-1 []
  (find-max-signal input))

;; create a starting set of machines
(defn machines [program phases]
  (mapv #(intcode/create-machine program (vector %)) phases))

(def test-139629729-from-98765 [3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26 
                                27 4 27 1001 28 -1 28 1005 28 6 99 0 0 5])

;; run the machines through one time
(defn run-one-step [machines input]
  (loop [i machines
         o []
         v input]
    (if (empty? i) o
        (let [m (first i)
              k (intcode/add-input m v)
              l (intcode/run-machine k)]
          (recur (rest i) (conj o l) (last (:output l)))))))
      
;; run the machines through until halted
(defn run-centipede
  ([machines] (run-centipede machines 0))
  ([machines input]
   (loop [m machines i input]
     (let [n (run-one-step m i)
           o (last (:output (last n)))]
       (if (intcode/halted? (last n)) o
           (recur n o))))))

(deftest part-2-examples
  (= 139629729 (run-centipede (machines test-139629729-from-98765 [9 8 7 6 5]))))


(defn part-2 []
  (apply max (map #(run-centipede (machines input %)) (combo/permutations [5 6 7 8 9]))))
