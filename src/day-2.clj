(ns day-2
  (:use clojure.test))

(defn atol [s] (Long/parseLong s))

(def input (mapv atol
                 (-> "day-2.input"
                     clojure.java.io/resource
                     slurp
                     (clojure.string/split #","))))

(defn run-step [[opcode src1-loc src2-loc dst-loc] program]
  (let [operation (case opcode 1 + 2 *)
        src1 (nth program src1-loc)
        src2 (nth program src2-loc)
        result (operation src1 src2)]
    (assoc program dst-loc result)))

(defn run-program
  ([program] (run-program 0 program))
  ([pc program] (if (= 99 (nth program pc))
                  program
                  (let [next-pc (+ 4 pc)
                        instruction (subvec program pc next-pc)]
                    (recur next-pc (run-step instruction program))))))

(deftest test-interpreter
  (is (= (run-program [1 0 0 0 99]) [2 0 0 0 99]))
  (is (= (run-program [2 3 0 3 99]) [2 3 0 6 99]))
  (is (= (run-program [2 4 4 5 99 0]) [2 4 4 5 99 9801]))
  (is (= (run-program [1 1 1 4 99 5 6 0 99]) [30 1 1 4 2 5 6 0 99])))

(defn try-program [noun verb]
  (first
   (run-program
    (assoc input 1 noun 2 verb))))

(defn part-1 []
  (try-program 12 2))

(defn check-noun-verb [[noun verb]]
  (if (= (try-program noun verb) 19690720) [noun verb]))

(defn part-2 []
  (some check-noun-verb (for [noun (range 0 99)
                              verb (range 0 99)]
                          [noun verb])))
