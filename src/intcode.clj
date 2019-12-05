(ns intcode
  (:use clojure.test))

(defprotocol Machine
  (decode-instruction [_])
  (run-step [_])
  (run-machine [_]))

(defn flags [number]
  (loop [number (quot number 100)
         flags [:opcode]]
    (if (= number 0)
      flags
      (recur (quot number 10)
             (conj flags (if (= 1 (rem number 10)) :immediate))))))
        
(defn input-loader [instruction program]
  (let [flags (flags (first instruction))]
    (fn [position]
      (let [value (nth instruction position)]
        (if (and (< position (count flags)) (= :immediate (nth flags position)))
          value
          (nth program value))))))

(def opcodes {1 :add 2 :mul 3 :input 4 :output})

(defrecord IntcodeMachine [pc program input output]
  Machine

  (decode-instruction [machine]
    (let [instruction (subvec program pc)
          opcode (opcodes (rem (first instruction) 100))
          in (input-loader instruction program)
          out #(nth instruction %)]
      (case opcode
        (:add :mul) [opcode (in 1) (in 2) (out 3)]
        :input [opcode (out 1)]
        :output [opcode (in 1)])))
      
  (run-step [machine]
    (let [instruction (decode-instruction machine)
          opcode (first instruction)]
      (case opcode
        (:add :mul) (let [[_ in1 in2 out] instruction
                          operation (case opcode :add + :mul *)]
                      (IntcodeMachine. (+ 4 pc)
                                       (assoc program out (operation in1 in2))
                                       input
                                       output))
        ;; 3 input
        :input (let [[_ out] instruction]
                 (IntcodeMachine. (+ 2 pc)
                                  (assoc program out (first input))
                                  (rest input)
                                  output))
        ;; 4 output
        :output (let [[_ in] instruction]
                  (IntcodeMachine. (+ 2 pc)
                                   program
                                   input
                                   (conj output in))))))
  (run-machine [machine]
    (loop [m machine]
      (if (= 99 (nth (:program m) (:pc m)))
        m
        (recur (run-step m))))))
        
(defn run-program
  ([program] (:program (run-machine (IntcodeMachine. 0 program [] []))))
  ([program input] (run-machine (IntcodeMachine. 0 program input []))))

(deftest test-interpreter-addition-multiplication-exit
  (is (= (run-program [1 0 0 0 99]) [2 0 0 0 99]))
  (is (= (run-program [2 3 0 3 99]) [2 3 0 6 99]))
  (is (= (run-program [2 4 4 5 99 0]) [2 4 4 5 99 9801]))
  (is (= (run-program [1 1 1 4 99 5 6 0 99]) [30 1 1 4 2 5 6 0 99])))

(deftest test-io
  (is (= (run-program [3 0 4 0 99] [-999])
         (IntcodeMachine. 4 [-999 0 4 0 99] [] [-999]))))

(deftest test-immediate-flag
  (is (= (run-program [1002 4 3 4 33]) [1002 4 3 4 99])))

(defn atol [s] (Long/parseLong s))

(defn load-file [name]
  (mapv atol
        (-> name
            clojure.java.io/resource
            slurp
            (clojure.string/split #","))))
