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

(def opcodes {1 :add
              2 :mul
              3 :input
              4 :output
              5 :jump-if-true
              6 :jump-if-false
              7 :less-than
              8 :equals})

(defn lt [a b] (if (< a b) 1 0))
(defn eq [a b] (if (= a b) 1 0))

(def opcode->fn
  {:add +
   :mul *
   :less-than lt
   :equals eq
   :jump-if-false =
   :jump-if-true not=})

(defrecord IntcodeMachine [pc program input output]
  Machine

  (decode-instruction [machine]
    (let [instruction (subvec program pc)
          opcode (opcodes (rem (first instruction) 100))
          in (input-loader instruction program)
          out #(nth instruction %)]
      (case opcode
        (:add :mul :less-than :equals) [opcode (in 1) (in 2) (out 3)]
        :input [opcode (out 1)]
        :output [opcode (in 1)]
        (:jump-if-true :jump-if-false) [opcode (in 1) (in 2)])))
      
  (run-step [machine]
    (let [instruction (decode-instruction machine)
          ;_ (println pc instruction)
          opcode (first instruction)]
      (case opcode
        (:add :mul :less-than :equals)
        (let [[_ in1 in2 out] instruction
              operation (opcode->fn opcode)]
                      (IntcodeMachine. (+ 4 pc)
                                       (assoc program out (operation in1 in2))
                                       input
                                       output))

        :input
        (let [[_ out] instruction]
          (IntcodeMachine. (+ 2 pc)
                           (assoc program out (first input))
                           (rest input)
                           output))

        :output
        (let [[_ in] instruction]
          (IntcodeMachine. (+ 2 pc)
                           program
                           input
                           (conj output in)))
        
        (:jump-if-false :jump-if-true)
        (let [[_ value dest] instruction]
          (IntcodeMachine. (if ((opcode->fn opcode) 0 value)
                             dest
                             (+ 3 pc))
                           program
                           input
                           output)))))
  
  (run-machine [machine]
    (loop [m machine]
      (if (= 99 (nth (:program m) (:pc m)))
        m
        (recur (run-step m))))))
        
(defn run-program
  ([program] (:program (run-machine (IntcodeMachine. 0 program [] []))))
  ([program input] (run-machine (IntcodeMachine. 0 program input []))))

(defn compile [program]
  (fn [input]
    (:output (run-program program input))))

(defn compile-1 [program]
  (fn [input]
    (first (:output (run-program program (vector input))))))

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

(deftest test-comparators
  (let [is-eq-8 (compile-1 [3 9 8 9 10 9 4 9 99 -1 8])
        is-lt-8 (compile-1 [3 9 7 9 10 9 4 9 99 -1 8])
        is-eq-8i (compile-1 [3 3 1108 -1 8 3 4 3 99])
        is-lt-8i (compile-1 [3 3 1107 -1 8 3 4 3 99])]
    (is (= (is-eq-8 8) 1))
    (is (= (is-eq-8 9) 0))
    (is (= (is-lt-8 8) 0))
    (is (= (is-lt-8 7) 1))
    (is (= (is-eq-8i 8) 1))
    (is (= (is-eq-8i 9) 0))
    (is (= (is-lt-8i 8) 0))
    (is (= (is-lt-8i 7) 1))))
  
(defn atol [s] (Long/parseLong s))

(defn load-file [name]
  (mapv atol
        (-> name
            clojure.java.io/resource
            slurp
            (clojure.string/split #","))))
