(ns intcode
  (:refer-clojure :exclude [peek compile load-string load-file])
  (:use clojure.test))

(defprotocol Machine
  (opcode [_])
  (add-input [_ i])
  (needs-input? [_])
  (halted? [_])
  (decode-instruction [_])
  (run-step [_])
  (run-machine [_]))

(def flag->address-mode
  {0 :address
   1 :immediate
   2 :relative})

(defn opcode->address-modes [number]
  (loop [number (quot number 100)
         flags [:address :address :address]
         flag 0]
    (if (= number 0)
      flags
      (recur (quot number 10)
             (assoc flags flag (flag->address-mode (rem number 10)))
             (inc flag)))))

(def opcode->address-modes (memoize opcode->address-modes))

(defn peek [m i]
  (or (nth m i) 0))

(defn poke [m i n]
  ;(println :poke i n)
  (if (< i (count m))
    (assoc m i n)
    (vec (concat m (repeat (- i (count m)) 0) (vector n)))))

(defn resolve-operand [value io address-mode program base-address]
  (case io
    :in (case address-mode
          :address (peek program value)
          :immediate value
          :relative (peek program (+ value base-address)))
    :out (case address-mode
           :address value
           :immediate value
           :relative (+ value base-address))))
  
(def opcodes
  {1 :add
   2 :mul
   3 :input
   4 :output
   5 :jump-if-true
   6 :jump-if-false
   7 :less-than
   8 :equals
   9 :adjust-relative-base
   99 :halt})

(def opcode->operands
  {:add [:in :in :out]
   :mul [:in :in :out]
   :input [:out]
   :output [:in]
   :jump-if-true [:in :in]
   :jump-if-false [:in :in]
   :less-than [:in :in :out]
   :equals [:in :in :out]
   :adjust-relative-base [:in]
   :halt []})   

(defn lt [a b] (if (< a b) 1 0))
(defn eq [a b] (if (= a b) 1 0))

(def opcode->fn
  {:add +
   :mul *
   :less-than lt
   :equals eq
   :jump-if-false =
   :jump-if-true not=})

(defrecord IntcodeMachine [pc program input output relative-base]
  Machine

  (opcode [machine]
    (opcodes (rem (peek program pc) 100)))
  
  (decode-instruction [machine]
    (let [opcode (opcode machine)
          io (opcode->operands opcode)
          nargs (count io)
          address-modes (opcode->address-modes (nth program pc))
          source (subvec program (inc pc) (+ pc nargs 1))]
      (loop [args (transient []) i 0]
        (if (>= i nargs)
          [opcode (persistent! args)]
          (recur (conj! args (resolve-operand (source i) (io i) (address-modes i) program relative-base))
                 (inc i))))))
      
  (run-step [machine]
    (let [[opcode args] (decode-instruction machine)]
      (case opcode 
        (:add :mul :less-than :equals)
        (let [[in1 in2 out] args
              operation (opcode->fn opcode)]
                      (IntcodeMachine. (+ 4 pc)
                                       (poke program out (operation in1 in2))
                                       input
                                       output
                                       relative-base))

        :input
        (let [[out] args]
          (IntcodeMachine. (+ 2 pc)
                           (poke program out (first input))
                           (subvec input 1)
                           output
                           relative-base))

        :output
        (let [[in] args]
          (IntcodeMachine. (+ 2 pc)
                           program
                           input
                           (conj output in)
                           relative-base))
        
        (:jump-if-false :jump-if-true)
        (let [[value dest] args]
          (IntcodeMachine. (if ((opcode->fn opcode) 0 value)
                             dest
                             (+ 3 pc))
                           program
                           input
                           output
                           relative-base))

        :adjust-relative-base
        (let [[value] args]
          (IntcodeMachine. (+ 2 pc)
                           program
                           input
                           output
                           (+ relative-base value))))))
  
  (needs-input? [machine]
    (and (= :input (opcode machine)) (empty? input)))

  (add-input [machine to-add]
    (IntcodeMachine. pc program (conj input to-add) output relative-base))
  
  (halted? [machine]
    (= :halt (opcode machine)))
          
  (run-machine [machine]
    (loop [m machine]
      (if (or (halted? m) (needs-input? m))
       m
       (recur (run-step m))))))
        
(defn run-program
  ([program] (:program (run-machine (IntcodeMachine. 0 program [] [] 0))))
  ([program input] (run-machine (IntcodeMachine. 0 program input [] 0))))

(defn create-machine
  ([program] (IntcodeMachine. 0 program [] [] 0))
  ([program input] (IntcodeMachine. 0 program input [] 0)))

(defn compile [program]
  (fn [input]
    (:output (run-program program input))))

(defn compile-1 [program]
  (fn [input]
    (first (:output (run-program program (vector input))))))

(deftest peek-poke
  (is (= 1 (peek [1] 0)))
  (is (= 0 (peek [] 0)))
  (is (= 0 (peek [1] 1)))
  (is (= [1] (poke [0] 0 1)))
  (is (= [1] (poke [] 0 1)))
  (is (= [1 2 3 0 0 7] (poke [1 2 3] 5 7))))

(deftest test-interpreter-addition-multiplication-exit
  (is (= (run-program [1 0 0 0 99]) [2 0 0 0 99]))
  (is (= (run-program [2 3 0 3 99]) [2 3 0 6 99]))
  (is (= (run-program [2 4 4 5 99 0]) [2 4 4 5 99 9801]))
  (is (= (run-program [1 1 1 4 99 5 6 0 99]) [30 1 1 4 2 5 6 0 99])))

(deftest test-io
  (is (= (run-program [3 0 4 0 99] [-999])
         (IntcodeMachine. 4 [-999 0 4 0 99] [] [-999] 0))))

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

(deftest relative-addresses
  (let [p [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]]
    (is (= p (:output (run-program p []))))))

(deftest large-numbers
  (is (= 1125899906842624 (first (:output (run-program [104 1125899906842624 99] [])))))
  (is (= 1219070632396864 (first (:output (run-program [1102 34915192 34915192 7 4 7 99 0] []))))))

(deftest relative-output
  (let [p [109 100 203 10 99]]
    (is (= (nth (:program (run-program p [666])) 110) 666))))

(defn atol [s] (Long/parseLong s))

(defn load-string [s]
  (mapv atol (clojure.string/split s #",")))

(defn load-file [name]
  (load-string
        (-> name
            clojure.java.io/resource
            slurp)))
