(ns advent.y2024.day-24
  (:require [advent.core :refer [atol]]
            [clojure.test :refer :all]))

(defn parse-values [values]
  (mapv (fn [value]
          (update (clojure.string/split value #": ") 1 atol))
        (clojure.string/split-lines values)))

(defn parse-rules [rules]
  (mapv (fn [rule]
          (let [[src1 op src2 dst] (clojure.string/split rule #"[ \->]+")]
            {:src1 src1 :op op :src2 src2 :dst dst}))
        (clojure.string/split-lines rules)))

(defn parse [input]
  (let [[values rules] (clojure.string/split input #"\n\n")]
    {:values (into {} (parse-values values))
     :rules (parse-rules rules)}))

(def test-input "x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02")

(def test (parse test-input))

(defn evaluate-1 [op val1 val2]
  ((case op
     "XOR" bit-xor
     "OR" bit-or
     "AND" bit-and) val1 val2))

(defn evaluate [{:keys [rules values]}]
  (reduce (fn [{:keys [values] :as acc} {:keys [src1 src2 op dst] :as rule}]
            (if-let [dstval (values dst)]
              ;; we have the dst val, nothing to do and don't propagate this rule
              acc 
              (let [src1val (values src1)
                    src2val (values src2)]
                (if (and src1val src2val)
                  ;; we have both src vals, create dst val, don't propagate this rule
                  (update acc :values assoc dst (evaluate-1 op src1val src2val))
                  ;; we don't have the src vals, propagate this rule to next iteration
                  (update acc :rules conj rule)))))
          {:values values
           :rules []}
          rules))

(defn iterate-to-fixpoint [f thing]
  (loop [thing thing]
    (let [new-thing (f thing)]
      (if (= new-thing thing)
        thing
        (recur new-thing)))))

(defn make-z [values]
  (reduce (fn [acc [^String k v]]
            (if (and (= (.charAt k 0) \z) (= v 1))
              (bit-set acc (atol (subs k 1)))
              acc))
          0
          values))

(defn part-1 [input]
  (let [system (parse input)
        complete-system (iterate-to-fixpoint evaluate system)]
    (make-z (:values complete-system))))

(deftest part-1-test
  (is (= 4 (part-1 test-input)))
  (is (= 2024 (part-1 "x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj"))))

(def input (slurp "resources/advent/y2024/day_24.txt"))

(time (part-1 input))

(count (:rules (parse input)))
        
;; loool
;; = 1.287952833 E+14
;; brute force swap attempts
