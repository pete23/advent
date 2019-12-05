(ns intcode
  (:use clojure.test))



(defn decode-instruction [machine]
  (let [program (:program machine)
        [opcode src1 src2 dst] (subvec program (:pc machine))
        operation (case opcode 1 + 2 *)]
    (fn [machine] (IntcodeMachine. (+ 4 (:pc machine))
                                   (assoc program dst (operation (nth program src1) (nth program src2)))
                                   (:input machine)
                                   (:output machine)))))

  

(defn run-instruction [[opcode src1-loc src2-loc dst-loc] machine]
  (let [program (:program machine)
        operation (case opcode 1 + 2 *)
        src1 (nth program src1-loc)
        src2 (nth program src2-loc)
        result (operation src1 src2)]
    (assoc program dst-loc result)))

(defprotocol Machine
  (decode-instruction [_])
  (run-step [_])
  (run-machine [_]))

(defrecord IntcodeMachine [pc program input output]
  Machine

  (decode-instruction [machine]
      (let [[opcode src1 src2 dst] (subvec program pc)
            operation (case opcode 1 + 2 *)]
        (fn [m] (IntcodeMachine. (+ 4 (:pc m))
                                 (assoc program dst (operation (nth program src1) (nth program src2)))
                                 (:input m)
                                 (:output m)))))

  (run-step [machine]
    (let [instruction (decode-instruction machine)]
      (instruction machine)))
  
  (run-machine [machine]
    (loop [m machine]
      (if (= 99 (nth (:program m) (:pc m)))
        m
        (recur (run-step m))))))
        
(defn run-program
  ([program] (:program (run-machine (IntcodeMachine. 0 program [] [])))))

(deftest test-interpreter
  (is (= (run-program [1 0 0 0 99]) [2 0 0 0 99]))
  (is (= (run-program [2 3 0 3 99]) [2 3 0 6 99]))
  (is (= (run-program [2 4 4 5 99 0]) [2 4 4 5 99 9801]))
  (is (= (run-program [1 1 1 4 99 5 6 0 99]) [30 1 1 4 2 5 6 0 99])))
