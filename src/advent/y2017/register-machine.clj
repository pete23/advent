(ns advent.y2017.register-machine
  (:use clojure.test))

(defn make-state []
  {:program-counter 0
   :registers { :1 1 }})

(defn plain-set [_ v] v)

(defn reg-op-fn [op]
  (fn [state register register-value operand]
    (assoc-in state [:registers register] (op register-value operand))))

(defn jump [state size]
  (assoc state :program-counter (+ (:program-counter state) size -1))) ;; -1 corrects the default PC advance  

(defn cond-zero-jump-fn [cond-fn]
  (fn [state _ register-value operand]
    (if (cond-fn 0 register-value)
      (jump state operand)
      state)))

(defn make-instructions []
  {"set" (reg-op-fn plain-set)
   "add" (reg-op-fn +)
   "sub" (reg-op-fn -)
   "mul" (reg-op-fn *)
   "mod" (reg-op-fn rem)
   "jnz" (cond-zero-jump-fn not=)
   "jgz" (cond-zero-jump-fn <)})

(defn create-op [op-fn register operand-fn]
  (fn [state]
    (let [register-value (or (register (:registers state) 0))
          operand (if operand-fn (operand-fn state))]
      (op-fn state register register-value operand))))

(defn numeric-string? [string]
  (re-matches #"\-?[0-9]+" string))

(defn translate-operand-to-fn [operand-string]
  (if (numeric-string? operand-string)
    ;; we have a numeric constant not state dependent
    (let [number (Integer/parseInt operand-string)]
      (fn [_] number))
    ;; we have a reference to a register
    (let [register (keyword operand-string)]
      (fn [state] (or (register (:registers state)) 0)))))

(defn create-instruction-from-strings [instruction-set [opcode register-string operand-string]]
  (let [register (keyword register-string)
        operand-fn (if operand-string (translate-operand-to-fn operand-string))]
    (create-op (instruction-set opcode) register operand-fn)))

(defn tokenize-string [string]
  (clojure.string/split string #"\s+"))

(defn string-to-instruction [instruction-set string]
  (->> string
       tokenize-string
       (create-instruction-from-strings instruction-set)))
  
(defn read-file-as-program [instruction-set filename]
  (->> filename
       (slurp)
       (clojure.string/split-lines)
       (mapv #(string-to-instruction instruction-set %))))

(defn take-step [program state]
;  (println state)
  (let [op (nth program (:program-counter state))]
    (-> state
        (op)
        (update-in [:program-counter] inc))))

