(ns advent.y2017.d25
  (:use clojure.test))

(defrecord StateCondition [write move next])
(defn sc [write move next] (StateCondition. write move next))

;; you'll note that i cba parsing the input file as it wasn't terrifying
(def machine-definition
  {:a [(sc 1 1 :b) (sc 0 -1 :c)]
   :b [(sc 1 -1 :a) (sc 1 1 :d)]
   :c [(sc 0 -1 :b) (sc 0 -1 :e)]
   :d [(sc 1 1 :a) (sc 0 1 :b)]
   :e [(sc 1 -1 :f) (sc 1 -1 :c)]
   :f [(sc 1 1 :d) (sc 1 1 :a)]})

(defn tape-create []
  [[0] [0]])

(defn safe-nth [v i]
  (if (>= i (count v)) 0 (nth v i)))

(defn tape-read [[tape antitape] position]
  (if (< position 0)
    (safe-nth antitape (- position))
    (safe-nth tape position)))

(defn tape-write [[tape antitape] position value]
  (if (< position 0)
    (vector tape (assoc antitape (- position) value))
    (vector (assoc tape position value) antitape)))

(defn tape-checksum [[tape antitape]]
  (+ (reduce + tape) (reduce + antitape)))

(defn take-step [machine-definition [tape position state]]
  ;;(println state position tape)
  (let [state-def (machine-definition state)
        condition (nth state-def (tape-read tape position))
        new-tape (tape-write tape position (:write condition))
        new-position (+ position (:move condition))
        new-state (:next condition)]
    (vector new-tape new-position new-state)))

(def test-definition
  {:a [(sc 1 1 :b) (sc 0 -1 :b)]
   :b [(sc 1 -1 :a) (sc 1 1 :a)]})

(defn make-initial-state [state]
  [(tape-create) 0 state])

(defn run-n-steps [n machine-definition state]
  (if (= n 0)
    state
    (recur (dec n) machine-definition (take-step machine-definition state))))

(deftest test-checksum
  (is (= 3 (tape-checksum (first (run-n-steps 6 test-definition (make-initial-state :a)))))))

(defn part-1 []
  (tape-checksum (first (run-n-steps 12481997 machine-definition (make-initial-state :a)))))
