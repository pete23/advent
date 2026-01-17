(ns advent.y2017.d18
  (:use clojure.test))

(defn recover [state _ register-value _]
  (if (not= 0 register-value)
    (assoc state :recovered-frequency (:sound-frequency state))
    state))

(defn sound [state _ register-value _]
  (assoc state :sound-frequency register-value))

(def part-1-initial-state 
  (merge (advent.register-machine/make-state)
         {:sound-frequency nil
          :recovered-frequency nil}))

(def part-1-instruction-set
  (merge (advent.register-machine/make-instructions)
         {"snd" sound
          "rcv" recover}))


(defn run-program
  ([program] (run-program program part-1-initial-state))
  ([program state] ;(println state)
   (if (:recovered-frequency state) state
       (recur program (advent.register-machine/take-step program state)))))

  
(def test-program (mapv #(advent.register-machine/string-to-instruction part-1-instruction-set %)
                        ["set a 1" "add a 2" "mul a a" "mod a 5"
                         "snd a" "set a 0" "rcv a" "jgz a -1"
                         "set a 1" "jgz a -2"]))

(deftest recover-4
  (is (= 4 (:recovered-frequency (run-program test-program)))))

(defn part-1 []
  (:recovered-frequency
   (run-program
    (advent.register-machine/read-file-as-program part-1-instruction-set "input/day-18.txt"))))

(defn send [state _ register-value _]
  (-> state
      (update-in [:outbound-queue] conj register-value)
      (update :outbound-sends inc)))

(defn receive [state register _ _]
  (if-let [message-received (peek (:inbound-queue state))]
    (-> state
        (assoc-in [:registers register] message-received)
        (update-in [:inbound-queue] pop))
    (-> state
        (assoc :blocking true)
        (update :program-counter dec))))
          
(def part-2-instruction-set
  (merge (advent.register-machine/make-instructions)
         {"snd" send
          "rcv" receive}))
(defn new-state [n]
  (-> (advent.register-machine/make-state)
      (merge {:ident n
              :registers { :p n :1 1 } ; set register 1 to avoid rewriting for values in position 1:-(
              :outbound-queue '()
              :outbound-sends 0
              :inbound-queue []
              :blocking false})))

(defn run-program-twice
  ([program] (run-program-twice program (new-state 0) (new-state 1)))
  ([program active waiting]
   ;; deadlock if we are blocking, and come back into control with nothing on the queue
   (if (and (:blocking active) (empty? (:inbound-queue active)))
     (if (= 1 (:ident active)) active waiting) ; return program 1 state for the answer!
     (let [next-active (advent.register-machine/take-step program (assoc active :blocking false))]
       (if (:blocking next-active)
         (recur program
                (assoc waiting :inbound-queue (vec (:outbound-queue next-active)))
                (assoc next-active :outbound-queue '()))                
         (recur program next-active waiting))))))

(deftest example-part-2
  (let [program (mapv #(advent.register-machine/string-to-instruction part-2-instruction-set %)
                      ["set x 7" "set y 2" "snd x" "snd y" "snd p" "rcv a" "rcv b" "rcv c" "rcv d"])
        end-state (run-program-twice program)
        end-registers (:registers end-state)]
    (is (= 3 (:outbound-sends end-state)))
    (is (= 7 (:a end-registers)))
    (is (= 2 (:b end-registers)))
    (is (= 0 (:c end-registers)))))

(defn part-2 []
  (:outbound-sends
   (run-program-twice
    (advent.register-machine/read-file-as-program part-2-instruction-set "input/day-18.txt"))))
