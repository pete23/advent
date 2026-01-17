(ns advent.y2017.d23
  (:use clojure.test))

(defn counted-mul [state register register-value operand]
  (-> state
      (assoc-in [:registers register] (* register-value operand))
      (update-in [:mul-count] inc)))

(def part-1-instruction-set (merge
                             (advent.register-machine/make-instructions)
                             { "mul" counted-mul }))

(def part-1-initial-state (merge
                           (advent.register-machine/make-state)
                           { :mul-count 0 }))

(defn pc-out-of-bounds? [program {:keys [program-counter]}]
  (or (> 0 program-counter) (<= (count program) program-counter)))
  
(defn run-program [program state]
  (if (pc-out-of-bounds? program state) state
      (recur program (advent.register-machine/take-step program state))))

(defn part-1
  ([] (part-1
       (advent.register-machine/read-file-as-program part-1-instruction-set "input/day-23.txt")
       part-1-initial-state))
  ([program state] (:mul-count (run-program program state))))

;; when a=0, b=99, c=99, h=1
;; when a=0, b=98, c=98, h=1
;; when a=0, b=98, c=99,
;; when a=1, b=-109900, c=-92900, h=
(defn part-2
  ([] (part-2
       (advent.register-machine/read-file-as-program part-1-instruction-set "input/day-23.txt")
       part-1-initial-state))
  ([program state] (:h (:registers (run-program program state)))))
  
