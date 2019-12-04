(ns day-4
  (:use clojure.test))

(defn digits
  ([n] (digits n '()))
  ([n a]
   (if (= n 0)
        a
        (recur (quot n 10) (conj a (mod n 10))))))

(def input (map digits (range 382345 843167)))

(defn has-double? [digits]
  (let [[a b] digits]
    (if b
      (if (= a b) true
          (recur (rest digits))))))

(defn is-ascending? [digits]
  (apply <= digits))

(defn part-1 []
  (count
   (filter (every-pred is-ascending? has-double?) input)))

(defn run-reducer [[run-digit run-count] digit]
  (cond
    ;; another digit in the current run
    (= digit run-digit) (vector digit (inc run-count))
    ;; end of run - if we had a run of 2, we've found a double
    (= 2 run-count) (reduced true)
    ;; if we have a nil then we're at the end
    (nil? digit) (reduced false)
    ;; otherwise it's a new run digit, count of 1
    :else (vector digit 1)))

(defn has-double-not-triple? [digits]
  (reduce run-reducer [nil 0] (concat digits '(nil))))

(defn part-2 []
  (count
   (filter (every-pred is-ascending? has-double-not-triple?) input)))
