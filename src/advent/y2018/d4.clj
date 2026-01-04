(ns advent.y2018.d4
  (:require [clojure.test :refer :all]))

(defn read-input [filename]
  (-> filename
      clojure.java.io/resource
      slurp
      (clojure.string/split #"\n")
      sort))

(def input (read-input "y2018/day-4.input"))
(def test-input (read-input "day-4-test.input"))

(def parse-test )

(def rexps {:guard #"Guard #([0-9]+)"
            :sleep #"([0-9][0-9])\] falls asleep"
            :wakes #"([0-9][0-9])\] wakes up" })

(defn atol [a] (Long/parseLong a))

(defn try-parse [line rexp]
  (let [matches (re-find (second rexp) line)]
    (if matches [(first rexp) (atol (second matches))])))

(defn parse [line]
  (some #(try-parse line %) rexps))

(deftest test-parsing
  (is (= '([:guard 10] [:sleep 5] [:wakes 25])
         (map parse ["[1518-11-01 00:00] Guard #10 begins shift"
                     "[1518-11-01 00:05] falls asleep"
                     "[1518-11-01 00:25] wakes up"]))))

(defn add [guard-schedule from to]
  (if (= from to) guard-schedule
      (recur (update guard-schedule from inc) (inc from) to)))

(defn add-result [result guard from to]
  (let [guard-schedule (or (result guard) (vec (repeat 60 0)))
        new-guard-schedule (add guard-schedule from to)]
    (assoc result guard new-guard-schedule)))
    
(defn log-reducer [{:keys [result guard sleep] :as acc} [token value]]
  (if (= :wakes token)
    (assoc acc :result (add-result result guard sleep value))
    (assoc acc token value)))

(defn create-schedules [input]
  (:result (reduce log-reducer { :result {} } (map parse input))))

(defn guard-sleep-totals [input]
  (->> input
       create-schedules
       (reduce-kv #(assoc %1 %2 (reduce + %3)) {})))

(defn max-minute [schedule]
  (apply max-key second (map-indexed vector schedule)))

(defn find-longest-asleep [input]
  (let [total-asleep (guard-sleep-totals input)
        max-asleep-guard (first (apply max-key val total-asleep))
        max-asleep-minute (first (max-minute (schedules max-asleep-guard)))]
    (* max-asleep-guard max-asleep-minute)))

(defn part-one []
  (find-longest-asleep input))


(defn find-most-frequent-same-minute [input]
  (->> input
       create-schedules
       (reduce-kv #(assoc %1 %2 (max-minute %3)) {})
       (apply max-key #(second (val %)))))

(defn part-two []
  (let [[guard [minute count]] (find-most-frequent-same-minute input)]
    (* guard minute)))

        
