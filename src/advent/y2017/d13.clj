(ns advent.y2017.d13
  (:use clojure.test))


(defn integerize-string [string]
  (mapv #(Integer/parseInt %)
        (clojure.string/split
         (clojure.string/replace string #":" "") #"\s+")))

(defn read-file [filename]
  (->> filename
       (slurp)
       (clojure.string/split-lines)
       (map integerize-string)
       (into {})))

(def input (read-file "input/day-13.txt"))

(def test-input {0 3, 1 2, 4 4, 6 4})

(defn failed-layer?
  ;; if we treat the layer as a modulo round then we hit when rem depth (2range-2) == 0
  ([[depth range]] (failed-layer? [depth range] 0))
  ([[depth range] delay] (= 0 (rem (+ depth delay) (- (* 2 range) 2)))))

(defn layer-severity [[depth range]]
  ;; severity is depth * range iff the layer is hit
  (if (failed-layer? [depth range])
    (* depth range)
    0))

(defn total-severity [firewall]
  (reduce + (map layer-severity firewall)))

(deftest part-1-example
  (is (= 24 (total-severity test-input))))

(defn part-1 [] (total-severity input))


(defn successful-delay? [firewall delay]
  (if (not (some #(failed-layer? % delay) firewall)) delay))

(defn delay-to-pass [firewall]
  (some #(successful-delay? firewall %) (range)))

(deftest part-2-example
  (is (= 10 (delay-to-pass test-input))))

;; there's probably an analytic solution to this based on the arithmetic
;; this completes in about a second or three so i'll stop fretting
(defn part-2 [] (delay-to-pass input))
