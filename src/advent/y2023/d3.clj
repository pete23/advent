(ns advent.y2023.d3
  (:require [clojure.string :refer [split-lines]]))

(def test-input (slurp "resources/advent/y2023/d3-test.txt"))
(def input (slurp "resources/advent/y2023/d3.txt"))

(def numbers #"[0-9]+")
(def symbols #"[^0-9\n\.]")

;; REGEXP API THAT YIELDS THE EXTENT
(defn matcher->seq [matcher acc]
  (let [found? (.find matcher)]
    (if (not found?) acc
        (recur matcher (conj acc {:start (.start matcher)
                                  :end (.end matcher)
                                  :group (.group matcher)})))))

(defn re-seq-pos [re s]
  (let [matcher (re-matcher re s)]
    (matcher->seq matcher [])))

;; ADJACENT CHECKS WHETHER BOUNDING BOX AROUND START END CONTAINS A SYMBOL
(defn in-range? [symbol? from to]
  (some symbol? (range from to)))

(defn adjacent? [line-length symbol? {:keys [start end]}]
  (or (symbol? (dec start))
      (symbol? end)
      (in-range? symbol? (- start line-length 1) (- end line-length -1))
      (in-range? symbol? (+ start line-length -1) (+ end line-length 1))))

;; FACTOR PARAMETERISATION
(defn compute-parameters [input symbols]
  {:line-length (inc (count (first (split-lines input))))
   :symbol-positions (into #{} (map :start (re-seq-pos symbols input)))
   :part-extents (map #(update % :group atol) (re-seq-pos numbers input))})

;; RETURN PARTS ADJACENT TO SYMBOLS
(defn parts-adjacent-to
  ([parameters] (parts-adjacent-to parameters (:symbol-positions parameters)))
  ([{:keys [line-length part-extents]} symbol-positions]
   (filter (partial adjacent? line-length symbol-positions) part-extents)))

;; PART-1
(defn part-1 [input]
  (let [parameters (compute-parameters input symbols)]
    (reduce + (map :group (parts-adjacent-to parameters)))))

;; PART-2
(def gears #"[*]")

(defn gear->ratio [parameters gear-position]
  (let [gear-position #{gear-position}
        parts (parts-adjacent-to parameters gear-position)]
    (if (= (count parts) 2)
      (reduce * (map :group parts))
      0)))

(defn part-2 [input]
  (let [parameters (compute-parameters input gears)
        gear-ratios (map (partial gear->ratio parameters) (:symbol-positions parameters))]
    (reduce + gear-ratios)))
