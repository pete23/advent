(ns advent.y2017.core
  (:use clojure.math.numeric-tower))

;; day 3 - this sucks - there's an analytic formula for spiral number to coordinates
;; but unfortunately my discrete maths has decayed and it was easier just to generate the spiral

(defn manhattan-distance-to-origin [[x y]]
  (+ (abs x) (abs y)))

(defn add-coords [[a b] [c d]]
  (vector (+ a c) (+ b d)))

(defn next-arm [step prev]
  (let [chains (if (odd? step)
                 [[1 0] [0 1]]
                 [[-1 0] [0 -1]])]
    (drop 1 (reductions add-coords prev (mapcat #(repeat step %) chains)))))

(defn lazy-seq-of-spiral-cartesians
  ([] (cons [0 0] (lazy-seq (lazy-seq-of-spiral-cartesians 1 '(0 0)))))
  ([step prev] (let [arm (next-arm step prev)]
              (concat arm (lazy-seq (lazy-seq-of-spiral-cartesians (inc step) (last arm)))))))

(def spiral (lazy-seq-of-spiral-cartesians))

(defn advent-3-1 [spiral-number]
  (->> spiral-number
       (dec) ; correct their coordinate system
       (nth spiral)
       (manhattan-distance-to-origin)))

(defn adjacent-sums [grid coords]
  (reduce + (remove nil? (map #(get grid (add-coords coords %)) [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 1] [1 0] [1 -1]]))))

(defn add-adjacent-sum [[_ grid] next-coords]
  (let [next-value (adjacent-sums grid next-coords)]
    (vector next-value (assoc grid next-coords next-value))))
    
(defn advent-3-2 [spiral-number]
  (some #(if (< spiral-number (first %)) (first %))
        (reductions add-adjacent-sum [1 { '(0 0) 1 }] (drop 1 spiral))))

(advent-3-2 368078)

;; https://adventofcode.com/2017/day/4

(defn read-file-as-list-of-lists [filename]
  (->> filename
       (slurp)
       (clojure.string/split-lines)
       (map #(clojure.string/split % #"\s"))))
  
(def advent-4-input
  (read-file-as-list-of-lists "advent-4-input.txt"))

;; if we stuff stuff into a set and the set has the same cardinality as the stuff
;; then there are no duplicates in the stuff
(defn check-no-duplicates [stuff]
  (if (= (count stuff)
         (count (into #{} stuff)))
    1 0))

(defn advent-4-1 []
  (reduce + (map check-no-duplicates advent-4-input)))

(defn advent-4-2 []
  (reduce + (map check-no-duplicates (map #(map sort %) advent-4-input))))

;; advent day 8

(defn safe-op-map [m]
  (into {} (map #(vector (first %) (fnil (second %) 0 0)) m)))

(def string-to-op-map
  (safe-op-map
   {"inc" +
    "dec" - }))

(def string-to-predicate-map
  (safe-op-map
   {"!=" not=
    "==" =
    "<" <
    ">" >
    "<=" <=
    ">=" >=}))


(defn advent-8-parse-instruction 
  ;; b inc 5 if a > 1
  [[reg op-str operand-str _ pred-reg pred-str pred-operand-str]]
  {:reg reg
   :op (string-to-op-map op-str)
   :operand (Integer/parseInt operand-str)
   :pred-reg pred-reg
   :pred (string-to-predicate-map pred-str)
   :pred-operand (Integer/parseInt pred-operand-str)})
   
(defn advent-8-execute-instruction [state
                                    {:keys [reg op operand pred-reg pred pred-operand]}]
  (let [[current-max registers] state]
    (if (pred (registers pred-reg) pred-operand)
      (let [new-value (op (registers reg) operand)
            new-max (max current-max new-value)]
        (vector new-max (assoc registers reg new-value)))
        [current-max registers])))
  
(defn advent-8-engine [input]
  (->> input
       (map advent-8-parse-instruction)
       (reduce advent-8-execute-instruction [0 {}])))

(defn advent-8-1-test []
  (->> [["b" "inc" "5" "if" "a" ">" "1"]
        ["a" "inc" "1" "if" "b" "<" "5"]
        ["c" "dec" "-10" "if" "a" ">=" "1"]
        ["c" "inc" "-20" "if" "c" "==" "10"]]
       (advent-8-engine)))
         
(defn advent-8-1 []
  (->> "advent-8-input.txt"
       (read-file-as-list-of-lists)
       (advent-8-engine)
       (second)
       (map second)
       (reduce max)))

(defn advent-8-2 []
  (->> "advent-8-input.txt"
       (read-file-as-list-of-lists)
       (advent-8-engine)
       (first)))

