(ns day-6
  (:use clojure.test))

(def input (-> "day-6.input"
               clojure.java.io/resource
               slurp
               clojure.string/split-lines))

(def test-input ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L"])

(defn string->pair [s]
  (clojure.string/split s #"\)"))

(defn input->orbital-dag [input]
  (reduce #(update %1 (first %2) conj (second %2))
          {}
          (map string->pair input)))

(defn calculate-orbits
  ([links] (calculate-orbits links {} {"COM" 0}))
  ([links acc next-planets]
   (if (empty? next-planets) acc
       (recur links (into acc next-planets)
              (reduce-kv (fn [m planet orbits]
                           (let [orbiters (links planet)]
                             (into m (map #(vector % (inc orbits)) orbiters)))) {} next-planets)))))
                           
(defn part-1
  ([]
   (part-1 input))
  ([input]
   (reduce + (map second (calculate-orbits (input->orbital-dag input))))))

(deftest part-1-test-input
  (= 42 (part-1 test-input)))

(defn input->origin-direction [input]
  (into {} (map #(vec (reverse (string->pair %))) input)))

(defn planet->origin-path [origin-direction planet]
  (loop [path (vector planet)]
    (let [next-planet (origin-direction (last path))]
      (if (nil? next-planet) path
          (recur (conj path next-planet))))))

(def part-2-test-input ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G"
                        "G)H" "D)I" "E)J" "J)K" "K)L" "K)YOU" "I)SAN"])

(defn part-2 [input]
  (let [orbital-distances (calculate-orbits (input->orbital-dag input))
        input->origin (input->origin-direction input)
        you->origin-set (set (planet->origin-path input->origin "YOU"))
        _ (println you->origin-set)
        common-ancestor (some you->origin-set (planet->origin-path input->origin "SAN"))]
    ;; orbital distance of SAN minus common ancestor
    ;; plus orbital distance of YOU minus common ancestor
    ;; minus two as YOU and SAN are assumed to be ON the first planet
    (- (+ (orbital-distances "SAN") (orbital-distances "YOU"))
       (+ (* (orbital-distances common-ancestor) 2) 2))))

        
