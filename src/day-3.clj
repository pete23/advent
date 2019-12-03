(ns day-3
  (:use clojure.test))

(defn atol [s] (Long/parseLong s))
  
(def input (-> "day-3.input"
               clojure.java.io/resource
               slurp
               clojure.string/split-lines))

(defn defshift [x y]
  (fn [[a b]] (vector (+ x a) (+ y b))))

(def direction->shift
  {\U (defshift 0 1)
   \D (defshift 0 -1)
   \L (defshift -1 0)
   \R (defshift 1 0)})

(defn string->instruction [s]
  (let [shift (direction->shift (first s))
        count (atol (subs s 1))]
    (fn [origin]
      (take count (iterate shift (shift origin))))))

(defn instructions->path
  ([instructions] (reduce instructions->path [[0 0]] instructions))
  ([acc instruction] (concat acc (instruction (last acc)))))

(defn string->instructions [s]
  (map string->instruction (clojure.string/split s #",")))

(defn intersections [paths]
  (disj
   (apply clojure.set/intersection (map set paths))
   [0 0]))

(defn paths [input]
  (map instructions->path
       (map string->instructions input)))

(defn manhattan-distance [[x y]] (+ (Math/abs x) (Math/abs y)))

(deftest test-intersections
  (is (= (intersections ["R8,U5,L5,D3" "U7,R6,D4,L4"]) (set [[3 3] [6 5]]))))

(defn part-1 []
  (apply min (map manhattan-distance (intersections (paths input)))))

(defn path-length [path to]
  (.indexOf path to))

(defn total-path-length [paths to]
  (reduce + (map #(path-length % to) paths)))

(defn part-2 []
  (let [paths (paths input)
        intersections (intersections paths)
        intersection-path-lengths (map #(total-path-length paths %) intersections)]
    (apply min intersection-path-lengths)))
        
