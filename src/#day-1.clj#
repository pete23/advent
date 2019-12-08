(ns day-1
  (:use clojure.test))

(defn atol [s] (Long/parseLong s))

(def input (->> "day-1.input"
                clojure.java.io/resource
                slurp
                clojure.string/split-lines
                (mapv atol)))

(defn fuel [n]
  (let [f (- (quot n 3) 2)]
    (if (< 0 f) f 0)))

(deftest test-fuel
  (is (= (fuel 12) 2))
  (is (= (fuel 14) 2))
  (is (= (fuel 1969) 654))
  (is (= (fuel 100756) 33583))
  (is (= (fuel 2) 0))
  (is (= (fuel 0) 0)))

(defn part-1 []
  (reduce + (map fuel input)))

(defn fixpoint-fuel
  ([n] (fix-point-fuel 0 (fuel n)))
  ([w n] (if (= n 0)
           w
           (recur (+ w n) (fuel n)))))

(deftest test-fixpoint-fuel
  (is (= (fixpoint-fuel 14) 2))
  (is (= (fixpoint-fuel 1969) 966))
  (is (= (fixpoint-fuel 100756) 50346)))

(defn part-2 []
  (reduce + (map fixpoint-fuel input)))

  
