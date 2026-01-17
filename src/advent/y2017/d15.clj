(ns advent.y2017.d15
  (:use clojure.test))

(defn match-low-16-bits [^long a ^long b]
  (= (bit-and 0xFFFF a) (bit-and 0xFFFF b)))

(defn make-generator-fn [^long factor ^long mask]
  (fn ^long [^long val]
    (let [next-val (rem (* factor val) 2147483647)]
      (if (= 0 (bit-and mask next-val))
        next-val
        (recur next-val)))))

;; this is probably the definition of write only clojure code:-)
(defn fast-low-matcher
  ([^long a-fact ^long b-fact]
   (fast-low-matcher a-fact 0 b-fact 0))
  ([^long a-fact ^long a-mask ^long b-fact ^long b-mask]
   (let [a-fn (make-generator-fn a-fact a-mask)
         b-fn (make-generator-fn b-fact b-mask)]
     (fn [^long n ^long acc ^long a-val ^long b-val]
       (let [next-n (dec n)
             next-a (long (a-fn a-val))
             next-b (long (b-fn b-val))
             next-acc (if (match-low-16-bits next-a next-b) (inc acc) acc)]
         (if (= next-n 0)
           next-acc
           (recur next-n next-acc next-a next-b)))))))
   
;; part 1
(time ((fast-low-matcher 16807 48271) 40000000 0 277 349))

;; part 2
(time ((fast-low-matcher 16807 3 48271 7) 5000000 0 277 349))


;; ABANDONED LAZY SEQUENCE ATTEMPT - JUST TOO SLOW:-(
(defn lazy-generator [^long factor ^long start]
  (iterate #(rem (* factor %) 2147483647) start))

(defn generator-a [^long start]
  (lazy-generator 16807 start))

(defn generator-b [^long start]
  (lazy-generator 48271 start))

(defn example-a [] (generator-a 65))
(defn example-b [] (generator-b 8921))

(defn bits-zero [n]
  #(= (bit-and n %) 0))

(defn example-a-2 [] (filter (bits-zero 3) (example-a)))
(defn example-b-2 [] (filter (bits-zero 7) (example-a)))

(deftest successful-generation
  (is (= (take 5 example-a)
         '(1092455 1181022009 245556042 1744312007 1352636452)))
  (is (= (take 5 example-b)
         '(430625591 1233683848 1431495498 137874439 285222916))))

(defn count-low-matches [n a b]
  (reduce +
          (take n
                (map #(if (match-low-16-bits %1 %2) 1 0) a b))))

(deftest successful-count
  (is (= 1 (count-low-matches 5 (example-a) (example-b)))
      (= 588 (count-low-matches 40000000 (example-a) (example-b)))))
