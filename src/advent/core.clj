(ns advent.core)

(defn ctoi [c]
  (let [i (- (int c) (int \0))]
    (when (<= 0 i 9) i)))

(defn atol [a]
  (Long/parseLong a))

(defn string->line [s]
  (mapv atol (clojure.string/split s #" +")))

(defn lines [s]
  (clojure.string/split-lines s))

(defn string->grid [s]
  (mapv string->line (lines s)))

(defn parse-grid [s]
  (let [g (clojure.string/split-lines s)
        x (count (first g))
        y (count g)]
    {:grid g :xs x :ys y}))

(defn cat [{:keys [grid xs ys]} [x y]]
  (cond
    (< x 0) nil
    (>= x xs) nil
    (< y 0) nil
    (>= y ys) nil
    :else (nth (nth grid y) x)))

(defn squares [{:keys [xs ys]}]
  (for [x (range xs)
        y (range ys)]
    [x y]))

(defn add [[x y] [x1 y1]]
  [(+ x x1) (+ y y1)])

(defn around [[x y]]
  [[(dec x) (dec y)]
   [x (dec y)]
   [(inc x) (dec y)]
   [(dec x) y]
   ;;[x y] selfnot included
   [(inc x) y]
   [(dec x) (inc y)]
   [x (inc y)]
   [(inc x) (inc y)]])

(defn sub [[x y] [x1 y1]]
  [(- x x1) (- y y1)])


(defn find-char [g ch]
  (filter (fn [c] (= ch (cat g c)))
          (squares g)))

(defn manhattan-distance [[x y] [x1 y1]]
  (+ (abs (- x x1))
     (abs (- y y1))))

(defn ctol [c]
  (if c (- (long c) 48) 0))



          
