(ns advent.y2024.d4)

(def test-input "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(def input (slurp "resources/advent/y2024/day_4.txt"))

(defn parse [s]
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

(defn find-char [g ch]
  (filter (fn [c] (= ch (cat g c)))
          (squares g)))

(def dirs (for [x (range -1 2)
                y (range -1 2)
                :when (not= 0 x y)] [x y]))

(defn add [[x y] [x1 y1]]
  [(+ x x1) (+ y y1)])

(defn substgrid
  ([grid start direction length] (substgrid grid start direction length ""))
  ([grid start direction length acc]
   (let [c (cat grid start)
         n (add start direction)]
     (if (or (= length 0) (nil? c))
       acc
       (recur grid n direction (dec length) (str acc c))))))

(defn part-1 [input]
  (let [grid (parse input)
        xes (find-char grid \X)]
    (count
     (filter #(= % "XMAS")
             (for [x xes
                   y dirs]
               (substgrid grid x y 4))))))

(defn x-mas? [grid a-loc]
  (let [l-to-r (substgrid grid (add a-loc [-1 -1]) [1 1] 3)
        r-to-l (substgrid grid (add a-loc [1 -1]) [-1 1] 3)]
    (and
     (or (= l-to-r "MAS") (= l-to-r "SAM"))
     (or (= r-to-l "MAS") (= r-to-l "SAM")))))

(defn part-2 [input]
  (let [grid (parse input)
        as (find-char grid \A)]
    (count (filter (partial x-mas? grid) as))))

          
                  
     
  
  

