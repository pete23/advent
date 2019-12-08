(ns day-8
  (:use clojure.test))

(defn ctol [c] (- (int c) (int \0)))

(def input (->> "day-8.input"
                clojure.java.io/resource
                slurp
                (mapv ctol)))

(def WIDTH 25)
(def HEIGHT 6)

(defn split-layers [source height width]
  (partition (* height width) source))

(def layers (split-layers input WIDTH HEIGHT))

(defn part-1 []
  (let [freq (reduce #(if (< (%1 0) (%2 0)) %1 %2)
                     (map frequencies layers))]
    (* (freq 1) (freq 2))))

(defn definite-pixel [& front-to-back]
  (some #(if (< % 2) %) front-to-back))

(defn part-2 []
  (partition WIDTH
             (apply map definite-pixel layers)))))

;; then use emacs to turn the zeros to spaces so you can read it:-)
