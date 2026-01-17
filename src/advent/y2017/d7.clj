(ns advent.y2017.d7
  (:use clojure.test))

(defrecord Program [name weight children])

(defn create-program-from-tokens [[name weight & children]]
  (Program. name (Integer/parseInt weight) children))

(defn tokenize-string [string]
  (clojure.string/split (clojure.string/replace string #"[\(\)\-\>#,]" "") #"\s+"))

(defn string-to-program [string]
  (->> string
       tokenize-string
       create-program-from-tokens))
  
(defn read-file-as-list-of-programs [filename]
  (->> filename
       (slurp)
       (clojure.string/split-lines)
       (map string-to-program)))

(def day-7-test (map string-to-program
                     ["pbga (66)"
                      "xhth (57)"
                      "ebii (61)"
                      "havc (66)"
                      "ktlj (57)"
                      "fwft (72) -> ktlj, cntj, xhth"
                      "qoyq (66)"
                      "padx (45) -> pbga, havc, qoyq"
                      "tknk (41) -> ugml, padx, fwft"
                      "jptl (61)"
                      "ugml (68) -> gyxo, ebii, jptl"
                      "gyxo (61)"
                      "cntj (57)"]))

(def input (read-file-as-list-of-programs "input/day-7.txt"))

; the one at the base cannot be a child
(defn find-base [programs]
  (let [child? (set (mapcat :children programs))]
      (some #(if (not (child? %)) %) (map :name programs))))

(deftest find-base-works
  (is (= "tknk" (find-base day-7-test))))

(defn part-1 []
  (find-base input))

(defn raise-difference-error [names-and-weights program-map]
  (let [freqs (frequencies (map second names-and-weights))
        right-weight (some #(if (not= (second %) 1) (first %)) freqs)
        wrong-weight (some #(if (= (second %) 1) (first %)) freqs)
        adjustment (- right-weight wrong-weight)
        wrong-name (some #(if (= (second %) wrong-weight) (first %)) names-and-weights)
        wrong-program (program-map wrong-name)]
    (throw (RuntimeException. (str wrong-name " should be " (+ adjustment (:weight wrong-program)))))))

(defn total-weight [program-name program-map]
  (let [program (program-map program-name)]
    (+ (:weight program)
       (if (:children program)
         (let [name-and-weights (map #(vector % (total-weight % program-map)) (:children program))]
           (if (apply = (map second name-and-weights))
             (reduce + (map second name-and-weights))
             (raise-difference-error name-and-weights program-map)))
         0))))

;; throws an exception if the tree is unbalanced
(defn add-total-weight [programs]
  (let [program-map (into {} (map #(vector (:name %) %) programs))
        base-program-name (find-base programs)]
    (total-weight base-program-name program-map)))

(defn part-2 []
  (add-total-weight input))
