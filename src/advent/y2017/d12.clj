(ns advent.y2017.d12)

(defn string-to-set-of-numbers [string]
  (->>  (clojure.string/split (clojure.string/replace string #"[<\->,]" "") #"\s+")
        (map #(Integer/parseInt %))
        (set)))

(defn read-file-as-list-of-sets-of-numbers [filename]
  (->> filename
       (slurp)
       (clojure.string/split-lines)
       (map string-to-set-of-numbers)))

(defn intersect-groups [[group unmatched-groups] next-group]
  (if (not-empty (clojure.set/intersection group next-group))
    (vector (clojure.set/union group next-group) unmatched-groups)
    (vector group (conj unmatched-groups next-group))))

(defn fix-point [f x]
  (let [fx (f x)]
    (if (= fx x) x
        (recur f fx))))

(defn regroup [[group unmatched]]
  (reduce intersect-groups [group []] unmatched))

(defn day-12-1 []
  (let [[zero-group & groups] (read-file-as-list-of-sets-of-numbers "day-12.input")]
    (count (first (fix-point regroup [zero-group groups])))))

(defn group-all [groups remaining]
  (if (empty? remaining) groups
      (let [[group remaining] (fix-point regroup [(first remaining) (rest remaining)])]
        (recur (conj groups group) remaining))))

(defn day-12-2 []
  (let [groups (read-file-as-list-of-sets-of-numbers "day-12.input")]
    (count (group-all [] groups))))
