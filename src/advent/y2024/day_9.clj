(ns advent.y2024.day-9
  (:require [dom-top.core :refer [loopr]]
            [clojure.test :refer :all]
            [clj-async-profiler.core :as prof]))

(defn ctol [c]
  (if c (- (long c) 48) 0))

(defn multiconj! [v i c]
  (reduce (fn [acc _] (conj! acc i)) v (range c)))

(defn parse-to-vector
  ([disk-map] (persistent! (parse-to-vector (transient []) 0 disk-map)))
  ([v next-id [file space & r]]
   (if (nil? file)
     v
     (recur (-> v
                (multiconj! next-id (ctol file))
                (multiconj! nil (ctol space)))
            (inc next-id)
            r))))

(defn find-nil [index v]
  (cond
    (= (count v) index) nil
    (nil? (nth v index)) index
    :else (recur (inc index) v)))

(defn defragment
  ([v] (defragment 0 (count v) v))
  ([index count v]
   (if-let [nildex (find-nil index v)]
     (let [count (dec count)]
       (recur nildex count (subvec (assoc v nildex (nth v count)) 0 count)))
     v)))

(defn checksum [v]
  (loopr
      [i 0 s 0]
      [id v]
      (recur (inc i) (+ s (* id i)))
    s))
      
(deftest part-1-test
  (is (= 1928
         (checksum (defragment (parse-to-vector "2333133121414131402"))))))

(def input (slurp "resources/advent/y2024/day_9.txt"))

(defn part-1 [input]
  (-> input
      parse-to-vector
      defragment
      checksum))

(defn parse-to-lists
  ([disk-map] (parse-to-lists {
                              :file->span (sorted-map)
                              :loc->free-count (sorted-map)}
                              0
                              0
                              disk-map))
  ([acc start-of-file next-id [file space & r]]
   (if (nil? file)
     acc
     (let [file (ctol file)
           space (or (ctol space) 0)
           end-of-file (+ start-of-file file)
           end-of-space (+ end-of-file space)]
       (recur (-> acc
                  (update :file->span assoc next-id [start-of-file file])
                  (update :loc->free-count assoc end-of-file space))
              end-of-space
              (inc next-id)
              r)))))

(defn consume-space [free-list new-start length]
  (if-let [prev-length (free-list new-start)]
    (let [new-length (- prev-length length)
          free-list (dissoc free-list new-start)]
      (if (= new-length 0)
          free-list
          (assoc free-list (+ new-start length) new-length)))
    free-list))

(defn find-new-start [freelist start-index length]
  (reduce (fn [_ [index free-length]]
            (cond
              (> index start-index) (reduced start-index)
              (<= length free-length) (reduced index)
              :else start-index))
          start-index
          freelist))
  
(defn defragment-2 [arena]
  (reduce
   (fn [acc [id [start-index length]]]
     (let [new-start (find-new-start (:loc->free-count acc) start-index length)]
       (-> acc
           (update :file->span assoc id [new-start length])
           (update :loc->free-count consume-space new-start length))))
   arena
   (reverse (:file->span arena))))

(defn checksum-2 [id->span]
  (reduce (fn [s [id [start-index length]]]
            (+ s (reduce + (map #(* id %) (range start-index (+ start-index length))))))
          0
          id->span))

(defn part-2 [input]
  (-> input
      parse-to-lists
      defragment-2
      :file->span
      checksum-2))


(prof/serve-ui 8080)
(prof/profile (time (part-2 "12345")))
