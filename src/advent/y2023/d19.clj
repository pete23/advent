(ns advent.y2023.d19
  (:require [clojure.string :refer [split]]))

(defn parse-conditions [s])

(defn parse-workflow [s]
  ;; px{a<2006:qkq,m>2090:A,rfg}
  ;; {:label :px :conditions [{:var :a :cond < :val 2006 :dest :qkq}
  ;;                          {:var :m :cond > :val 2090 :dest :A}
  ;;                          {:dest :rfg}]
  (let [[label & r] (split s #"{")]
    {:label (keyword label)
     :conditions (parse-conditions r)}))
