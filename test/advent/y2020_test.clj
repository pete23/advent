(ns advent.y2020-test
  (:require [clojure.test :refer :all]
            [advent.y2020.d1]
            [advent.y2020.d2]))

(deftest day-1-test
  (is (= 32064 (advent.y2020.d1/day-1-1)))
  (is (= 193598720 (advent.y2020.d1/day-1-2))))

(deftest day-2-test
  (is (= 469 (advent.y2020.d2/part-1)))
  (is (= 267 (advent.y2020.d2/part-2))))
