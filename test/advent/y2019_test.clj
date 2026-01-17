(ns advent.y2019-test
  (:require [clojure.test :refer :all]
            [advent.y2019.d1]
            [advent.y2019.d2]
            [advent.y2019.d3]
            [advent.y2019.d4]
            [advent.y2019.d5]
            [advent.y2019.d6]
            [advent.y2019.d7]))

(deftest day-1-test
  (is (= 3087896 (advent.y2019.d1/part-1)))
  (is (= 4628989 (advent.y2019.d1/part-2))))

(deftest day-2-test
  (is (= 2894520 (advent.y2019.d2/part-1)))
  (is (= [93 42] (advent.y2019.d2/part-2))))

(deftest day-3-test
  (is (= 1285 (advent.y2019.d3/part-1)))
  (is (= 14228 (advent.y2019.d3/part-2))))

(deftest day-4-test
  (is (= 460 (advent.y2019.d4/part-1)))
  (is (= 290 (advent.y2019.d4/part-2))))

(deftest day-5-test
  (is (= 0 (first (:output (advent.y2019.d5/part-1)))))
  (is (= 8834787 (first (:output (advent.y2019.d5/part-2))))))

(deftest day-6-test
  (is (= 160040 (advent.y2019.d6/part-1 advent.y2019.d6/input)))
  (is (= 373 (advent.y2019.d6/part-2 advent.y2019.d6/input))))

(deftest day-7-test
  (is (= 70597 (advent.y2019.d7/part-1)))
  (is (= 30872528 (advent.y2019.d7/part-2))))
