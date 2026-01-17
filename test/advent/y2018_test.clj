(ns advent.y2018-test
  (:require [clojure.test :refer :all]
            [advent.y2018.d1]
            [advent.y2018.d2]))

(deftest day-1-test
  (is (= 531 (advent.y2018.d1/part-1)))
  (is (= 76787 (advent.y2018.d1/part-2))))

(deftest day-2-test
  (is (= 5166 (advent.y2018.d2/part-1)))
  (is (= "cypueihajytordkgzxfqplbwn" (apply str (advent.y2018.d2/part-2)))))
