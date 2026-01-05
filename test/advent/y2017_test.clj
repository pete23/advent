(ns advent.y2017-test
  (:require [clojure.test :refer :all]
            [advent.y2017.d1]
            [advent.y2017.d2]))

(deftest day-1-test
  (is (= 1343 (advent.y2017.d1/day-1-1)))
  (is (= 1274 (advent.y2017.d1/day-1-2))))

(deftest day-2-test
  (is (= 34925 (advent.y2017.d2/checksum)))
  (is (= 221 (advent.y2017.d2/checksum-2))))
