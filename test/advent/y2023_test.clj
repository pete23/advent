(ns advent.y2023-test
  (:require [clojure.test :refer :all]
            [advent.y2023.d1]))

(deftest day-1-test
  (is (= 54573 (advent.y2023.d1/part-1)))
  (is (= 54591 (advent.y2023.d1/part-2))))
