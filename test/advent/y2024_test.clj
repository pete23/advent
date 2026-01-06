(ns advent.y2024-test
  (:require [clojure.test :refer :all]))

;; NOTE: 2024 solutions require dependencies not available from Maven Central:
;; - dom-top.core (from Clojars)
;; - clojure.core.matrix (from Maven Central but needs specific setup)
;;
;; To run these tests, add to project.clj:
;; [dom-top "1.0.9"]
;; [net.mikera/core.matrix "0.63.0"]
;; [net.mikera/vectorz-clj "0.48.0"]
;;
;; Files present:
;; - day_1.clj through day_25.clj (various days complete)
;; - Resources in resources/y2024/
;;
;; Once dependencies are available, run solutions and add tests here.

;; Example test structure (uncomment when dependencies available):
;;
;; (deftest day-1-test
;;   (require 'advent.y2024.d1)
;;   (is (= expected (advent.y2024.d1/part-1)))
;;   (is (= expected (advent.y2024.d1/part-2))))
