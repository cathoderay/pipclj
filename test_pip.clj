(ns test-pip
  (:use pip clojure.test)
  (:require
    [clojure.contrib.math :as math]
    [clojure.contrib.generic.math-functions :as math-functions]))

;==================================
; find-max-from-intervals tests
;==================================

(deftest find-max-from-intervals-test
  (let [input [[[1 2] [3 4] [5 30] [6 5] [9 7]] #{[0 3] [3 4]}]
        expected [2 [0 3]]
        result (apply find-max-from-intervals input)]
    (is (= expected result)))
  (let [input [[[1 2] [3 4] [5 30] [6 5] [9 7]] #{[0 4]}]
        expected [2 [0 4]]
        result (apply find-max-from-intervals input)]
    (is (= expected result))))

;==================================
; max-in-interval tests
;==================================

(deftest max-in-interval-simple-case
  (let [input [[[1 2] [3 4] [5 30] [9 7]] [0 3]]
        expected [25.5 2]
        result (apply max-in-interval input)]
    (is (math-functions/approx= (first expected) (first result) 0.00001))))

(deftest max-in-interval-simple-case-2
  (let [input [[[1 2] [3 1] [5 30] [9 2]] [0 3]]
        expected [28 2]
        result (apply max-in-interval input)]
    (is (math-functions/approx= (first expected) (first result) 0.00001))))

;==================================
; pip tests
;==================================

(deftest pip-empty-input
  (let [input []
        expected []
        k 2
        result (pip k input)]
    (is (= expected result))))

(deftest pip-one-observation-input
  (let [input [[1 2]]
        expected [[1 2]]
        k 3
        result (pip k input)]
    (is (= expected result))))

(deftest pip-two-observations-input
  (let [input [[1 2] [2 3]]
        expected [[1 2] [2 3]]
        k 3
        result (pip k input)]
    (is (= expected result))))

(deftest pip-simple-test-case
  (let [input [[1 2] [2 3] [3 50] [4 10]]
        expected [[1 2] [3 50] [4 10]]
        k 3
        result (pip k input)]
    (is (= expected result))))

(deftest pip-required-more-points-than-series-length
  (let [input [[1 2] [2 3] [3 50] [4 10]]
        expected input
        k 10
        result (pip k input)]
    (is (= expected result))))

(deftest pip-testing-if-pip-is-evaluating-intervals-correctly
  (let [input [[0 1] [1 2] [2 4] [3 10] [4 20] [5 1] [6 3]
               [7 1] [8 10] [9 1] [10 10] [11 5] [12 30] [13 5]
               [14 1] [15 9] [16 13] [17 1] [18 0] [19 20] [20 1]
               [21 2] [22 4] [23 10] [24 5] [25 2] [26 50] [27 0]
               [28 7] [29 8] [30 5] [31 4] [32 3] [33 2] [34 20]
               [35 1] [36 3] [37 1] [38 10] [39 1] [40 10] [41 5]
               [42 30] [43 5] [44 1] [45 9] [46 13] [47 1] [48 0]
               [49 20] [50 1] [51 2] [52 4] [53 10] [54 20] [55 1]
               [56 3] [57 1] [58 10] [59 1] [60 10] [61 5] [62 30]
               [63 5] [64 1] [65 9] [66 13] [67 1] [68 0] [69 20]
               [70 1] [71 2] [72 4] [73 10] [74 5] [75 2] [76 50]
               [77 0] [78 7] [79 8] [80 5] [81 4] [82 3] [83 2]
               [84 20] [85 1] [86 3] [87 1] [88 10] [89 1] [90 10]
               [91 5] [92 30] [93 5] [94 1] [95 9] [96 13] [97 1]
               [98 0] [99 20]]
        expected [[0 1] [12 30] [25 2] [26 50] [27 0]
                  [42 30] [75 2] [76 50] [77 0] [99 20]]
        k 10
        result (pip k input)]
    (is (= expected result))))

;==================================
; vertical-distance tests
;==================================

(deftest vertical-distance-colinearity
  (let [p1 [1, 2]
        p3 [2, 3]
        p2 [3, 4]
        expected 0
        result (vertical-distance p1 p3 p2)]
    (is (math-functions/approx= expected result 0.00001))))

(deftest vertical-distance-positive
  (let [p1 [1, 2]
        p3 [2, 5]
        p2 [3, 2]
        expected 3
        result (vertical-distance p1 p3 p2)]
    (is (math-functions/approx= expected result 0.00001))))

(deftest vertical-distance-negative
  (let [p1 [1, 2]
        p3 [2, -1]
        p2 [3, 2]
        expected 3
        result (vertical-distance p1 p3 p2)]
    (is (math-functions/approx= expected result 0.00001))))

(deftest vertical-distance-float-positive
  (let [p1 [1, 2]
        p3 [2, 4.8]
        p2 [3, 5]
        expected 1.3
        result (vertical-distance p1 p3 p2)]
    (is (math-functions/approx= expected result 0.00001))))

(deftest vertical-distance-float-negative
  (let [p1 [1, 2]
        p3 [2, -10.0]
        p2 [3, 5]
        expected 13.5
        result (vertical-distance p1 p3 p2)]
    (is (math-functions/approx= expected result 0.00001))))

