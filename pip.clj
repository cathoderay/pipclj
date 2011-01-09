; Implementation of Perceptually Important Points Algorithm
; Author: Ronald Andreu Kaiser
; Third Version - Optimized
; Last Updated: Mon 29 Nov 2010 01:01:45 AM BRST


(ns pip
  (:require
    [clojure.contrib.math :as math]))


(defn vertical-distance
  "Returns the vertical distance between three points.
   P1 [x1 y1] and P2 [x2 y2] represent the extreme left and right PIPs respectively.
   P3 [x3 y3] is the candidate."
  [[x1 y1] [x3 y3] [x2 y2]]
  (math/abs
    (- (+ (double y1)
      (* (- (double y2) (double y1))
        (/ (- (double x3) (double x1))
          (- (double x2) (double x1)))))
      (double y3))))


(defn max-in-interval
  "Returns a vector with the best distance thorough the interval with the
   corresponding index in the original-series.
   Assumes that there is at least one point between P1 and P2."
  [original-series [p1-index p2-index :as interval]]
  (let [p1 (original-series p1-index)
        p2 (original-series p2-index)]
    (reduce
      (fn [[last-best-distance last-best-index :as last-best] candidate-index]
        (let [current-distance (vertical-distance p1 (original-series candidate-index) p2)]
          (if (> (double current-distance) (double last-best-distance))
            [current-distance candidate-index]
            last-best)))
      [-1 -1]
      (lazy-seq (range (inc p1-index) p2-index)))))


(defn find-max-from-intervals
  "Returns the index of the point in original-series that is most
   distant and the subjacent interval."
  [original-series intervals]
  (rest
    (reduce
      (fn [[last-best-distance last-best-index last-interval :as last-best] current-interval]
        (let [current-best
              (if (> (- (int (current-interval 1)) (int (current-interval 0))) 1)
                (max-in-interval original-series current-interval)
                last-best)]
          (if (> (double (current-best 0)) (double last-best-distance))
            [(current-best 0) (current-best 1) current-interval]
            last-best)))
      [-1 -1 [-1 -1]]
      intervals)))


(defn format-output
  "Returns a vector, which is a transformation of the
   current-intervals to points in the original-series
   representing the final PIPs."
  [original-series current-intervals]
  (let [sorted-intervals (sort-by first (vec current-intervals))]
    (reduce
      (fn [last-interval current-interval]
        (conj last-interval (original-series (current-interval 1))))
      [(original-series (ffirst sorted-intervals))]
      sorted-intervals)))


(defn pip
  "Returns a vector with the best k PIPs from original-series.
   If k is greater or equal the length of original-series,
   returns the same original-series."
  [k original-series]
  (binding [max-in-interval (memoize max-in-interval)]
    (let [original-length (int (count original-series))
          k (int k)]
      (if (or (< k (int 3))
        (< original-length (int 3))
        (>= k original-length))
        original-series
        (let [current-intervals #{[0 (- original-length 1)]}]
          (format-output
            original-series
            (nth
              (iterate
                (fn [current-intervals]
                  (let [new-pip-interval
                        (find-max-from-intervals original-series (lazy-seq current-intervals))
                        point-to-split (first new-pip-interval)
                        interval-to-split (second new-pip-interval)]
                    (conj (disj current-intervals interval-to-split)
                      [(first interval-to-split) point-to-split]
                      [point-to-split (second interval-to-split)])))
                current-intervals)
              (int (- k 2)))))))))
