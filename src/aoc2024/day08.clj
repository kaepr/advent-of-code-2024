(ns aoc2024.day08
  (:require [clojure.string :as str]
            [aoc2024.util :as u]))

(def sample
  "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(defn order-points
  "Order points based on position of x coordinate.

  Lower x coordinate, should be first."
  [[x1 _y1 :as p1] [x2 _y2 :as p2]]
  (if (<= x1 x2)
    [p1 p2]
    [p2 p1]))

(defn antinode-points
  [p1 p2]
  (let [[[x1 y1 :as p1] [x2 y2 :as p2]] (order-points p1 p2)
        dx (abs (- x1 x2))
        dy (abs (- y1 y2))
        top-left? (<= y1 y2)]
    (if top-left?
      [[(- x1 dx) (- y1 dy)] [(+ x2 dx) (+ y2 dy)]]
      [[(- x1 dx) (+ y1 dy)] [(+ x2 dx) (- y2 dy)]])))

(antinode-points [1 2] [0 5])

(defn generate-pairs [coll]
  (set (for [p1 coll
             p2 coll
             :when (not= p1 p2)]
         [p1 p2])))

(defn antennas [grid]
  (set (for [row (range 0 (count grid))
             col (range 0 (count (first grid)))
             :when (not= "." (get-in grid [row col]))]
         [row col])))

(defn parse-grid [s]
  (->> s
       str/split-lines
       (mapv #(str/split % #""))))

(defn same-antenna? [grid p1 p2]
  (= (get-in grid p1) (get-in grid p2)))

(defn part-1 [s]
  (let [grid (parse-grid s)
        valid? (fn [[r c]]
                 (and (>= r 0)
                      (>= c 0)
                      (< r (count grid))
                      (< c (count (first grid)))))
        points (->> grid
                    antennas
                    generate-pairs
                    (filterv (fn [[p1 p2]] (same-antenna? grid p1 p2)))
                    (mapcat (fn [[p1 p2]] (antinode-points p1 p2)))
                    (filter valid?))]
    (count (set points))))

(defn lazy-point-seq
  [initial-value generator]
  (let [step (fn step [current]
               (lazy-seq (cons current
                               (step (generator current)))))]
    (step initial-value)))

(defn up-left [[[r c] dx dy]]
  [[(- r dx) (- c dy)] dx dy])

(defn down-right [[[r c] dx dy]]
  [[(+ r dx) (+ c dy)] dx dy])

(defn down-left [[[r c] dx dy]]
  [[(+ r dx) (- c dy)] dx dy])

(defn up-right [[[r c] dx dy]]
  [[(- r dx) (+ c dy)] dx dy])

(defn antinode-points-2
  [grid p1 p2]
  (let [valid? (fn [[r c]]
                 (and (>= r 0)
                      (>= c 0)
                      (< r (count grid))
                      (< c (count (first grid)))))
        [[x1 y1 :as p1] [x2 y2 :as p2]] (order-points p1 p2)
        dx (abs (- x1 x2))
        dy (abs (- y1 y2))
        top-left? (<= y1 y2)
        up-left-coordinates (lazy-point-seq [p1 dx dy] up-left)
        up-right-coordinates (lazy-point-seq [p2 dx dy] up-right)
        down-left-coordinates (lazy-point-seq [p2 dx dy] down-left)
        down-right-coordinates (lazy-point-seq [p1 dx dy] down-right)]
    (if top-left?
      (concat (take-while valid? (map first up-left-coordinates))
              (take-while valid? (map first down-right-coordinates)))
      (concat (take-while valid? (map first up-right-coordinates))
              (take-while valid? (map first down-left-coordinates))))))

(defn part-2 [s]
  (let [grid (parse-grid s)
        valid? (fn [[r c]]
                 (and (>= r 0)
                      (>= c 0)
                      (< r (count grid))
                      (< c (count (first grid)))))
        points (->> grid
                    antennas
                    generate-pairs
                    (filterv (fn [[p1 p2]] (same-antenna? grid p1 p2)))
                    (mapcat (fn [[p1 p2]] (antinode-points-2 grid p1 p2)))
                    (filter valid?))]
    (count (set points))))

(comment

  (part-1 (u/get-input))

  (part-2 (u/get-input))

  ())
