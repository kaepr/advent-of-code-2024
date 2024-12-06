(ns aoc2024.day06
  (:require [clojure.string :as str]
            [aoc2024.util :as u]))

(def sample
  "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

;; :g -> guard
;; :x -> not visited
;; :o -> obstacle
;; :v -> visited

(defn ->cell-type [s]
  (condp = s
    "." :x
    "#" :o
    "^" :g))

(defn parse-grid [s]
  (->> s
       str/split-lines
       (mapv #(str/split % #""))
       (mapv #(mapv ->cell-type %))))

(defn find-element-idx [pred coll]
  (first (for [row-idx (range (count coll))
               col-idx (range (count (first coll)))
               :let [v (get-in coll [row-idx col-idx])]
               :when (pred v)]
           [row-idx col-idx])))

(defn traverse [grid [row-idx col-idx]]
  (let [rows (count grid)
        cols (count (first grid))
        at-boundary? (fn [r c] (or (= r 0)
                                   (= c 0)
                                   (= r (- rows 1))
                                   (= c (- cols 1))))
        next-direction (fn [d]
                         (condp = d
                           :up :right
                           :right :down
                           :down :left
                           :left :up))
        next-index (fn [row col dir]
                     (condp = dir
                       :up [(- row 1) col]
                       :right [row (+ col 1)]
                       :down [(+ row 1) col]
                       :left [row (- col 1)]))
        next-obstacle? (fn [grid row col dir]
                         (= :o (get-in grid (next-index row col dir))))]
    (loop [grid (update-in grid [row-idx col-idx] (fn [_] :v))
           row row-idx
           col col-idx
           direction :up]
      (cond
        (at-boundary? row col) grid
        (next-obstacle? grid row col direction) (recur grid row col (next-direction direction))
        :else (let [[next-row next-col] (next-index row col direction)]
                (recur (update-in grid [next-row next-col] (fn [_] :v))
                       next-row
                       next-col
                       direction))))))

(defn count-visited [grid]
  (let [vf (fn [v] (if (= :v v) 1 0))]
    (->> grid
         (map #(map vf %))
         (map #(reduce + 0 %))
         (reduce + 0))))

(defn part-1 [s]
  (let [grid (parse-grid s)
        start (find-element-idx #(= :g %) grid)
        visited-grid (traverse grid start)]
    (count-visited visited-grid)))

(defn traverse-cycle [grid [row-idx col-idx]]
  (let [rows (count grid)
        cols (count (first grid))
        at-boundary? (fn [r c] (or (= r 0)
                                   (= c 0)
                                   (= r (- rows 1))
                                   (= c (- cols 1))))
        next-direction (fn [d]
                         (condp = d
                           :up :right
                           :right :down
                           :down :left
                           :left :up))
        next-index (fn [row col dir]
                     (condp = dir
                       :up [(- row 1) col]
                       :right [row (+ col 1)]
                       :down [(+ row 1) col]
                       :left [row (- col 1)]))
        next-obstacle? (fn [grid row col dir]
                         (= :o (get-in grid (next-index row col dir))))]
    (loop [grid (update-in grid [row-idx col-idx] (fn [_] :v))
           row row-idx
           col col-idx
           direction :up
           seen-states #{}]
      (cond
        (seen-states [row col direction]) :cycle-detected
        (at-boundary? row col) grid
        (next-obstacle? grid row col direction) (let [seen-states-updated (conj seen-states [row col direction])]
                                                  (recur
                                                   grid
                                                   row
                                                   col
                                                   (next-direction direction)
                                                   seen-states-updated))
        :else (let [[next-row next-col] (next-index row col direction)]
                (recur (update-in grid [next-row next-col] (fn [_] :v))
                       next-row
                       next-col
                       direction
                       (conj seen-states [row col direction])))))))

;; Didn't realize that state [row col direction] uniquely identifies
;; If state encountered again, then there must be a cycle
;; Saw solution on Slack, which helped me realize this
(defn part-2 [s]
  (let [grid (parse-grid s)
        start (find-element-idx #(= :g %) grid)
        rows (count grid)
        cols (count (first grid))
        start? (fn [r c] (and (= r (first start)) (= c (second start))))
        obstacles (for [r (range rows)
                        c (range cols)
                        :let [v (when-not (start? r c)
                                  (traverse-cycle (update-in grid [r c] (fn [_] :o))
                                                  start))]
                        :when (= :cycle-detected v)]
                    [r c])]
    (count obstacles)))

(comment

  (def input (u/get-input))

  (time (part-1 input))

  (time (part-2 input))

  ())
