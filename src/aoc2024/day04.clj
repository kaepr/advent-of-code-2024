(ns aoc2024.day04
  (:require [aoc2024.util :as u]
            [clojure.string :as str]))

(def sample
  "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(def directions [[0 1] ; move right
                 [0 -1] ; move left
                 [1 0] ; move down
                 [-1 0] ; move up
                 [-1 1] ; up right
                 [1 1] ; down right
                 [-1 -1] ; down left
                 [1 -1] ; up left
                 ])

(defn paths [start-x start-y]
  (let [df (fn [[delta-x delta-y]]
             (map (fn [v]
                    [(+ start-x (* delta-x v))
                     (+ start-y (* delta-y v))])
                  (range 0 4)))]
    (map df directions)))

(defn part-1 [s]
  (let [grid (->> s
                  str/split-lines
                  (map (comp vec seq))
                  vec)
        rows (count grid)
        cols (count (first grid))
        indexes (for [r (range 0 rows) c (range 0 cols)] [r c])
        xmas? (fn [v] (or
                       (= "XMAS" v)
                       (= "XMAS" (str/reverse v))))]
    (->> indexes
         (mapcat #(apply paths %))
         (map (fn [path] (map #(get-in grid %) path)))
         (map str/join)
         (filter xmas?)
         count
         ((fn [v] (/ v 2)))))) ; dividing by 2, as XMAS, is valid from both directions

(part-1 sample)

(defn part-2 [s]
  (let [grid (->> s
                  str/split-lines
                  (map (comp vec seq))
                  vec)
        rows (count grid)
        cols (count (first grid))
        A? (fn [r c] (= \A (get-in grid [r c])))
        ks->char (fn [ks] (get-in grid ks))
        mas? (fn [v] (or (= "MAS" v)
                         (= "MAS" (str/reverse v))))
        path->str (fn [v] (map (fn [p] (str/join (map ks->char p))) v))
        mas-path (fn [sr sc]
                   [[[(- sr 1) (- sc 1)] [sr sc] [(+ sr 1) (+ sc 1)]]
                    [[(+ sr 1) (- sc 1)] [sr sc] [(- sr 1) (+ sc 1)]]])
        indexes (for [r (range 0 rows) c (range 0 cols)] [r c])]
    (->> indexes
         (filter #(apply A? %))
         (map #(apply mas-path %))
         (map path->str)
         (filter (fn [[v1 v2]] (and (mas? v1) (mas? v2))))
         count)))

(part-2 sample)

(comment

  (part-1 (u/get-input))

  (part-2 (u/get-input))

  ())
