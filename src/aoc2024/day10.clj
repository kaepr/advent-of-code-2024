(ns aoc2024.day10
  (:require [clojure.string :as str]
            [aoc2024.util :as u]))

(def sample
  "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

(def sample-2
  "0123
1234
8765
9876")

(defn parse-grid [s]
  (->> s
       str/split-lines
       (map #(str/split % #""))
       (mapv #(mapv Integer/parseInt %))))

(defn next-dirs [row col]
  [[(inc row) col]
   [(dec row) col]
   [row (inc col)]
   [row (dec col)]])

(#{} {:row 8})

(apply conj #{} [#{1 2 3}
                 #{1 2 3}])

(defn trailheads [g path row col prev-height]
  (let [v (get-in g [row col])
        traverse (fn [r c v]
                   (prn r c v)
                   (prn path)
                   (trailheads g (conj path {:row row
                                             :col col}) r c v))]
    (cond
      (nil? v) #{}
      (not= 1 (- v prev-height)) #{}
      (path {:row row
             :col col}) 0              ; trying to retread old path, break early
      (and (= prev-height 8) (= v 9)) path
      :else
      (apply conj #{} [(traverse (inc row) col v)
                       (traverse (dec row) col v)
                       (traverse row (inc col) v)
                       (traverse row (dec col) v)]))))

(trailheads (parse-grid sample-2) #{} 0 0 -1)

(defn part-1 [s]
  (let [g (parse-grid s)]))

(part-1 sample-2)

(comment

  ())
