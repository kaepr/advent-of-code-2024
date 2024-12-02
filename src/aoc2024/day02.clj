(ns aoc2024.day02
  (:require [clojure.string :as str]))

(def sample
  "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(defn parse-string [s]
  (map Integer/parseInt (str/split s #" ")))

(defn chk [f]
  (fn
    ([_x] true)
    ([x y] (f x y))
    ([x y & more]
     (if (f x y)
       (if (next more)
         (recur y (first more) (next more))
         (f y (first more)))
       false))))

(def dec-le-3 (chk (fn [x y] (and (> x y) (>= 3 (- x y))))))

(def inc-le-3 (chk (fn [x y] (and (> y x) (>= 3 (- y x))))))

(defn safe? [v]
  (or (apply inc-le-3 v) (apply dec-le-3 v)))

(defn part-1 [input]
  (->> input
       str/split-lines
       (map parse-string)
       (map safe?)
       (filter true?)
       count))

;; https://stackoverflow.com/a/16382286
(defn without-each [x]
  (let [x (vec x)]
    (map (fn [idx] (concat (subvec x 0 idx) (subvec x (inc idx))))
         (range (count x)))))

(defn part-2 [input]
  (->> input
       str/split-lines
       (map parse-string)
       (map without-each)
       (map (fn [v]
              (->> v
                   (map safe?)
                   (some true?))))
       (filter true?)
       count))

(part-1 (slurp "./input.txt"))

(part-2 sample)

(part-2 (slurp "./input.txt"))
