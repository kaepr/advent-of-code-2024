(ns aoc2024.day01
  (:require [clojure.string :as str]))

(defn get-num [s] (map read-string (remove empty? (str/split s #" "))))

(def input "")

(def firsts
  (->> input
       (str/split-lines)
       (map get-num)
       (map first)
       sort))

(def lasts
  (->> input
       (str/split-lines)
       (map get-num)
       (map last)
       sort))

(comment

  (def ans1
    (reduce + (map #(abs (- %1 %2)) firsts lasts)))

  (def ans2
    (reduce + (map #(* % (get (frequencies lasts) % 0)) firsts)))

  ())

