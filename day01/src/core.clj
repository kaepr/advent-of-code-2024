(ns core
  (:require [clojure.string :as str]))

(def input (slurp "./input.txt"))

(defn get-num [s] (map Integer/parseInt (remove empty? (str/split s #" "))))

(->> input
     (str/split-lines)
     (map get-num))

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

(abs -123)

(def ans1
  (reduce + (map #(abs (- %1 %2)) firsts lasts)))

(def ans2
  (reduce + (map #(* % (get (frequencies lasts) % 0)) firsts)))
