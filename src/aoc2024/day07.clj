(ns aoc2024.day07
  (:require [clojure.string :as str]
            [aoc2024.util :as u]))

(def sample
  "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(defn parse-line [x]
  (let [target (Long/parseLong (first x))
        nums (->> (second x)
                  (re-seq #"\d+")
                  (map Long/parseLong))]
    [target nums]))

(defn valid? [[target nums]]
  (let [valid-help (fn valid-help [acc nums]
                     (if (not (seq nums))
                       (= target acc)
                       (or (valid-help (* acc (first nums)) (rest nums))
                           (valid-help (+ acc (first nums)) (rest nums)))))]
    (valid-help (first nums) (rest nums))))

(defn part-1 [s]
  (->> s
       str/split-lines
       (map #(str/split % #":"))
       (map parse-line)
       (filter valid?)
       (map first)
       (reduce + 0)))

(defn concat-long [x y]
  (Long/parseLong (str x y)))

(defn valid-2? [[target nums]]
  (let [valid-help (fn valid-help [acc nums]
                     (if (not (seq nums))
                       (= target acc)
                       (or (valid-help (* acc (first nums)) (rest nums))
                           (valid-help (+ acc (first nums)) (rest nums))
                           (valid-help (concat-long acc (first nums)) (rest nums)))))]
    (valid-help (first nums) (rest nums))))

(defn part-2 [s]
  (->> s
       str/split-lines
       (map #(str/split % #":"))
       (map parse-line)
       (filter valid-2?)
       (map first)
       (reduce + 0)))

(part-2 sample)

(comment

  (def input (u/get-input))

  (time (part-1 input))

  (time (part-2 input))

  ())
