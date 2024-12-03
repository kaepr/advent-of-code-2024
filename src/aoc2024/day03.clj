(ns aoc2024.day03
  (:require [aoc2024.util :as u]))

(def sample
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(def mul-regex #"mul\(\d+\,\d+\)")

(defn parse-mul [s]
  (->> s
       (re-seq #"\d+")
       (map Integer/parseInt)))

(parse-mul "mul(5,5)")

(re-seq #"\d+" "mul(5123,5")

(re-seq mul-regex sample)

(defn part-1 [s]
  (->> s
       (re-seq mul-regex)
       (map parse-mul)
       (map #(apply * %))
       (reduce + 0)))

(part-1 sample)

(def sample-2
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(comment (time (part-1 (u/get-input))))

(def mul-do-dont-regex #"don't\(\)|do\(\)|mul\(\d+\,\d+\)")

(defn part-2 [s]
  (let [rf (fn [{:keys [res take?]} v]
             (cond
               (= v "don't()") {:take? false :res res}
               (= v "do()") {:take? true :res res}
               :else (if take?
                       {:take? take? :res (conj res v)}
                       {:take? take? :res res})))]
    (->> s
         (re-seq mul-do-dont-regex)
         (reduce rf {:take? true :res []})
         :res
         (map parse-mul)
         (map #(apply * %))
         (reduce + 0))))

(part-2 sample-2)

(comment
  (time (part-2 (u/get-input))))
