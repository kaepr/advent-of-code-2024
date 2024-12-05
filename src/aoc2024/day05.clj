(ns aoc2024.day05
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [aoc2024.util :as u]))

(def sample
  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defn find-index [pred coll]
  (first (keep-indexed (fn [idx item]
                         (when (pred item) idx))
                       coll)))

(defn parse-input [s]
  (let [lines (str/split-lines s)
        empty-idx (find-index empty? lines)
        [page-rules page-updates] (split-at empty-idx lines)]
    {:page-rules (->> page-rules
                      (map #(str/split % #"\|"))
                      (map #(map Integer/parseInt %)))
     :page-updates (->> page-updates
                        next
                        (map #(str/split % #","))
                        (map #(map Integer/parseInt %)))}))

(defn add-edge [g u v]
  (update g u (fnil conj #{}) v)) ; fnil used so that first time
                                  ; update is called with nil,
                                  ; it becomes a set

(defn create-graph [edges]
  (reduce (fn [g [u v]]
            (add-edge g u v))
          {}
          edges))

(defn vertices [g]
  (reduce-kv
   (fn [acc k v] (set/union (conj acc k) v))
   #{}
   g))

(defn create-pair [v xs]
  (map (fn [x] `(~v ~x)) xs))

(defn page-number-pair [xs]
  (if (<= (count xs) 1)
    '()
    (concat
     (create-pair (first xs) (rest xs))
     (page-number-pair (rest xs)))))

(defn valid-update? [g page-update]
  (let [vertices (vertices g)
        both-exist? (fn [u v] (and (vertices u)
                                   (vertices v)))
        pairs (page-number-pair page-update)
        valid-update? (fn [[u v]]
                        (if (both-exist? u v)
                          (boolean ((get g u #{}) v))
                          true))]
    (->> pairs
         (map valid-update?)
         (every? true?))))

(defn middle-element [coll]
  (when (seq coll)
    (nth coll (quot (count coll) 2))))

(defn part-1 [s]
  (let [{:keys [page-rules page-updates]} (parse-input s)
        g (create-graph page-rules)]
    (->> page-updates
         (filter #(valid-update? g %))
         (map middle-element)
         (reduce + 0))))

(defn swap-element [xs idx1 idx2]
  (let [v (vec xs)]
    (-> v
        (assoc idx1 (get v idx2))
        (assoc idx2 (get v idx1))
        seq)))

(defn fix-mapping [g page-update]
  (if (valid-update? g page-update)
    page-update
    (let [vxs (vertices g)
          both-exist? (fn [u v] (and (vxs u) (vxs v)))
          pairs (page-number-pair page-update)
          valid? (fn [[u v]] (if (both-exist? u v)
                               (boolean ((get g u #{}) v))
                               true))
          f-invalid (first (filter #(not (valid? %)) pairs))
          elm1 (first f-invalid)
          elm2 (second f-invalid)
          i1 (find-index #(= % elm1) page-update)
          i2 (find-index #(= % elm2) page-update)
          u (swap-element page-update i1 i2)]
      (fix-mapping g u))))

(defn part-2 [s]
  (let [{:keys [page-rules page-updates]} (parse-input s)
        g (create-graph page-rules)]
    (->> page-updates
         (filter (fn [v] (not (valid-update? g v))))
         (map #(fix-mapping g %))
         (map middle-element)
         (reduce + 0))))

(comment

  (part-1 sample)

  (part-2 sample)

  (def input (u/get-input))

  (part-1 input)

  (part-2 input)

  ())
