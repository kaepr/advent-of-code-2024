(ns aoc2024.day09
  (:require [clojure.string :as str]
            [aoc2024.util :as u]))

(def sample "2333133121414131402")

(defn parse-block [s]
  (vec (flatten (into [] (map-indexed
                          (fn [idx [file-length free-space]]
                            (into [] (concat
                                      (take (Character/digit file-length 10) (repeat {:id idx
                                                                                      :val idx}))
                                      (take (Character/digit (or free-space \0) 10) (repeat :empty)))))
                          (partition 2 2 nil s))))))

(defn compact-block [blk]
  (let [free-space? (fn [blk idx] (= :empty (nth blk idx)))]
    (loop [front-idx 0
           back-idx (- (count blk) 1)
           blk blk]
      (cond
        (< back-idx front-idx) blk
        (not (free-space? blk front-idx)) (recur (inc front-idx) back-idx blk)
        (free-space? blk back-idx) (recur front-idx (dec back-idx) blk)
        :else (let [back (nth blk back-idx)
                    blk' (assoc blk front-idx back)
                    blk'' (assoc blk' back-idx :empty)]
                (recur (inc front-idx) (dec back-idx) blk''))))))

(defn parse-block-2 [s]
  (vec (flatten (into [] (map-indexed
                          (fn [idx [file-length free-space]]
                            (into [] (concat
                                      [{:size (Character/digit file-length 10)
                                        :id idx}]
                                      [{:size (Character/digit (or free-space \0) 10)
                                        :type :empty}])))
                          (partition 2 2 nil s))))))

(defn index-first-pred-match [xs pred]
  (->> xs
       (keep-indexed (fn [idx itm] (when (pred itm) [idx itm])))
       first))

(defn delete-at-index [coll index]
  (vec (concat (subvec coll 0 index)
               (subvec coll (inc index)))))

(defn insert-after [coll index item]
  (if (>= index 0)
    (vec (concat (subvec coll 0 index)
                 [item]
                 (subvec coll index)))
    (vec (concat [item] coll))))

(insert-after [1 2 3 4] 1 'a)

(defn move [block idx]
  (let [file (nth block idx)
        expected-size (:size file)
        empty-big-enough? (fn [itm]
                            (and (= :empty (:type itm))
                                 (>= (or (:size itm) 0) expected-size)))
        [empty-idx empty-block] (index-first-pred-match block empty-big-enough?)
        empty-idx (or empty-idx (count block))
        empty-idx (if (>= empty-idx idx)
                    nil
                    empty-idx)
        ;_ (prn "new")
        ;_ (prn file)
        ;_ (prn empty-idx empty-block)
        ;_ (prn block file idx)
        ]
    (if empty-idx
      (let [itm (nth block idx)
            coll (delete-at-index block idx)
            coll (insert-after coll idx {:size expected-size
                                         :type :empty})
            coll (delete-at-index coll empty-idx)
            coll (insert-after coll empty-idx itm)
            remaining-size (- (:size empty-block) expected-size)]
        (if (> remaining-size 0)
          (let [remaining-empty-block {:size remaining-size
                                       :type :empty}
                nxt-block (nth coll (inc empty-idx))]
            (if (= :empty (:type nxt-block))
              (assoc coll empty-idx {:type :empty
                                     :size (+ remaining-size (:size nxt-block))})
              (insert-after coll (inc empty-idx) remaining-empty-block)))
          coll))
      block)))

(defn compact-block-file [blk]
  (let []
    (loop [back (- (count blk) 1)
           blk blk]
      (cond
        (= 0 back) blk
        (= :empty (:type (nth blk back))) (recur (dec back) blk)
        :else (recur (dec back) (move blk back))))))

(defn checksum [v]
  (reduce + 0 (map-indexed
               (fn [idx itm]
                 (if (= itm :empty)
                   0
                   (* idx (or (:id itm) 0)))) v)))

(defn checksum-2 [v]
  (let [empty-f (fn [start e]
                  (+ start (:size e)))
        file-f (fn [start file]
                 {:sum (reduce + 0 (map (fn [v]
                                          (* (:id file) (+ v start)))
                                        (range (:size file))))
                  :nxt (+ start (:size file))})
        rf (fn [{:keys [sum start]} itm]
             (let [;_ (prn itm)
                   ;_ (prn sum start)
                   ]
               (if (= :empty (:type itm))
                 {:sum sum
                  :start (empty-f start itm)}
                 (let [v (file-f start itm)
                      ; _ (prn v)
                       ]
                   {:sum (+ sum (:sum v))
                    :start (:nxt v)}))))]
    (reduce rf {:start 0
                :sum 0} v)))

(defn part-1 [s]
  (-> s
      parse-block
      compact-block
      checksum))

(defn part-2 [s]
  (-> s
      parse-block-2
      compact-block-file
      checksum-2
      :sum))

(part-1 sample)

(part-2 sample)

(comment

  (def input (str/trim-newline (u/get-input)))

  (time (part-1 input))

  (time (part-2 input))

  ())

