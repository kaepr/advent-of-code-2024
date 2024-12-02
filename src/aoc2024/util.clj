(ns aoc2024.util
  (:require [clojure.java.io :as io]))

(defn get-input []
  (slurp (io/resource "input.txt")))
