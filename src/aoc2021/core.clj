(ns aoc2021.core
  (:gen-class)
  (:require [clojure.set :refer [difference union intersection]])
  (:require [clojure.math.numeric-tower :as math]))

(use 'clojure.math.combinatorics)
(use 'clojure.set)

(defn read-input
  [input-file]
  (let [input (slurp input-file)]
    (apply vector (map #(read-string %) (clojure.string/split-lines input))))
  )

(defn read-input-csv
  [input-file]
  (let [input (slurp input-file)]
    (map #(Integer. %) (re-seq #"[^,\n]+" input)))
  )

;; day 1
(defn calc-increases
  [input]
  (->>
   input
   (partition 2 1)
   (filter (fn [[x y]] (> y x)))
   (count)
   )
  )

(defn -main
  "Advent of Code 2021."
  [& args]
  (let [input (read-input "resources/input_1.txt")]
    (println "1.1 number of increases = " (calc-increases input))
    )
  )
