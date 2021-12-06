(ns aoc2021.core
  (:gen-class)
  (:require [clojure.set :refer [difference union intersection]])
  (:require [clojure.math.numeric-tower :as math]))

(use 'clojure.math.combinatorics)
(use 'clojure.set)

(defn read-input
  [input-file]
  (let [input (slurp input-file)]
    (apply vector (clojure.string/split-lines input)))
  )

(defn read-integer-input
  [input-file]
  (let [input (slurp input-file)]
    (apply vector (map #(Integer. %) (clojure.string/split-lines input))))
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

(defn calc-increases-sliding
  [input]
  (->>
   input
   (partition 3 1)
   (map #(apply + %))
   (calc-increases)
   )
  )

;; day 2
(defn calc-depth
  [input]
  (->>
   input
   (map #(clojure.string/split % #" "))
   (reduce (fn [[x d] [a vs]]
             (let [v (Integer. vs)]
               (case a
                 "forward" [(+ x v) d]
                 "down" [x (+ d v)]
                 "up" [x (- d v)]
                 )
               )
             )
           [0 0]
           )
   (apply (fn [x d] (* x d)))
   )
  )

(defn calc-aim
  [input]
  (->>
   input
   (map #(clojure.string/split % #" "))
   (reduce (fn [[aim x d] [a vs]]
             (let [v (Integer. vs)]
               (case a
                 "forward" [aim (+ x v) (+ d (* aim v))]
                 "down" [(+ aim v) x d]
                 "up" [(- aim v) x d]
                 )
               )
             )
           [0 0 0]
           )
   (apply (fn [aim x d] (* x d)))
   )
  )

;; Day 3
(defn add-digit
  [n input]
  (->>
   input
   (map #(bit-and 1 (bit-shift-right % n)))
   (reduce +))
  )

(defn calc-gamma
  [input]
  (let [bitcount 
        (loop [bits (range 0 13)
               cur 0
               counts [0 0 0 0 0 0 0 0 0 0 0 0]]
          (if (= (count bits) 0)
            counts
            (recur (drop 1 bits)
                   (first bits)
                   (assoc counts cur (add-digit cur input))))
          )
        len (count input)]
    (Integer/parseUnsignedInt (clojure.string/reverse (apply str (map #(if (> % (/ len 2)) "1" "0") bitcount))) 2)
    )
  )

(defn calc-power
  [input]
  (let [gamma (->>
               input
               (map #(Integer/parseUnsignedInt % 2))
               (calc-gamma)
               )
        epsilon (bit-and (bit-not gamma) 2r111111111111)]
    (* gamma epsilon)
    )
  )

(defn -main
  "Advent of Code 2021."
  [& args]
  (let [input (read-integer-input "resources/input_1.txt")]
    (println "1.1 number of increases = " (calc-increases input))
    (println "1.2 number of increases = " (calc-increases-sliding input))
    )
  (let [input (read-input "resources/input_2.txt")]
    (println "2.1 horizontal position * depth = " (calc-depth input))
    (println "2.2 horizontal position * depth = " (calc-aim input))
    )
  (let [input (read-input "resources/input_3.txt")]
    (println "3.1 Power consumption = " (calc-power input))
    ;; (println "2.2 horizontal position * depth = " (calc-aim input))
    )
  )
