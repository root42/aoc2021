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

;; Day 3.1
(defn add-digit
  [n input]
  (->>
   input
   (map #(bit-and 1 (bit-shift-right % n)))
   (reduce +))
  )

(defn calc-gamma
  "Calculates the gamma value and returns it as a vector of 1s and 0s."
  [N keep input]
  (loop [bits (range 0 (inc N))
         cur 0
         counts (vec (repeat N 0))]
    (if (= (count bits) 0)
      (reverse (map #(if (>= % (/ (count input) 2)) keep (- 1 keep)) counts))
      (recur (drop 1 bits)
             (first bits)
             (assoc counts cur (add-digit cur input))))))

(defn binvec-to-uint
  "Converts a vector of 1s and 0s to an unsigned integer"
  [v]
  (Integer/parseUnsignedInt (apply str v) 2)
  )

(defn pow
  [x n]
  (reduce * (repeat n x)))

(defn calc-power
  [input]
  (let [num-bits (count (first input))
        gamma (->>
               input
               (map #(Integer/parseUnsignedInt % 2))
               (calc-gamma num-bits 0)
               binvec-to-uint
               )
        epsilon (bit-and (bit-not gamma) (dec (pow 2 num-bits)))]
    (* gamma epsilon)
    )
  )

;; Day 3.2
(defn calc-support-value
  [num-bits keep input]
  (let [parsed (map #(Integer/parseUnsignedInt % 2) input)]
    (loop [bits (range 0 (inc num-bits))
           gamma (calc-gamma num-bits keep parsed)
           rating input
           i 0]
      (if (or (= 1 (count rating)) (empty? bits))
        (first rating)
        (let [r (filter (fn [v] (= (first (str (nth gamma i))) (nth v i))) rating)]
          (recur (drop 1 bits)
                 (calc-gamma num-bits keep (map #(Integer/parseUnsignedInt % 2) r))
                 r
                 (inc i))
          )))))

(defn calc-life-support-rating
  [input]
  (let [num-bits (count (first input))
        oxygen (Integer/parseUnsignedInt (calc-support-value num-bits 1 input) 2)
        co2 (Integer/parseUnsignedInt (calc-support-value num-bits 0 input) 2)]
    (* oxygen co2)
    ))

;; Day 4.1
(defn parse-board
  [board-text]
  (->> board-text
       (map #(clojure.string/split % #" +"))
       flatten
       (filter not-empty)
       (map #(Integer. % ))
       vec
       )
  )

(defn parse-bingo
  [input]
  (let [nums-text (first input)
        boards-text (drop 1 input)
        nums (map #(Integer. %) (re-seq #"[^,\n]+" nums-text))
        boards (map parse-board (loop [bt (drop 1 boards-text) ; skip the one leading empty line
                                       b []]
                                  (if (empty? bt)
                                    b
                                    (recur (drop 6 bt) ; drop current board + empty line
                                           (conj b (vec (take 5 bt))) ; take new board
                                           ))))]
    [nums boards]
    )
  )

(defn mark-board
  [n marks board ]
  (let [x (.indexOf board n)]
    (if (> x -1) (assoc marks x 1) marks)))

(defn mark-boards
  [n boards marks]
  (loop [r (range 0 (count boards))
         m []]
    (if (empty? r)
        m
        (let [i (first r)] 
          (recur (drop 1 r) (conj m (mark-board n (nth marks i) (nth boards i))))))))

(defn bingo-row
  [N i marks]
  (let [num (reduce + (map #(nth marks %) (for [x (range 0 N)] (+ x (* i N)))))]
    (if (= num N) true false)))

(defn bingo-col
  [N i marks]
  (let [num (reduce + (map #(nth marks %) (for [x (range 0 N)] (+ i (* x N)))))]
    (if (= num N) true false)))

(defn bingo?
  [N marks]
  (loop [r (range 0 N)]
    (if (empty? r)
      false
      (let [i (first r)]
        (if (or (bingo-row N i marks) (bingo-col N i marks))
          true
          (recur (drop 1 r)))))))

(defn filter-boards
  [boards marks]
  (loop [r (range 0 (count boards))
         result []]
    (if (empty? r)
      result
      (let [i (first r)]
        (if (bingo? 5 (nth marks i))
          (recur (drop 1 r) (conj result [(nth boards i) (nth marks i)]))
          (recur (drop 1 r) result)
          )))))

(defn calc-score
  [n [board marks]]
  (* n (reduce + (map * board (map #(- 1 %) marks))))
  )

(defn calc-bingo-score
  [nums boards]
  boards
  (loop [ns nums
         n (first ns)
         marks (repeat (count boards) (vec (repeat (count (first boards)) 0)))]
    (let [marked (filter-boards boards marks)]
      (if (> (count marked) 0)
        (calc-score n (first marked))
        (recur (drop 1 ns) (first ns) (mark-boards (first ns) boards marks))))))

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
    (println "3.2 Life support rating = " (calc-life-support-rating input))
    )
  (let [input (read-input "resources/input_4.txt")
        [nums boards] (parse-bingo input)]
    (println "4.1 Final score = " (calc-bingo-score nums boards))
    )
  )
