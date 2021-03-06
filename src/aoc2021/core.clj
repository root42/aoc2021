(ns aoc2021.core
  (:gen-class)
  (:require [clojure.set :refer [difference union intersection]])
  (:require [clojure.math.numeric-tower :as math]))

(use 'clojure.math.combinatorics)
(use 'clojure.set)

(defn read-input
  [input-file]
  (let [input (slurp input-file)]
    (clojure.string/split-lines input))
  )

(defn read-integer-input
  [input-file]
  (let [input (slurp input-file)]
    (apply vector (map #(Integer. %) (clojure.string/split-lines input))))
  )

(defn read-input-csv
  [input-file]
  (let [input (slurp input-file)]
    (apply vector (map #(Integer. %) (re-seq #"[^,\n]+" input))))
  )

;; day 1
(defn calc-increases
  [input]
  (->>
   input
   (partition 2 1)
   (filter (fn [[x y]] (> y x)))
   count
   )
  )

(defn calc-increases-sliding
  [input]
  (->>
   input
   (partition 3 1)
   (map #(apply + %))
   calc-increases
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
                 ))) [0 0])
   (apply (fn [x d] (* x d)))))

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
                 ))) [0 0 0])
   (apply (fn [aim x d] (* x d)))))

;; Day 3.1
(defn add-digit
  [n input]
  (->>
   input
   (map #(bit-and 1 (bit-shift-right % n)))
   (reduce +)))

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
  (Integer/parseUnsignedInt (apply str v) 2))

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
    (* gamma epsilon)))

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
                 (inc i)))))))

(defn calc-life-support-rating
  [input]
  (let [num-bits (count (first input))
        oxygen (Integer/parseUnsignedInt (calc-support-value num-bits 1 input) 2)
        co2 (Integer/parseUnsignedInt (calc-support-value num-bits 0 input) 2)]
    (* oxygen co2)))

;; Day 4.1
(defn parse-board
  [board-text]
  (->> board-text
       (map #(clojure.string/split % #" +"))
       flatten
       (filter not-empty)
       (map #(Integer. % ))
       vec
       ))

(defn parse-bingo
  [input]
  (let [nums-text (first input)
        boards-text (drop 2 input)
        nums (map #(Integer. %) (re-seq #"[^,\n]+" nums-text))
        boards (map parse-board
                    (loop [bt boards-text
                           b []]
                      (if (empty? bt)
                        b
                        (recur (drop 6 bt) ; drop current board + empty line
                               (conj b (vec (take 5 bt))) ; take new board
                               ))))]
    [nums boards] ))

(defn mark-board
  [n [board marks]]
  (let [x (.indexOf board n)]
    (if (> x -1)
      [board (assoc marks x 1)]
      [board marks])))

(defn mark-boards
  [n boards]
  (loop [r (range 0 (count boards))
         m []]
    (if (empty? r)
        m
        (let [i (first r)
              board (nth boards i)] 
          (recur (drop 1 r) (conj m (mark-board n board)))))))

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
  [boards]
  (loop [r (range 0 (count boards))
         result []
         rejects []]
    (if (empty? r)
      [result rejects]
      (let [i (first r)
            marks (second (nth boards i))]
        (if (bingo? 5 marks)
          (recur (drop 1 r) (conj result (nth boards i)) rejects)
          (recur (drop 1 r) result (conj rejects (nth boards i)))
          )))))

(defn calc-score
  [n [board marks]]
  (* n (reduce + (map * board (map #(- 1 %) marks))))
  )

(defn calc-bingo-score
  [f nums boards]
  (loop [ns nums
         n (first nums)
         bs boards]
    (let [[result rejects] (filter-boards bs)]
      (if (> (count result) 0)
        [(calc-score n (f result)) ns rejects]
        (recur (drop 1 ns) (first ns) (mark-boards (first ns) bs))))))

(defn calc-first-bingo-score
  [nums only-boards]
  (let [marks (vec (repeat (count only-boards) (vec (repeat (count (first only-boards)) 0))))
        boards (map vector only-boards marks)]
    (first (calc-bingo-score first nums boards))))

(defn calc-last-bingo-score
  [nums only-boards]
  (let [marks (vec (repeat (count only-boards) (vec (repeat (count (first only-boards)) 0))))
        boards (map vector only-boards marks)]
    (loop [res nil
           ns nums
           bs boards]
      (let [[result ns2 rejects] (calc-bingo-score last ns bs)]
        (if (empty? rejects)
          result
          (recur result ns2 rejects))))))

;; Day 5.1
(defn parse-int-list
  [list]
  (->> list
       (map #(Integer. %))))

(defn parse-vent-data
  [input]
  (->> input
       (map #(re-seq #"(.*),(.*) -> (.*),(.*)" %))
       (map flatten)
       (map #(drop 1 %))
       (map parse-int-list))
  )

(defn horiz-or-vert?
  [line]
  (or (= (nth line 0) (nth line 2)) (= (nth line 1) (nth line 3))))

(defn draw-line
  [[x0 y0 x1 y1] vent-map]
  (let [dist-x (Math/abs (- x0 x1))
        dist-y (Math/abs (- y0 y1))
        steep (> dist-y dist-x)]
    (let [[x0 y0 x1 y1] (if steep [y0 x0 y1 x1] [x0 y0 x1 y1])]
      (let [[x0 y0 x1 y1] (if (> x0 x1) [x1 y1 x0 y0] [x0 y0 x1 y1])]
        (let  [dx (- x1 x0)
               dy (Math/abs (- y0 y1))
               y-step (if (< y0 y1) 1 -1)]
          (let [plot (if steep 
                       #(assoc %3 [%1 %2] (inc (get %3 [%1 %2] 0))) 
                       #(assoc %3 [%2 %1] (inc (get %3 [%2 %1] 0))))]
            (loop [vm vent-map
                   x x0
                   y y0
                   error (Math/floor (/ dx 2)) ]
              (if (<= x x1)
                (if (< error dy) 
                  (recur (plot x y vm) (inc x) (+ y y-step) (+ error (- dx dy)))
                  (recur (plot x y vm) (inc x) y            (- error dy)))
                vm))))))))

(defn more-than-two?
  [[coords amount]]
  (>= amount 2))

(defn calc-overlap-points
  [input f]
  (loop [lines (->> input
                    parse-vent-data
                    (filter f))
         vent-map {}]
    (if (empty? lines)
      (count (filter more-than-two? vent-map))
      (recur (drop 1 lines) (draw-line (first lines) vent-map)))))

;; Day 6.1
(defn count-fish
  [input]
  (loop [fish {}
         i input]
    (if (empty? i)
      fish
      (recur (assoc fish (first i) (inc (get fish (first i) 0))) (drop 1 i)))))

(defn breed-fish
  [fish]
  (loop [new-fish {}
         ages (keys fish)]
    (if (empty? ages)
      new-fish
      (let [age (first ages)
            new-ages (if (= age 0) [6 8] [(dec age)])
            next-gen (map vector new-ages (repeat (count new-ages) (get fish age)))]
        (recur (reduce (fn [coll [k v]] (assoc coll k (+ v (get coll k 0)))) new-fish next-gen) (drop 1 ages))))))

(defn calc-lantern-fish
  [input days]
  (loop [ds days
         fish (count-fish input)]
    (if (zero? ds)
      (reduce + (map val fish))
      (recur (dec ds) (breed-fish fish)))))

;; Day 7.1
(defn calc-crab-fuel-linear
  [xs x]
  (reduce + (map #(Math/abs (- x %)) xs))
  )

(defn calc-crab-fuel-median
  [input]
  (->> input
       sort
       (drop (/ (count input) 2))
       first
       (calc-crab-fuel-linear input))
  )

;; Day 7.2
(defn sum-i-to-N
  [N]
  (/ (* N (inc N)) 2))

(defn calc-crab-fuel-quadratic
  [xs x]
  (reduce + (map (fn [N] (sum-i-to-N (Math/abs (- x N)))) xs)))

(defn calc-crab-fuel-mean
  [input]
  (calc-crab-fuel-quadratic input (quot (reduce + input) (count input)))
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
    (println "3.2 Life support rating = " (calc-life-support-rating input))
    )
  (let [input (read-input "resources/input_4.txt")
        [nums boards] (parse-bingo input)]
    (println "4.1 Final score for first board = " (calc-first-bingo-score nums boards))
    (println "4.2 Final score for last boad = " (calc-last-bingo-score nums boards))
    )
  (let [input (read-input "resources/input_5.txt")]
    (println "5.1 Points with at least two horizontal or vertical lines overlapping = " (calc-overlap-points input horiz-or-vert?))
    (println "5.2 Points with at least two arbitrary lines overlapping = " (calc-overlap-points input any?))
    )
  (let [input (read-input-csv "resources/input_6.txt")]
    (println "6.1 Lantern fish after 80 days = " (calc-lantern-fish input 80))
    (println "6.2 Lantern fish after 256 days = " (calc-lantern-fish input 256))
    )
  (let [input (read-input-csv "resources/input_7.txt")]
    (println "7.1 Fuel for crab submarines = " (calc-crab-fuel-median input))
    (println "7.2 Fuel for crab submarines = " (calc-crab-fuel-mean input))
    )
  )
