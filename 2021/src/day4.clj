(ns day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(def example
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")
(def problem (slurp (io/resource "day4.txt")))

(defn board
  [lines]
  (not-empty
   (into [] (comp
             (keep not-empty)
             (map str/trim)
             (map #(str/replace % #"\s+" " "))
             (map #(str/split % #" ")))
         lines)))

(defn parse-input
  [input]
  (let [[numbers _ & board-lines] (str/split-lines input)
        boards (into [] (comp
                         (partition-by str/blank?)
                         (keep board))
                     board-lines)]
    {:numbers (str/split numbers #",")
     :boards boards}))

(defn mark
  [n boards]
  (walk/postwalk #(if (= n %) true %) boards))

(defn winner?
  [board]
  (let [chicken-dinner? #(first (filter (partial every? true?) %))]
    (when (or (chicken-dinner? board)
              (chicken-dinner? (apply map vector board)))
      board)))

(defn find-winner
  [boards]
  (first (filter winner? boards)))

(defn winner-score
  [n board]
  (* (parse-long n)
     (reduce + 0
             (into [] (comp
                       (mapcat identity)
                       (remove boolean?)
                       (map parse-long))
                   board))))

(defn tabulate
  [{:keys [numbers boards]} & {:keys [remove-winners?]}]
  (reduce (fn [boards n]
            (let [new-boards (cond->> boards
                               true (mark n)
                               (and remove-winners? (< 1 (count boards))) (remove winner?))
                  winner (find-winner new-boards)]
              (if (and winner (or (not remove-winners?) (= 1 (count new-boards))))
                (reduced (winner-score n winner))
                (not-empty new-boards))))
          boards
          numbers))

(comment
  (tabulate (parse-input example))
  (tabulate (parse-input problem))
  (tabulate (parse-input example) :remove-winners? true)
  (tabulate (parse-input problem) :remove-winners? true))
