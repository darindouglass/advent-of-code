(ns day11
  (:require [clojure.java.io :as io]
            [grid-utils :as g]))

(def example
  "5483143223
   2745854711
   5264556173
   6141336146
   6357385478
   4167524645
   2176841721
   6882881134
   4846848554
   5283751526")
(def problem (slurp (io/resource "day11.txt")))

(defn parse-grid
  [input]
  (update (g/parse-grid input) :board vec))

(defn zero
  [board]
  (map #(if (< 9 %) 0 %) board))

(defn count-of
  [board v]
  (get (frequencies board) v 0))

(defn flasher?
  [v]
  (= 10 v))

(defn flash*
  [state board]
  (let [i (ffirst (drop-while (comp (complement flasher?) second) (map-indexed vector board)))
        neighbors (map first (g/neighbors state i :diagonals? true))]
    (reduce (fn [acc i]
              (if (= 10 (get acc i))
                acc
                (update acc i inc)))
            (update board i inc)
            neighbors)))

(defn flash
  [state board]
  (let [has-flash? (comp pos? #(count-of % 10))]
    (first (drop-while has-flash? (iterate #(flash* state %) board)))))

(defn step!
  [{:keys [board] :as state}]
  (assoc state :board (->> board
                           (mapv inc)
                           (flash state)
                           (zero))))

(defn simulate
  [grid]
  (rest (iterate step! grid)))

(defn synchronized-step
  [simulation]
  (->> simulation
       (map-indexed vector)
       (drop-while (fn [[_i {:keys [board]}]]
                     (not= (count board) (count-of board 0))))
       (ffirst)))

(comment
  (->> problem
       (parse-grid)
       (simulate)
       (take 10)
       (map :board)
       (map #(count-of % 0))
       (reduce + 0))
  (->> problem
       (parse-grid)
       (simulate)
       (synchronized-step)))
