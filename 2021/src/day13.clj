(ns day13
  (:require [clojure.java.io :as io]
            [clojure.java.math :as math]
            [clojure.string :as str]
            [grid-utils :as g]))

(def example
  "6,10
   0,14
   9,10
   0,3
   10,4
   4,11
   6,0
   6,12
   4,1
   0,13
   10,12
   3,4
   3,0
   8,4
   1,10
   2,14
   8,10
   9,0

   fold along y=7
   fold along x=5")
(def problem (slurp (io/resource "day13.txt")))

(defn to-point
  [string]
  (map parse-long (str/split string #",")))

(defn to-fold
  [fold]
  (let [[_ axis num] (re-matches #".*(.)=(\d+)" fold)]
    {:axis axis
     :num (parse-long num)}))

(defn pprint
  [points]
  (let [height (apply max (map second points))
        width (apply max (map first points))]
    (doseq [y (range (inc height))
            :let [_ (prn)]
            x (range (inc width))]
      (print (if (points [x y]) "#" ".")))
    (prn)))

(defn parse-input
  [input]
  (let [[points folds] (->> input
                            (str/split-lines)
                            (map str/trim)
                            (split-with (complement str/blank?)))]
    {:points (into #{} (map to-point) points)
     :folds (map to-fold (rest folds))}))

(defn fold*
  [[x y] {:keys [axis num]}]
  (case axis
    "x" [(- num (math/abs (- num x))) y]
    "y" [x (- num (math/abs (- num y)))]))

(defn fold
  [{:keys [points folds]}]
  (let [[fold & rest] folds]
    (when fold
      {:points (into #{} (map #(fold* % fold)) points)
       :folds rest})))

(comment
  (doseq [{:keys [points]} (->> problem
                                (parse-input)
                                (iterate fold)
                                (take-while some?))]
    (println "--------")
    (pprint points)))
