(ns day7
  (:require [clojure.java.math :as math]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def example "16,1,2,0,4,2,7,1,2,14")
(def problem (slurp (io/resource "day7.txt")))

(defn parse-input
  [input]
  (map parse-long (str/split (str/trim input) #",")))

(defn sum-all
  [numbers]
  (reduce + 0 numbers))

(defn fuel-count
  [numbers]
  (apply min (for [i (range (apply max numbers))]
               (sum-all (map #(sum-all (range 1 (inc (math/abs (- % i))))) numbers)))))
