(ns day18
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.math :as math]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(def example
  "[[[[4,3],4],4],[7,[[8,4],9]]]
   [1,1]")
(def problem (slurp (io/resource "day17.txt")))

(defn parse-input
  [input]
  (->> input
       (str/split-lines)
       (map str/trim)
       (map edn/read-string)))

(defn split
  [n]
  [(quot n 2) (long (math/ceil (/ n 2)))])

(defn reduce-snail
  [tree]
  tree)

(defn add
  [[first second & rest]]
  (if second
    (recur (cons (reduce-snail [first second]) rest))
    first))

(comment
  )
