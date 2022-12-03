(ns day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example
  ["00100"
   "11110"
   "10110"
   "10111"
   "10101"
   "01111"
   "00111"
   "11100"
   "10000"
   "11001"
   "00010"
   "01010"])
(def problem (str/split-lines (slurp (io/resource "day3.txt"))))

(defn binary-num
  [val]
  (Long/valueOf val 2))

(defn order
  [digits]
  (sort-by val (frequencies digits)))

(defn digits
  [numbers]
  (apply map vector numbers))

(defn consumption2
  [numbers]
  (let [{:keys [gamma epsilon]}
        (reduce (fn [acc digits]
                  (let [[least-common most-common] (map first (order digits))]
                    (-> acc
                        (update :gamma str most-common)
                        (update :epsilon str least-common))))
                {:gamma "" :epsilon ""}
                (digits numbers))]
    (* (binary-num gamma) (binary-num epsilon))))

(defn filter-by-bits
  [f numbers]
  (loop [i 0
         candidates numbers]
    (let [digits (digits candidates)
          ordering (order (nth digits i))
          filtered (filter #(f % ordering i) candidates)]
      (cond
        (= 1 (count filtered)) (first filtered)
        (seq filtered) (recur (inc i) filtered)))))

(defn oxygen-rate
  [input]
  (let [filter-fn (fn [candidate [least most] i]
                    (let [digit (if (= (last least) (last most))
                                  \1
                                  (first most))]
                      (= digit (nth candidate i))))]
    (filter-by-bits filter-fn input)))

(defn co2-rate
  [input]
  (let [filter-fn (fn [candidate [least most] i]
                    (let [digit (if (= (last least) (last most))
                                  \0
                                  (first least))]
                      (= digit (nth candidate i))))]
    (filter-by-bits filter-fn input)))

(defn diagnostic
  [numbers]
  {:consumption (consumption2 numbers)
   :life-support-rating (* (binary-num (oxygen-rate numbers))
                           (binary-num (co2-rate numbers)))})

(comment
  (diagnostic example))
