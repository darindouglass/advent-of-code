(ns day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example
  "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")
(def problem (slurp (io/resource "day5.txt")))

(defn point
  [point-str]
  (let [[x y] (str/split (str/trim point-str) #",")]
    {:x (parse-long x)
     :y (parse-long y)}))

(defn points
  [line]
  (let [[p1 p2] (str/split line #"->")]
    (sort-by :x [(point p1) (point p2)])))

(defn vertical?
  [[{x1 :x} {x2 :x}]]
  (= x1 x2))

(defn horizontal?
  [[{y1 :y} {y2 :y}]]
  (= y1 y2))

(defn vertical-line
  [[{:keys [x]} :as points]]
  (let [[y1 y2] (sort (map :y points))]
    (map #(hash-map :x x :y %) (range y1 (inc y2)))))

(defn line
  [[{x1 :x y1 :y :as p1} {x2 :x y2 :y :as p2} :as points] & {:keys [ignore-diagonals?]}]
  (cond
    (vertical? points)
    (vertical-line points)

    (and ignore-diagonals? (not (horizontal? points)))
    []

    :else
    (let [m (/ (- y2 y1) (- x2 x1))
          b (- y1 (* m x1))]
      (loop [p p1
             points []]
        (if (= p2 p)
          (conj points p)
          (let [x (inc (:x p))
                p' {:x x
                    :y (+ (* m x) b)}]
            (recur p' (conj points p))))))))

(defn parse-input
  [input]
  (map points (str/split-lines input)))

(comment
  (->> example
       (parse-input)
       (mapcat #(line % :ignore-diagonals? false))
       (frequencies)
       (filter (comp #(< 2 %) val))
       (count))
  (->> example
       (parse-input)
       (mapcat line)
       (frequencies)
       (filter (comp #(< 2 %) val))
       (count)))
