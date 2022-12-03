(ns day17
  (:require [clojure.java.io :as io]))

(def example "target area: x=20..30, y=-10..-5")
(def problem (slurp (io/resource "day17.txt")))

(defn parse-input
  [input]
  (let [[x-low x-high y-low y-high] (->> input
                                         (re-find #"x=([-\d]+)\.\.([-\d]+).*y=([-\d]+)\.\.([-\d]+)")
                                         (rest)
                                         (map parse-long))]
    {:x [x-low x-high]
     :y [y-low y-high]}))

(defn battleship?
  [target {[x y] :position}]
  (let [{[x-low x-high] :x [y-low y-high] :y} target]
    (and (<= x-low x x-high)
         (<= y-low y y-high))))

(defn has-a-chance?
  [target {[x y] :position}]
  (let [{[_x-low x-high] :x [y-low _y-high] :y} target]
    ;; we still have a chance to hit if we haven't passed the box by
    (and (<= x x-high) (<= y-low y))))

(defn step!
  [{[x y] :position [dx dy] :velocity}]
  {:position [(+ x dx) (+ y dy)]
   :velocity [(cond-> dx
                (pos? dx) (dec)
                (neg? dx) (inc))
              (dec dy)]})

(defn simulate
  [target velocity]
  (->> {:position [0 0] :velocity velocity}
       (iterate step!)
       (reduce (fn [acc state]
                 (let [next (conj acc state)]
                   (cond
                     (not (has-a-chance? target state)) (reduced nil)
                     (battleship? target state) (reduced next)
                     :else next)))
               [])
       (not-empty)))

(defn velocities
  [target]
  (let [{[_x-low x-high] :x [y-low _y-high] :y} target]
    (for [;; any lower and we'll miss left; any more and we'll miss right
          dx (range 1 (inc x-high))
          ;; any lower and we'll miss low; any higher and we'll also miss low
          ;; (given the rules of the simulation, we'll always reach a point where
          ;; we're back at `y=0` (b/c the `y` arc is a parabola) but going `-dy` speed. so the next step MUST NOT
          ;; be greater than `(math/abs y-low)` otherwise we'd miss the target completely).
          dy (range y-low (inc (- y-low)))
          :let [hits (simulate target [dx dy])]
          :when hits]
      {:velocity [dx dy]
       :highest-point (apply max (map (comp second :position) hits))})))

(defn highest-point
  [hits]
  (apply max (map :highest-point hits)))

(comment
  (->> problem
       (parse-input)
       (velocities)
       (highest-point))
  (->> problem
       (parse-input)
       (velocities)
       (count)))
