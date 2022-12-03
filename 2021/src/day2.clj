(ns day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn commands
  [input]
  (into [] (comp
            (map str/trim)
            (map #(str/split %1 #" "))
            (map (juxt (comp keyword first) (comp parse-long second))))
        (str/split-lines input)))

(def example
  (commands
   "forward 5
   down 5
   forward 8
   up 3
   down 8
   forward 2"))
(def problem (commands (slurp (io/resource "day2.txt"))))

(defn move
  [{:keys [aim] :as pos} [command increment]]
  (let [forward? (= :forward command)
        axis (if forward? :x :aim)
        increment (if (= :up command) (- increment) increment)]
    (cond-> pos
      forward? (update :y + (* aim increment))
      true (update axis + increment))))

(defn move-all
  [commands]
  (reduce move {:x 0 :y 0 :aim 0} commands))

(defn compute
  [{:keys [x y]}]
  (* x y))

(comment
  (compute (move-all example))
  (compute (move-all problem)))
