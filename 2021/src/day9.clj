(ns day9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example
  "2199943210
   3987894921
   9856789892
   8767896789
   9899965678")
(def problem (slurp (io/resource "day9.txt")))

(defn parse-input
  [input]
  (let [lines (map str/trim (str/split-lines input))
        height (count lines)
        width (count (first lines))]
    {:height height
     :width width
     :board (mapcat #(map (comp parse-long str) %) lines)}))

(defn xy
  [{:keys [width]} i]
  [(mod i width) (quot i width)])

(defn index
  [{:keys [width]} [x y]]
  (+ x (* width y)))

(defn inbounds?
  [{:keys [height width]} [x y]]
  (and (<= 0 x (dec width))
       (<= 0 y (dec height))))

(defn neighbor
  [{:keys [board] :as state} direction i]
  (let [[x y] (xy state i)
        coord (case direction
                :up    [x (dec y)]
                :down  [x (inc y)]
                :left  [(dec x) y]
                :right [(inc x) y])
        neighbor (index state coord)]
    (when (inbounds? state coord)
      [neighbor (nth board neighbor)])))

(defn neighbors
  [state i]
  (keep #(neighbor state % i) [:up :down :left :right]))

(defn lowpoint?
  [{:keys [board] :as state} i num]
  (every? #(< num (second %)) (neighbors state i)))

(defn lowpoints
  [{:keys [board] :as state}]
  (reduce
   (fn [acc [i num]]
     (cond-> acc
       (lowpoint? state i num) (conj [i num])))
   []
   (map-indexed vector board)))

(defn increasing?
  [{:keys [board] :as state} i num]
  (every? #(< num (nth board %)) (neighbors state i)))

(defn basin
  [state [i num :as lowpoint]]
  (let [neighbors (->> i
                       (neighbors state)
                       (filter #(< num (second %) 9)))]
    (into #{} (cons lowpoint (mapcat #(basin state %) neighbors)))))

(defn basins
  [state lowpoints]
  (map #(basin state %) lowpoints))

(defn basin-size
  [basins]
  (->> basins
       (map count)
       (sort)
       (take-last 3)
       (apply *)))

(defn risk-level
  [lowpoints]
  (reduce + 0 (map (comp inc second) lowpoints)))

(comment
  (->> example
       (parse-input)
       (lowpoints)
       (risk-level))
  (let [state (parse-input example)]
    (->> state
         (lowpoints)
         (basins state)
         (basin-size))))
