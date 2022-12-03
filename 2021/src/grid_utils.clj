(ns grid-utils
  (:require [clojure.string :as str]))

(defn parse-grid
  [input]
  (let [lines (map str/trim (str/split-lines input))
        height (count lines)
        width (count (first lines))]
    {:height height
     :width width
     :board (mapcat #(map (comp parse-long str) %) lines)}))

(defn inbounds?
  [{:keys [height width]} [x y]]
  (and (<= 0 x (dec width))
       (<= 0 y (dec height))))

(defn xy
  [{:keys [width]} i]
  (if (vector? i)
    i

    [(mod i width) (quot i width)]))

(defn index
  [{:keys [width]} [x y]]
  (+ x (* width y)))

(defn neighbor
  [{:keys [board] :as state} direction i]
  (let [[x y] (xy state i)
        coord (case direction
                :up       [x (dec y)]
                :up-left  [(dec x) (dec y)]
                :up-right [(inc x) (dec y)]
                :left  [(dec x) y]
                :right [(inc x) y]
                :down       [x (inc y)]
                :down-left  [(dec x) (inc y)]
                :down-right [(inc x) (inc y)])
        neighbor (index state coord)]
    (when (inbounds? state coord)
      [neighbor (nth board neighbor)])))

(defn neighbors
  [state i & {:keys [diagonals?]}]
  (vec
   (cond->> [:up :down :left :right]
     diagonals? (concat [:up-left :up-right :down-left :down-right])
     true (keep #(neighbor state % i)))))

(defn pprint
  ([{:keys [board] :as state}]
   (pprint state board))
  ([{:keys [width]} board]
   (doseq [line (partition-all width board)]
     (apply println (map #(if (< 9 %) % (format "% 2d" %)) line)))))
