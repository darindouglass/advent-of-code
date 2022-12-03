(ns day15
  (:require [clojure.java.io :as io]
            [clojure.data.priority-map :as p]
            [clojure.string :as str]
            [grid-utils :as g]))

(def example
  "1163751742
   1381373672
   2136511328
   3694931569
   7463417111
   1319128137
   1359912421
   3125421639
   1293138521
   2311944581")
(def problem (slurp (io/resource "day15.txt")))

(defn expand
  [factor {:keys [height width board] :as state}]
  (let [new-height (* factor height)
        new-width (* factor width)
        bound #(if (< 9 %) (inc (mod % 10)) %)
        new-board (->> board
                       (map-indexed vector)
                       (reduce (fn [acc [i risk]]
                                 (let [[x y] (g/xy state i)
                                       points (for [dy (range factor)
                                                    dx (range factor)
                                                    :let [x' (+ x (* width dx))
                                                          y' (+ y (* height dy))
                                                          risk' (bound (+ risk dy dx))]]
                                                [(g/index {:width new-width} [x' y']) risk'])]
                                   (into acc points)))
                               {})
                       (sort)
                       (map val))]
    {:height new-height
     :width new-width
     :board new-board}))

(defn board-edges
  [{:keys [board] :as state}]
  (assoc state :edges
         (into {}
               (for [i (range (count board))
                     [j risk] (g/neighbors state i)]
                 [[(g/xy state i) (g/xy state j)] risk]))))

(defn print-from-edges
  [{:keys [height width edges]}]
  (let [print-pad #(when % (print " " (if (< 9 %) % (format "% 2d" %))))]
    (doseq [y (range height)
            :let [_ (prn)]
            x (range width)
            :let [risk (edges [[x y] [(inc x) y]])
                  _ (when (zero? x)
                      (print-pad (if (zero? y)
                                   (parse-long (str (first example)))
                                   (edges [[x (dec y)] [x y]]))))]
            :when risk]
      #_(print [x y] [(inc x) y])
      (print-pad risk)))
  (prn))

;; shamelessly taken from: http://clj-me.cgrand.net/2010/09/04/a-in-clojure/
(defn euclidian-distance [a b]          ; multidimensional
  (Math/sqrt (reduce + (map #(let [c (- %1 %2)] (* c c)) a b))))

(defn A*
 "Finds a path between start and goal inside the graph described by edges
  (a map of edge to distance); estimate is an heuristic for the actual
  distance. Accepts a named option: :monotonic (default to true).
  Returns the path if found or nil."
 [edges estimate start goal & {mono :monotonic :or {mono true}}]
  (let [f (memoize #(estimate % goal)) ; unsure the memoization is worthy
        neighbours (reduce (fn [m [a b]] (assoc m a (conj (m a #{}) b)))
                      {} (keys edges))]
    (loop [q (p/priority-map start (f start))
           preds {}
           shortest {start 0}
           done #{}]
      (when-let [[x hx] (peek q)]
        (if (= goal x)
          (reverse (take-while identity (iterate preds goal)))
          (let [dx (- hx (f x))
                bn (for [n (remove done (neighbours x))
                         :let [hn (+ dx (edges [x n]) (f n))
                               sn (shortest n Double/POSITIVE_INFINITY)]
                         :when (< hn sn)]
                     [n hn])]
            (recur (into (pop q) bn)
              (into preds (for [[n] bn] [n x]))
              (into shortest bn)
              (if mono (conj done x) done))))))))

(comment
  (let [{:keys [edges height width]} (->> example
                                          (g/parse-grid)
                                          (expand 5)
                                          (board-edges)
                                          (g/pprint))
        path (A* edges euclidian-distance [0 0] [(dec width) (dec height)])]
    (->> path
         (rest)
         (map (comp edges vector) path)
         (reduce + 0))))
