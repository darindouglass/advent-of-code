(ns day16
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(def example "110100101111111000101000")
(def example2 "0011100000000000011011110100010100101001000100100000000")
(def example3 "11101110000000001101010000001100100000100011000001100000")
(def problem (slurp (io/resource "day16.txt")))

(defn bin
  [chars]
  (Long/parseLong (apply str chars) 2))

(defn normalize
  [input]
  (if (re-matches #"[01]+" input)
    input
    (str/replace (->> input
                      (str/trim)
                      (map #(format "%4s" (Long/toBinaryString (Long/parseLong (str %) 16))))
                      (str/join))
                 #" " "0")))

(defn read-literal
  [input]
  (let [part-size 5
        [data remainder] (loop [i 0
                                [char & remainder] input
                                input []]
                           (cond
                             (nil? char)
                             [input]

                             (and (zero? (mod i part-size))
                                  (= \0 (nth input (- i part-size) \1)))
                             [input (cons char remainder)]

                             :else
                             (recur (inc i) remainder (conj input char))))]
    [(->> data
          (partition-all part-size)
          (mapcat rest)
          (bin))
     remainder]))

(defn read-meta
  [input]
  (let [[version remainder] (split-at 3 input)
        [type remainder] (split-at 3 remainder)]
    [{:version (bin version)
      :type (bin type)}
     remainder]))

(defn packets
  [input]
  (loop [input input
         packets' []]
    (if (every? #{\0} input)
      packets'
      (let [[meta remainder] (read-meta input)
            packet #(assoc meta :data %)
            [flag & flagged-remainder] remainder]
        (cond
          (= 4 (:type meta))
          (let [[data remainder] (read-literal remainder)]
            (recur remainder (conj packets' (packet data))))

          (= \0 flag)
          (let [[count remainder] (split-at 15 flagged-remainder)
                [sub-packets remainder] (split-at (bin count) remainder)]
            (recur remainder (conj packets' (packet (packets sub-packets)))))

          :else
          (let [[sub-packet-count remainder] (split-at 11 flagged-remainder)
                [my-packets other-packets] (split-at (bin sub-packet-count) (packets remainder))]
            (concat packets' [(packet my-packets)] other-packets)))))))

(defn version-count
  [packets]
  (let [count (atom 0)]
    (walk/postwalk (fn [node]
                     (when (map? node)
                       (swap! count + (:version node)))
                     node)
                   packets)
    @count))

(defn calculate
  [{:keys [data type]}]
  (let [calculated (when-not (= 4 type)
                     (map calculate data))]
    (case type
      0 (apply + calculated)
      1 (apply * calculated)
      2 (apply min calculated)
      3 (apply max calculated)
      4 data
      5 (if (> (first calculated) (second calculated)) 1 0)
      6 (if (< (first calculated) (second calculated)) 1 0)
      7 (if (= (first calculated) (second calculated)) 1 0))))

(comment
  (->> example
       (normalize)
       (packets))
  (->> problem
       (normalize)
       (packets)
       (first)
       (calculate)))
