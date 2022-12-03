(ns day1
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def example [199 200 208 210 200 207 240 269 260 263])
(def problem (edn/read-string (slurp (io/resource "day1.edn"))))

(defn increasing?
  [[first second]]
  (< first second))

(defn sliding-window
  [n coll]
  (partition n 1 coll))

(defn count-f
  [f input]
  (->> input
       (sliding-window 2)
       (filter f)
       (count)))

(comment
  (count-f increasing? example)
  (->> example
       (sliding-window 3)
       (map #(apply + %1))
       (count-f increasing?)))
