(ns day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example "3,4,3,1,2")
(def problem (slurp (io/resource "day6.txt")))

(def rate (dec 7))
(def incubation 2)

(defn parse-input
  [input]
  (frequencies (map parse-long (str/split (str/trim input) #","))))

(defn step!
  [fish]
  (let [aged (reduce-kv
              (fn [acc timer count]
                (let [new-timer (if (zero? timer)
                                  rate
                                  (dec timer))
                      current-val (get acc new-timer 0)]
                  (assoc acc new-timer (+ current-val count))))
              {}
              fish)
        spawn-count (get fish 0)]
    (cond-> aged
      spawn-count (assoc (+ rate incubation) spawn-count))))

(defn simulate
  [days fish]
  (->> fish
       (iterate step!)
       ;; handle the 0th step
       (take (inc days))
       (last)))

(defn count-fish
  [fish]
  (reduce + 0 (vals fish)))

(comment
  (->> problem
       (parse-input)
       (simulate 256)
       (count-fish)))
