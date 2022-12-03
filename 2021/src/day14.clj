(ns day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [grid-utils :as g]))

(def example
  "NNCB

   CH -> B
   HH -> N
   CB -> H
   NH -> C
   HB -> C
   HC -> B
   HN -> C
   NN -> C
   BH -> H
   NC -> B
   NB -> B
   BN -> B
   BB -> N
   BC -> B
   CC -> N
   CN -> C")
(def problem (slurp (io/resource "day14.txt")))

(defn to-rule
  [string]
  (let [[_ substring insert] (re-matches #"(\w+) -> (\w+)" string)]
    [(seq substring) (first (seq insert))]))

(defn parse-input
  [input]
  (let [[[polymer] rules] (->> input
                               (str/split-lines)
                               (map str/trim)
                               (split-with (complement str/blank?)))]
    {:pairs (frequencies (partition 2 1 polymer))
     :counts (frequencies polymer)
     :rules (into {} (map to-rule) (rest rules))}))

(defn transform
  [{:keys [pairs rules] :as state}]
  (reduce-kv
   (fn [acc pair count]
     (let [new-char (rules pair)
           [first second] (partition 2 1 (interpose new-char pair))
           add (fnil + 0)]
       (-> acc
           ;; the net gain each step is simply `count` `new-char`s.
           (update-in [:counts new-char] add count)
           ;; we've lost these pairs...
           (update-in [:pairs pair] - count)
           ;; but gained these.
           (update-in [:pairs first] add count)
           (update-in [:pairs second] add count))))
   state
   pairs))

(defn polymerize
  [n state]
  (first (drop n (iterate transform state))))

(defn score
  [{:keys [counts]}]
  (let [ordered (sort (vals counts))]
    (- (last ordered) (first ordered))))

(defn size-of
  [{:keys [counts]}]
  (reduce + 0 (vals counts)))

(comment
  (let [polymer (->> example
                     (parse-input)
                     (polymerize 10))]
    (println (size-of polymer) (score polymer))))
