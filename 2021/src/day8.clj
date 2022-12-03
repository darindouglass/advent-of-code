(ns day8
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def example
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
   edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
   fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
   fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
   aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
   fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
   dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
   bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
   egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
   gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")
(def problem (slurp (io/resource "day8.txt")))

(defn parse-input
  [input]
  (->> input
       (str/split-lines)
       (map str/trim)
       (map #(str/replace % #"\|" ""))
       (map #(str/split % #"\s+"))))

(defn solve-fiver
  [segment & {:keys [top-left right-side]}]
  (cond
    (set/superset? segment right-side) 3
    (set/superset? segment top-left) 5
    :else 2))

(defn solve-sixer
  [segment & {:keys [top-middle-bottom left-side]}]
  (cond
    (not (set/superset? segment top-middle-bottom)) 0
    (set/superset? segment left-side) 6
    :else 9))

(defn solve-segment
  [segment opts]
  (case (count segment)
    2 1
    3 7
    4 4
    5 (solve-fiver segment opts)
    6 (solve-sixer segment opts)
    7 8))

(defn segments-with-counts
  [counts segments]
  (into #{} (filter #(counts (count %))) segments))

(defn solve
  [segments]
  (let [fivers (segments-with-counts #{5} segments)
        unique (segments-with-counts #{2 3 4 7} segments)
        four (first (segments-with-counts #{4} segments))
        ;; with the above three data points we can know the following chunks
        ;; of segments
        top-middle-bottom (apply set/intersection (map set fivers))
        right-side (apply set/intersection (map set unique))
        left-side (set/difference (set "abcdefg") (set/union top-middle-bottom right-side))
        top-left (set/difference four top-middle-bottom right-side)]
    (map #(solve-segment (set %)
                         {:top-middle-bottom top-middle-bottom
                          :top-left top-left
                          :left-side left-side
                          :right-side right-side})
         segments)))

(comment
  (->> example
       (parse-input)
       (map solve)
       (map #(take-last 4 %))
       (map str/join)
       (map parse-long)
       (reduce + 0)))
