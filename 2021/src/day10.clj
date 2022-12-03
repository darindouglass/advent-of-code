(ns day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example
  "[({(<(())[]>[[{[]{<()<>>
   [(()[<>])]({[<{<<[]>>(
   {([(<{}[<>[]}>{[]{[(<()>
   (((({<>}<{<{<>}{[]{[]{}
   [[<[([]))<([[{}[[()]]]
   [{[{({}]{}}([{[{{{}}([]
   {<[[]]>}<{[{[{[]{()[[[]
   [<(<(<(<{}))><([]([]()
   <{([([[(<>()){}]>(<<{{
   <{([{{}}[<[[[<>{}]]]>[]]")
(def problem (slurp (io/resource "day10.txt")))

(def delimiters
  {\[ \]
   \{ \}
   \( \)
   \< \>})

(def opener? (comp some? delimiters))

(defn parse-input
  [input]
  (map str/trim (str/split-lines input)))

(defn paired?
  [opener char]
  (= char (delimiters opener)))

(defn categorize
  [line]
  (let [val (reduce
             (fn [[opener :as stack] char]
               (cond
                 (opener? char)
                 (conj stack char)

                 (paired? opener char)
                 (pop stack)

                 :else
                 (reduced
                  {:current-opener opener
                   :invalid-character char})))
             '()
             line)]
    (if (list? val)
      {:line line
       :stack val}
      val)))

(defn corrupted?
  [v]
  (nil? (:stack v)))

(defn calc-corrupted
  [corrupted-maps]
  (let [score {\) 3
               \] 57
               \} 1197
               \> 25137}]
    (->> corrupted-maps
         (map :invalid-character)
         (frequencies)
         (map (fn [[char num]] (* num (score char))))
         (reduce + 0))))

(defn calc-completed
  [{:keys [stack]}]
  (let [score {\) 1
               \] 2
               \} 3
               \> 4}]
    (->> stack
         (map delimiters)
         (reduce #(+ (score %2) (* 5 %1)) 0))))

(defn middle
  [coll]
  (nth coll (quot (count coll) 2)))

(comment
  (->> example
       (parse-input)
       (map categorize)
       (filter corrupted?)
       (calc-corrupted))
  (->> example
       (parse-input)
       (map categorize)
       (remove corrupted?)
       (map calc-completed)
       (sort)
       (middle)))
