(ns day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example
  "start-A
   start-b
   A-c
   A-b
   b-d
   A-end
   b-end")
(def example2
  "dc-end
   HN-start
   start-kj
   dc-start
   dc-HN
   LN-dc
   HN-end
   kj-sa
   kj-HN
   kj-dc")
(def example3
  "fs-end
   he-DX
   fs-he
   start-DX
   pj-DX
   end-zg
   zg-sl
   zg-pj
   pj-he
   RW-he
   fs-DX
   pj-RW
   zg-RW
   start-pj
   he-WI
   zg-he
   pj-fs
   start-RW")
(def problem (slurp (io/resource "day12.txt")))

(defn parse-graph
  [input]
  (-> input
      (->> (str/split-lines)
           (map str/trim)
           (reduce (fn [acc line]
                     (let [[a b] (str/split line #"-")]
                       (-> acc
                           (update a (fnil conj []) b)
                           (update b (fnil conj []) a))))
                   {}))
      (dissoc "end")
      (update-vals #(remove #{"start"} %))))

(defn large?
  [node]
  (= node (str/upper-case node)))

(defn able-to-visit?
  [visits node]
  (or (large? node)
      (nil? (get visits node))
      (every? #(= 1 %) (vals visits))))

(defn paths
  ([graph]
   (paths graph "start" {}))
  ([graph node visits]
   (if (= "end" node)
     [[node]]
     (for [child (not-empty (get graph node))
           :when (able-to-visit? visits child)
           path (paths graph child (cond-> visits
                                     (not (large? child))
                                     (update child (fnil inc 0))))]
       (cons node path)))))

(comment
  (->> example
       (parse-graph)
       (paths)
       (count)))
