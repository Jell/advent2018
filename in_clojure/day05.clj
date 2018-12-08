(ns day05
  (:require [clojure.string :as str]))

(def input (str/trim (slurp "../inputs/day05.txt")))

(def alphabet "abcdefghijklmnopqrstuvwxyz")
(def ALPHABET "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def reactive
  (zipmap (str alphabet ALPHABET)
          (str ALPHABET alphabet)))

(defn react [input]
  (if-let [c1 (first input)]
    (if-let [next-step (react (rest input))]
      (if-let [c2 (first next-step)]
        (if (= c2 (reactive c1))
          (rest next-step)
          (cons c1 next-step))
        (cons c1 next-step))
      (list c1))
    (list)))

(def initial-reaction (react input))

(def part-1 (count initial-reaction))

(println "Day 05 - Part 1:" part-1)

(def part-2
  (->> alphabet
       (pmap (fn [c]
               (->> initial-reaction
                    (remove #{(first (str/lower-case c))
                              (first (str/upper-case c))})
                    react
                    count)))
       (apply min)))

(println "Day 05 - Part 2:" part-2)

;; Needed with pmap
(shutdown-agents)
