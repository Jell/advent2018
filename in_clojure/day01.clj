(ns day01
  (:require [clojure.string :as str]))

(def input
  (->> "../inputs/day01.txt"
       (slurp)
       (str/split-lines)
       (map read-string)))

(->> input
     (reduce +)
     (println "Day 1 - Part 1: "))

(->> input
     (cycle)
     (reductions +)
     (reduce (fn [seen new]
               (if (contains? seen new)
                 (reduced new)
                 (conj seen new)))
             #{})
     (println "Day 1 - Part 2: "))
