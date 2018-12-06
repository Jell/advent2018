(ns day03
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [s]
  (->> s
       (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
       (drop 1)
       (map read-string)
       (map vector [:id :x :y :w :h])
       (into {})))

(defn surface [{:keys [id x y w h]}]
  (for [i (range x (+ x w))
        j (range y (+ y h))]
    [id i j]))

(defn claim [fabric row]
  (reduce (fn [f [id i j]]
            (update f [i j] (fnil #(conj % id) #{})))
          fabric
          (surface row)))

(def suit
  (->> "../inputs/day03.txt"
       (slurp)
       (str/split-lines)
       (map parse-line)
       (reduce claim {})
       (vals)))

(def all-ids (reduce set/union suit))

(->> suit
     (map count)
     (filter #(>= % 2))
     (count)
     (println "Day 03 - Part 1:"))

(->> suit
     (filter #(> (count %) 1))
     (reduce set/difference all-ids)
     (first)
     (println "Day 03 - Part 2:"))
