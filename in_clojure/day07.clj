(ns day07
  (:require [clojure.string :as str]
            [clojure.set :refer [map-invert]]))

(def input
  (->> "../inputs/day07.txt"
       (slurp)
       (str/split-lines)
       (map (partial re-matches #"Step (.) must be finished before step (.) can begin."))
       (map (partial rest))))

(def nodes
  (->> input
       (apply concat)
       (set)
       (sort)) )

(def empty-graph
  (->> nodes
       (map #(vector % #{}))
       (into {})))

(def graph
  (reduce (fn [graph [before after]]
            (update graph after conj before))
          empty-graph
          input))

(defn free-nodes [g]
  (->> g
       (filter #(= (second %) #{}))
       (map first)
       (set)))

(defn update-map [m f]
  (reduce-kv
   (fn [m k v]
     (assoc m k (f v))) {} m))

(def part-1
  (loop [g graph output ""]
    (if (seq g)
      (let [node (first (sort (free-nodes g)))]
        (recur
         (update-map
          (dissoc g node)
          (fn [v] (disj v node)))
         (str output node)))
      output)))

(println "Day 07 - Part 1:" part-1)


(defn update-wip [wip ns]
  (reduce
   (fn [w n]
     (update w n (fn [i] (if (nil? i)
                           (+ 60 (.indexOf nodes n))
                           (dec i)))))
   wip ns))

(def workers 5)

(def part-2
  (loop [g graph timer 1 wip {} output []]
    (if (or (seq g) (seq wip))
      (let [ns (concat
                (keys wip)
                (take (- workers (count wip))
                      (remove (set (keys wip))
                              (sort (free-nodes g)))))
            wip (update-wip wip ns)
            done (->> wip
                      (filter (fn [[k v]] (= 0 v)))
                      (map first))]
        (if (seq done)
          (recur
           (update-map
            (apply dissoc g done)
            (fn [v] (apply disj v done)))
           (inc timer)
           (apply dissoc wip done)
           (apply conj output done))
          (recur g (inc timer) wip output)))
      timer)))

(println "Day 07 - Part 2:" part-2)
