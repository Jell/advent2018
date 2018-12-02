(ns day02
  (:require [clojure.string :as str]
            [clojure.set :refer [map-invert]]))

(def input
  (->> "../inputs/day02.txt"
       (slurp)
       (str/split-lines)))

(defn row-checksum [row]
  (->> row
       (sort)
       (partition-by identity)
       (map count)
       (set)))

(defn flip-select-keys [ks m]
  (select-keys m ks))

(->> input
     (mapcat row-checksum)
     (group-by identity)
     (map (fn [[k v]] [k (count v)]))
     (into {})
     (flip-select-keys #{2 3})
     (vals)
     (apply *)
     (println "Day 02 - Part 1:"))

(defn one-less-options [row]
  (let [c (count row)]
    (vec
     (for [i (range c)]
       (str (subs row 0 i)
            (subs row (inc i) c))))))

(defn transpose [m]
  (vec
   (for [j (range (count (first m)))]
     (vec
      (for [i (range (count m))]
        (get-in m [i j]))))))

(defn flip-find [e c]
  (find c e))

(defn paired-match [rows]
  (->> rows
       (group-by identity)
       (map (fn [[k v]] [k (count v)]))
       (into {})
       (map-invert)
       (flip-find 2)
       second))

(->> input
     (mapv one-less-options)
     (transpose)
     (some paired-match)
     (println "Day 02 - Part 2:"))
