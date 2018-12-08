(ns day05
  (:require [clojure.string :as str]))

(def input "dabAcCaCBAcCcaDA")
(def input (str/trim (slurp "../inputs/day05.txt")))

(def alphabet "abcdefghijklmnopqrstuvwxyz")
(def ALPHABET "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def reaction-regex
  (->> (map str alphabet ALPHABET)
       (concat (map str ALPHABET alphabet))
       (str/join "|")
       re-pattern))

(defn react [polymer]
  (loop [x ^String polymer]
    (let [x2 (str/replace x reaction-regex "")]
      (if (= (.length x2) (.length x))
        x
        (recur x2)))))

(def initial-reaction (react input))

(def part-1 (.length initial-reaction))

(println "Day 05 - Part 1:" part-1)

(def part-2
  (->> alphabet
       (pmap (fn [c]
               (-> initial-reaction
                   (str/replace (str/lower-case c) "")
                   (str/replace (str/upper-case c) "")
                   react
                   count)))
       (apply min)))

(println "Day 05 - Part 2:" part-2)

;; Needed with pmap
(shutdown-agents)
