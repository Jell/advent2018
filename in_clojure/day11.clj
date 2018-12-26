(ns day01
  (:require [clojure.string :as str]))

(def input 7689)

(defn power [serial x y]
  (let [rack-id (+ x 10)]
    (-> (* rack-id y)
        (+ serial)
        (* rack-id)
        (mod 1000)
        (quot 100)
        (- 5))))

(def grid-size 300)

(def grid
  (vec
   (for [y (range grid-size)]
     (vec
      (for [x (range grid-size)]
        (power input x y))))))

(def power-squares
  (for [x (range (- grid-size 2))
        y (range (- grid-size 2))]
    [[x, y]
     (reduce + (for [vx (range 3)
                     vy (range 3)]
                 (get-in grid [(+ y vy) (+ x vx)])))]))

(println "Day 11 - Part 1:" (->> power-squares
                                 (sort-by second)
                                 (last)
                                 (first)
                                 (str/join ",")))

(comment
  ;; Too slow!
  (def power-squares-any-size
    (for [s (range grid-size)
          x (range (- grid-size s))
          y (range (- grid-size s))]
      [[x, y, s]
       (reduce + (for [vx (range s)
                       vy (range s)]
                   (get-in grid [(+ y vy) (+ x vx)])))]))


  (println "Day 11 - Part 2:" (->> power-squares-any-size
                                   (sort-by second)
                                   (last)
                                   (first)
                                   (str/join ","))))
