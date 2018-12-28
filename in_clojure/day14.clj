(ns day14
  (:require [clojure.string :as str]))

(def input 286051)

(def part-1
  (loop [elf-1 0, elf-2 1, board [3 7]]
    (let [elf-1-val (board elf-1)
          elf-2-val (board elf-2)

          val (+ elf-1-val elf-2-val)

          digits (if (>= val 10) [1 (mod val 10)] [val])

          board (apply conj board digits)

          l (count board)
          elf-1-pos (mod (+ 1 elf-1 elf-1-val) l)
          elf-2-pos (mod (+ 1 elf-2 elf-2-val) l)]

      (if (> l (+ 10 input))
        (apply str (take 10 (drop input board)))
        (recur elf-1-pos
               elf-2-pos
               board)))))

(println "Day 14 - Part 1:" part-1)

(def part-2
  (let [input-digits (map (comp read-string str) (str input))
        n-digits (count input-digits)]
    (loop [elf-1 0, elf-2 1, board [3 7]]
      (let [elf-1-val (board elf-1)
            elf-2-val (board elf-2)

            val (+ elf-1-val elf-2-val)

            digits (if (>= val 10) [1 (mod val 10)] [val])

            board (apply conj board digits)

            l (count board)
            elf-1-pos (mod (+ 1 elf-1 elf-1-val) l)
            elf-2-pos (mod (+ 1 elf-2 elf-2-val) l)]

        (cond
          (= (subvec board (max 0 (- l n-digits 1)) (- l 1))
             input-digits)
          (- l n-digits 1)

          (= (subvec board (max 0 (- l n-digits)) l)
             input-digits)
          (- l n-digits)

          :else
          (recur elf-1-pos
                 elf-2-pos
                 board))))))

(println "Day 14 - Part 2:" part-2)
