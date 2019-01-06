(ns day18
  (:require [clojure.string :as str]))

(def input
  (->> "../inputs/day18.txt"
       (slurp)
       (str/split-lines)
       (mapv vec)))

(def input-width
  (count (first input)))

(def input-height
  (count input))

(defn neighbors [board x y]
  (remove nil?
          (for [dy (range -1 2)
                dx (range -1 2)
                :when (not= [0,0] [dx,dy])]
            (get-in board [(+ y dy) (+ x dx)]))))

(defn step [board]
  (vec
   (for [y (range input-height)]
     (vec
      (for [x (range input-width)]
        (let [t (get-in board [y x])
              ns (neighbors board x y)]
          (case t
            \. (if (<= 3 (count (filter #{\|} ns)))
                 \| \.)
            \| (if (<= 3 (count (filter #{\#} ns)))
                 \# \|)
            \# (if (and (some #{\#} ns)
                        (some #{\|} ns))
                 \# \.))))))))

(defn render [board]
  (println
   (str "\n\n"
        (str/join "\n" (map str/join board)))))

(def after-10-min
  (nth (iterate step input) 10))

(defn score [board]
  (* (count (filter #{\|} (flatten board)))
     (count (filter #{\#} (flatten board)))))

(def part-1
  (score after-10-min))

(println "Day 14 - Part 1:" part-1)

(def first-loop
  (loop [i 0 b input indices {input 0}]
    (let [i (inc i)
          b2 (step b)]
      (if (indices b2)
        [(indices b2) i]
        (recur i b2 (assoc indices b2 i))))))

(def long-time 1000000000)

(def part-2
  (score
   (nth
    (iterate step input)
    (let [[start end] first-loop]
      (+ start
         (mod (- long-time start)
              (- end start)))))))

(println "Day 14 - Part 2:" part-2)
