#lang racket
(require threading)
(require racket/set)

(define input
  (~>> "../inputs/day01.txt"
       file->lines
       (map string->number)))

(define part-1
  (foldl + 0 input))

(displayln (format "Day 01 - Part 1: ~a" part-1))

(define part-2
  (call/cc
   (λ (return)
     (let loop ([rows input] [seen (set)] [ freq 0 ])
       (if (empty? rows)
           (loop input seen freq)
           (let ([freq (+ (car rows) freq)])
             (if (set-member? seen freq)
                 (return freq)
                 (loop (cdr rows) (set-add seen freq) freq))))))))

(displayln (format "Day 01 - Part 2: ~a" part-2))
