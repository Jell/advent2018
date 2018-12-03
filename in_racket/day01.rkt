#lang racket
(require threading)

(define input
  (~>> "../inputs/day01.txt"
       file->lines
       (map string->number)))

(define part-1
  (foldl + 0 input))

(displayln (format "Day 01 - Part 1: ~a" part-1))
