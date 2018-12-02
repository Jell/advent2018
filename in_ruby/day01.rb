require 'set'

input = File.read("../inputs/day01.txt").lines.map(&:to_i)

part1 = input.reduce(&:+)

puts "Day 01 - Part 1: #{part1}"

part2 = input.cycle.reduce([0, Set.new]) do |(freq, seen), i|
  freq += i
  seen.include?(freq) ? (break freq) : [freq, seen << freq]
end

puts "Day 01 - Part 2: #{part2}"
