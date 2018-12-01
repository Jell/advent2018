require 'set'

input = File.read("../inputs/day01.txt").lines.map(&:to_i)

part1 = input.reduce(&:+)

puts "Day 1 - part 1: #{part1}"

part2 = input.cycle.reduce([0, Set.new]) do |(freq, seen), i|
  freq += i
  seen.include?(freq) ? (break freq) : [freq, seen << freq]
end

puts "Day 1 - part 2: #{part2}"
