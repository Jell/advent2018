input = File.read("../inputs/day01.txt").lines.map(&.to_i)

part1 = input.sum

puts "Day 1 - part 1: #{part1}"

part2 = input.cycle.reduce({0, Set(Int32).new}) do |(freq, seen), i|
  freq += i
  seen.includes?(freq) ? break {freq, seen} : {freq, seen << freq}
end.first

puts "Day 1 - part 2: #{part2}"
