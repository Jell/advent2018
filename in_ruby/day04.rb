input = File.read("../inputs/day04.txt").lines.sort

shifts = {}

input.slice_before(/Guard/).each do |shift|
  shift.first =~ /Guard #(\d+)/
  guard = $1.to_i

  shifts[guard] ||= []
  shifts[guard] += shift.drop(1).map { |line|
    line =~ /:(\d\d)\]/
    $1.to_i
  }.each_slice(2).flat_map { |(start, ending)|
    (start ... ending).to_a
  }
end

longestId = shifts.max_by { |k, v| v.count }.first

bestMinute = shifts.fetch(longestId).
  group_by(&:itself).
  max_by { |k, v| v.count }.
  first

puts "Day 04 - Part 1: #{longestId * bestMinute}"

bestId, (mostMinute, _) = shifts.reduce({}) { |accu, (guard, minutes)|
  accu.merge!(
    guard => minutes.group_by(&:itself).
      map { |k, v| [k, v.count] }.
      max_by { |_, v| v } || [0, 0]
  )
}.max_by { |_, (_, c)| c }

puts "Day 04 - Part 2: #{bestId * mostMinute}"
