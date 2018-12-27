require 'set'

input = File.read("../inputs/day12.txt").lines

$padding = 100

initial_state = "." * $padding + input.first[/[#.]+/] + "." * $padding

$alive = Set.new(
  input.drop(2).grep(/=> #/).map { |l| l.split(" => ").first.chars }
)

def next_step (state)
  ('..' + state + '..').chars.each_cons(5).map do |s|
    $alive.include?(s) ? "#" : "."
  end.join("")
end

def steps (n, state)
  (1..n).reduce(state) { |s, _|
    next_step(s)
  }
end

def pot_count (state)
  state.chars.each_with_index.map { |c, i|
    c == "#" ? i - $padding : 0
  }.reduce(&:+)
end

part1 = pot_count(steps(20, initial_state))

puts "Day 12 - Part 1: #{part1}"

patterns = Set.new([initial_state[/#.+#/]])

repeat_index, repeat_pattern = (1..200).reduce(initial_state) { |state, i|
  s = next_step(state)
  pattern = s[/#.+#/]
  if patterns.include?(pattern)
    break [i, s]
  end
  patterns << pattern
  s
}

repeat_count = pot_count(repeat_pattern)

count_diff = pot_count(next_step(repeat_pattern)) - repeat_count

index_diff = 50000000000 - repeat_index

part2 = repeat_count + index_diff * count_diff

puts "Day 12 - Part 2: #{part2}"
