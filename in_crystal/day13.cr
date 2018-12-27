input = File.read("../inputs/day13.txt").lines

enum Crossing
  Left
  Straight
  Right
end

alias Crossings = Iterator(Crossing)
alias Train = {Int32, Int32, Int32, Int32, Crossings, Array({Int32, Int32}), String}

def crossings
  [Crossing::Left, Crossing::Straight, Crossing::Right].cycle
end

trains = [] of Train

colors = [31,32,33,34,35,36,37,90,91,92,93,94,95,96].shuffle.cycle

circuit = input.each_with_index.map do |(line, y)|
  line.chars.each_with_index.map do |(c, x)|
    case c
    when '>'
      trains << {x, y, 1, 0, crossings, [{x,y}], colors.next.to_s}
      '-'
    when '<'
      trains << {x, y, -1, 0, crossings, [{x,y}], colors.next.to_s}
      '-'
    when 'v'
      trains << {x, y, 0, 1, crossings, [{x,y}], colors.next.to_s}
      '|'
    when '^'
      trains << {x, y, 0, -1, crossings, [{x,y}], colors.next.to_s}
      '|'
    else c
    end
  end.to_a
end.to_a

class Any
  def self.===(other)
    true
  end
end

def render (circuit, trains, with_trace = false)
  state = circuit.clone.map { |l| l.map(&.to_s)}
  trains.each do |x,y,dx,dy,_,trace,color|
    if state[y][x] =~ /[<>v^X]/
      state[y][x] = "X"
    else
      state[y][x] =
        case [dx, dy]
        when [1, 0] then ">"
        when [-1, 0] then "<"
        when [0, 1] then "v"
        when [0, -1] then "^"
        else "X"
        end
    end

    trace.each do |x, y|
      state[y][x] = "\e[#{color}m#{state[y][x]}\e[39m"
    end
  end
  state.map { |l| l.join("") }.join("\n")
end

def move (circuit, train)
  x, y, dx, dy, cs, trace, color = train

  x = x + dx
  y = y + dy

  trace << {x, y}

  if circuit[y].nil?
    raise "out of bound #{y}"
  elsif circuit[y][x].nil?
    raise "out of bound #{ {x, y} }"
  else
    location = circuit[y][x]
  end

  dx, dy =
      case {location, dx, dy}

      when {'-', 1, 0} then {1, 0}
      when {'-',-1, 0} then {-1, 0}
      when {'|', 0, 1} then { 0,1}
      when {'|', 0,-1} then { 0,-1}

      when {'\\', 1, 0} then { 0, 1}
      when {'\\',-1, 0} then { 0,-1}
      when {'\\', 0,-1} then {-1, 0}
      when {'\\', 0, 1} then { 1, 0}

      when {'/', 1, 0} then { 0,-1}
      when {'/',-1, 0} then { 0, 1}
      when {'/', 0,-1} then { 1, 0}
      when {'/', 0, 1} then {-1, 0}

      when {'+', 1, 0}
        case cs.next
        when Crossing::Left     then { 0,-1}
        when Crossing::Right    then { 0, 1}
        when Crossing::Straight then { 1, 0}
        else raise "x"
        end

      when {'+', -1, 0}
        case cs.next
        when Crossing::Left  then    { 0, 1}
        when Crossing::Right then    { 0,-1}
        when Crossing::Straight then {-1, 0}
        else raise "x"
        end

      when {'+', 0, -1}
        case cs.next
        when Crossing::Left  then    {-1, 0}
        when Crossing::Right then    { 1, 0}
        when Crossing::Straight then { 0,-1}
        else raise "x"
        end

      when {'+', 0, 1}
        case cs.next
        when Crossing::Left  then    { 1, 0}
        when Crossing::Right then    {-1, 0}
        when Crossing::Straight then { 0, 1}
        else raise "x"
        end

      else raise "error: #{ {circuit[y][x],x,y,dx,dy} }"
      end

  {x, y, dx, dy, cs, trace, color}
end

def crash? (trains)
  (Set.new trains.map { |x,y,_,_,_,_,_| {x, y}}).size != trains.size
end

def crash_location (trains)
  trains.group_by {|x,y,_,_,_,_,_| {x, y}}.select { |k, v| v.size > 1 }
end

puts render(circuit, trains)

loop do
  trains.each_with_index {|t, i|
    trains[i] = move(circuit, t)
    break if crash?(trains)
  }
  break if crash?(trains)
end

puts render(circuit, crash_location(trains).values.first)

puts trains.size
puts crash_location(trains)

crash_x, crash_y = crash_location(trains).keys.first

puts "Day 01 - Part 1: #{crash_x},#{crash_y}"
