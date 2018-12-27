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

def init
  input = File.read("../inputs/day13.txt").lines

  init_trains = [] of Train

  colors = [31,32,33,34,35,36,37,90,91,92,93,94,95,96].shuffle.cycle


  circuit = input.each_with_index.map do |(line, y)|
    line.chars.each_with_index.map do |(c, x)|
      case c
      when '>'
        init_trains << {x, y, 1, 0, crossings, [{x,y}], colors.next.to_s}
        '-'
      when '<'
        init_trains << {x, y, -1, 0, crossings, [{x,y}], colors.next.to_s}
        '-'
      when 'v'
        init_trains << {x, y, 0, 1, crossings, [{x,y}], colors.next.to_s}
        '|'
      when '^'
        init_trains << {x, y, 0, -1, crossings, [{x,y}], colors.next.to_s}
        '|'
      else c
      end
    end.to_a
  end.to_a

  {circuit, init_trains}
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

    if with_trace
      trace.each do |x, y|
        state[y][x] = "\e[#{color}m#{state[y][x]}\e[39m"
      end
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


circuit, trains = init
# puts render(circuit, trains)

loop do
  trains.sort_by! {|t| [t[1], t[0]]}
  trains.each_with_index {|t, i|
    trains[i] = move(circuit, t)
    break if crash?(trains)
  }
  break if crash?(trains)
end

# puts render(circuit, crash_location(trains).values.first)

crash_x, crash_y = crash_location(trains).keys.first

puts "Day 01 - Part 1: #{crash_x},#{crash_y}"

circuit, trains = init

trains = Array(Train | Nil).new(trains.size) {|i| trains[i] }.shuffle

loop do
  trains.sort_by! {|t| t ? [t[1], t[0]] : [0,0]}

  (0 ... trains.size).each do |i|
    t = trains[i]
    if t
      new_t = move(circuit, t)
      trains[i] = new_t
      if crash?(trains.compact)
        (0 ... trains.size).each do |i2|
          t2 = trains[i2]
          if t2 && t2[0] == new_t[0] && t2[1] == new_t[1]
            trains[i2] = nil
          end
        end
      end
    end
  end
  trains.compact!

  break if trains.size == 1
end

last_train = trains.compact.first

puts "Day 01 - Part 2: #{last_train[0]},#{last_train[1]}"
