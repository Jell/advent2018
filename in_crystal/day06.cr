input = File.read("../inputs/day06.txt").lines.map { |l|
  l.split(", ").map(&.to_i)
}

height = input.map { |(_,y)| y}.max
width = input.map { |(x,_)| x}.max

graph = [] of Array(Tuple(String, Int32))

(height + 1).times do
  graph << [{"x", height + width}]* (width + 1)
end

def update (graph, id, x, y, dist)
  cursor = graph[y][x]
  if cursor[1] == dist
    unless cursor[0] == id
      graph[y][x] = {".", dist}
    end
    true
  elsif cursor[1] > dist
    graph[y][x] = {id, dist}
    true
  else
    false
  end
end

input.each_with_index do |(x, y), i|
  id = i.to_s
  (0..x).each do |x_offset|
    (0..y).each do |y_offset|
      dist = x_offset + y_offset
      unless update(graph, id, x - x_offset, y - y_offset, dist)
        break
      end
    end
    (0..(height - y)).each do |y_offset|
      dist = x_offset + y_offset
      unless update(graph, id, x - x_offset, y + y_offset, dist)
        break
      end
    end
  end

  (0..(width - x)).each do |x_offset|
    (0..y).each do |y_offset|
      dist = x_offset + y_offset
      unless update(graph, id, x + x_offset, y - y_offset, dist)
        break
      end
    end
    (0..(height - y)).each do |y_offset|
      dist = x_offset + y_offset
      unless update(graph, id, x + x_offset, y + y_offset, dist)
        break
      end
    end
  end
end

on_edge = (["."] +
           graph.first.map(&.first) +
           graph.last.map(&.first) +
           graph.map(&.first).map(&.first) +
           graph.map(&.last).map(&.first)).uniq

closed_surfaces = graph.flat_map { |row|
  row.map(&.first)
} - on_edge

part_1 = closed_surfaces.group_by(&.itself).map {|k, v| v.size }.max

puts "Day 06 - Part 1: #{part_1}"

part_2 =  (0..width).flat_map { |x|
  (0..height).map { |y|
    input.reduce(0) { |dist, (x_p, y_p)|
      dist + (x_p - x).abs + (y_p - y).abs
    }
  }
}.count {|d| d < 10_000 }

puts "Day 06 - Part 2: #{part_2}"
