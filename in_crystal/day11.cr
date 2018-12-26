def input
  7689
end

def grid_size
  300
end

def power (serial, x, y)
  rack_id = x + 10
  (((((rack_id * y) + serial) * rack_id) % 1000) / 100) - 5
end

grid = (0..grid_size).map { |y|
  (0..grid_size).map { |x|
    power(input, x, y)
  }
}

def best_square (size : Int32, g : Array(Array(Int32))) : { {Int32, Int32}, Int32}
  best = 0
  location = {0,0}
  (0..(grid_size - size)).each do |y|
    (0..(grid_size - size)).each do |x|
      s = 0
      (0..(size - 1)).each do |dx|
        (0..(size - 1)).each do |dy|
          s += g[y + dy][x + dx]
        end
      end
      if s > best
        best = s
        location = {x, y}
      end
    end
  end
  {location, best}
end

def best_of_all_sizes (grid : Array(Array(Int32)))
  top = 0
  location_and_size = {0,0,0}
  (1..20).each do |size|
    location, score = best_square(size, grid)
    if score > top
      top = score
      location_and_size = {location[0],location[1],size}
    end
  end
  location_and_size
end

location, score = best_square(3, grid)
x, y = location
puts "Day 11 - Part 1: #{x},#{y}"

x, y, s = best_of_all_sizes(grid)
puts "Day 11 - Part 2: #{x},#{y},#{s}"
