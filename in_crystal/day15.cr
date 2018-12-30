module Foo
  extend self

  input = File.read("../inputs/day15_short_1.txt").lines

  alias Coord = {Int32, Int32}

  @@walls = Set(Coord).new
  @@creatures = Hash(Coord, {Char, Int32}).new

  input.each_with_index do |line, y|
    line.chars.each_with_index do |c, x|
      case c
      when 'G' then @@creatures[{x,y}] = {'G', 200}
      when 'E' then @@creatures[{x,y}] = {'E', 200}
      when '#' then @@walls << {x,y}
      end
    end
  end

  def render
    width = @@walls.map(&.first).max
    height = @@walls.map(&.last).max

    puts (0..height).map { |y|
      (0..width).map { |x|
        if @@walls.includes?({x,y})
          '#'
        else
          @@creatures[{x,y}]? && @@creatures[{x,y}].first || '.'
        end
      }.join("")
    }.join("\n")
  end

  def is_there?(kind, coord)
    @@creatures[coord]? && @@creatures[coord].first == kind
  end

  def search (kind, paths : Array(Array(Coord))) : Array(Coord)
    if paths.empty?
      [] of Coord
    else
      seen = paths.flatten.to_set

      path = paths.shift
      x, y = path.last
      [
        {x, y - 1},
        {x - 1, y},
        {x + 1, y},
        {x, y + 1},
      ].each do |coord|
        if is_there?(kind, coord)
          return path
        end

        unless @@walls.includes?(coord) || seen.includes?(coord) || (@@creatures[coord]?)
          paths << path + [coord]
        end
      end

      search(kind, paths)
    end
  end

  def hps (creatures)
    creatures.map {|k,v| v[1] }.sum
  end

  def run
    render

    i = 0
    loop do
      @@creatures.to_a.sort_by {|k, v| k.reverse }.each do |coord, stats|
        kind, life = stats
        x, y = coord
        enemy = (kind == 'G' ? 'E' : 'G')
        path = search(enemy, [[coord]])

        unless path.empty? || (path[1]?).nil?
          x, y = path[1]
          @@creatures.delete(coord)
          @@creatures[path[1]] = stats
        end

        in_range = [] of {Coord, {Char, Int32}}
        [
          {x, y - 1},
          {x - 1, y},
          {x + 1, y},
          {x, y + 1},
        ].each do |enemy_coord|
          if (@@creatures[enemy_coord]?) && @@creatures[enemy_coord][0] == enemy
            in_range << {enemy_coord, @@creatures[enemy_coord]}
          end
        end

        in_range.sort_by! {|c,v| v[1] }

        unless in_range.empty?
          enemy_coord, enemy_stats = in_range.first
          enemy_kind, enemy_life = enemy_stats
          if enemy_life <= 3
            @@creatures.delete(enemy_coord)
          else
            @@creatures[enemy_coord] = {enemy_kind, enemy_life - 3}
          end
        end
      end

      i += 1
      puts i
      puts @@creatures
      render

      if @@creatures.map {|k,v| v[0] }.uniq.size == 1
        break
      end
    end

    puts hps(@@creatures) * i
  end
end

Foo.run
