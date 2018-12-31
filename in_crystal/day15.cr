class Day15
  alias Coord = {Int32, Int32}

  def initialize (path)
    input = File.read(path).lines

    @walls = Set(Coord).new
    @creatures = Hash(Coord, {Char, Int32}).new
    @current = {0,0}
    @strike = {0,0}

    input.each_with_index do |line, y|
      line.chars.each_with_index do |c, x|
        case c
        when 'G' then @creatures[{x,y}] = {'G', 200}
        when 'E' then @creatures[{x,y}] = {'E', 200}
        when '#' then @walls << {x,y}
        end
      end
    end
  end

  def red (s)
    "\e[31m#{s}\e[39m"
  end

  def green (s)
    "\e[32m#{s}\e[39m"
  end

  def grey (s)
    "\e[90m#{s}\e[39m"
  end


  def render
    width = @walls.map(&.first).max
    height = @walls.map(&.last).max

    puts (0..height).map { |y|
      on_line = [] of { {Int32, Int32}, {Char, Int32} }
      (0..width).map { |x|
        if @walls.includes?({x,y})
          grey("#")
        elsif (@creatures[{x,y}]?)
          on_line << { {x,y}, @creatures[{x,y}]}
          k, hp = @creatures[{x,y}]
          case
          when {x,y} == @current then green(k)
          when {x,y} == @strike then red(k)
          else k
          end
        else
          if ({x,y} == @current)
            green("~")
          elsif ({x,y} == @strike)
            red("X")
          else
            grey(".")
          end
        end
      }.join("") + "   " + on_line.map {|c, (k, hp)|
        if @strike == c
          red("#{k}(#{hp})")
        else
          "#{k}(#{hp})"
        end
      }.join(", ")
    }.join("\n")
  end

  def is_there?(kind, coord)
    @creatures[coord]? && @creatures[coord].first == kind
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
          other_path = search(kind, paths)
          if other_path.size == path.size && other_path.last.reverse < path.last.reverse
            return other_path
          else
            return path
          end
        end

        unless @walls.includes?(coord) || seen.includes?(coord) || (@creatures[coord]?)
          paths << path + [coord]
        end
      end

      search(kind, paths)
    end
  end

  def hps (creatures)
    creatures.map {|k,v| v[1] }.sum
  end

  def part1
    # render

    i = 0
    loop do
      stop = false

      @creatures.to_a.sort_by {|k, v| k.reverse }.each do |coord, stats|
        @current = coord
        @strike = nil

        if @creatures.map {|k,v| v[0] }.uniq.size == 1
          stop = true
          break
        end

        # might have died since the start!
        next unless @creatures[coord]?

        kind, life = stats
        x, y = coord
        enemy = (kind == 'G' ? 'E' : 'G')
        path = search(enemy, [[coord]])

        unless path.empty? || (path[1]?).nil?
          x, y = path[1]
          @creatures.delete(coord)
          @creatures[path[1]] = stats
        end

        in_range = [] of {Coord, {Char, Int32}}
        [
          {x, y - 1},
          {x - 1, y},
          {x + 1, y},
          {x, y + 1},
        ].each do |enemy_coord|
          if (@creatures[enemy_coord]?) && @creatures[enemy_coord][0] == enemy
            in_range << {enemy_coord, @creatures[enemy_coord]}
          end
        end

        in_range.sort_by! {|c,v| v[1] }

        unless in_range.empty?
          enemy_coord, enemy_stats = in_range.first
          enemy_kind, enemy_life = enemy_stats
          @strike = enemy_coord
          if enemy_life <= 3
            @creatures.delete(enemy_coord)
          else
            @creatures[enemy_coord] = {enemy_kind, enemy_life - 3}
          end
        end

        # render
        # gets
      end
      @current = nil
      @strike = nil

      i += 1
      # puts i
      # render
      # gets

      break if stop
    end

    # puts i
    # render
    part1 = hps(@creatures) * (i - 1)

    puts "Day 15 - Part 1: #{part1}"
  end
end

# Day15.new("../inputs/day15_short_1.txt").part1
# Day15.new("../inputs/day15_short_2.txt").part1
# Day15.new("../inputs/day15_short_3.txt").part1
# Day15.new("../inputs/day15_short_4.txt").part1
# Day15.new("../inputs/day15_short_5.txt").part1
# Day15.new("../inputs/day15_short_6.txt").part1
# Day15.new("../inputs/day15_ambiguous.txt").part1
Day15.new("../inputs/day15.txt").part1
