class Day15
  alias Coord = {Int32, Int32}

  def initialize (path, elf_power = 3, raise_on_elf_death = false, render_all_steps = false)
    input = File.read(path).lines

    @elf_power = elf_power
    @walls = Set(Coord).new
    @creatures = Hash(Coord, {Char, Int32}).new
    @current = {0,0}
    @strike = {0,0}
    @raise_on_elf_death = raise_on_elf_death
    @render_all_steps = render_all_steps

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

    result = (0..height).map { |y|
      on_line = [] of { {Int32, Int32}, {Char, Int32} }
      " "*3 + (0..width).map { |x|
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
      }.join("") + " "*3 + on_line.map {|c, (k, hp)|
        if @strike == c
          red("#{k}(#{hp})")
        else
          "#{k}(#{hp})"
        end
      }.join(", ").ljust(60)
    }.join("\n\r")

    3.times { puts }
    puts result
    3.times { puts }
    puts "\033[;H"
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
          other_path = search(kind, paths.select {|p| p.size <= path.size })
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

  def run
    if @render_all_steps
      puts "\033c"
      puts "\u001B[?25l"
      render
    end

    i = 0
    loop do
      stop = false

      @creatures.to_a.sort_by {|k, v| k.reverse }.each do |coord, stats|
        @current = coord
        @strike = nil

        # might have died since the start!
        next unless @creatures[coord]?

        if @creatures.map {|k,v| v[0] }.uniq.size == 1
          stop = true
          break
        end

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

        in_range = in_range.sort_by! {|c,v| {v[1], c.reverse} }

        unless in_range.empty?
          enemy_coord, enemy_stats = in_range.first
          enemy_kind, enemy_life = enemy_stats
          power = kind == 'G' ? 3 : @elf_power
          @strike = enemy_coord
          if enemy_life <= power
            if enemy_kind == 'E' && @raise_on_elf_death
              raise "abandon timeline"
            end
            @creatures.delete(enemy_coord)
          else
            @creatures[enemy_coord] = {enemy_kind, enemy_life - power}
          end
        end

        if @render_all_steps
          puts
          render
          sleep(0.1)
          # gets
        end
      end
      @current = nil
      @strike = nil

      i += 1

      if @render_all_steps
        puts i
        render
        # gets
      end

      break if stop
    end

    score = hps(@creatures) * (i - 1)

    if @render_all_steps
      puts "Done after #{i} steps. Score: #{score}. [CONTINUE]"
      puts "\u001B[?25l"
      gets
      puts "\033[2J"
    end

    score
  end
end

# Day15.new("../inputs/day15_short_1.txt", render_all_steps: true).run
# Day15.new("../inputs/day15_short_2.txt", render_all_steps: true).run
# Day15.new("../inputs/day15_short_3.txt", render_all_steps: true).run
# Day15.new("../inputs/day15_short_4.txt", render_all_steps: true).run
# Day15.new("../inputs/day15_short_5.txt", render_all_steps: true).run
# Day15.new("../inputs/day15_short_6.txt", render_all_steps: true).run
# Day15.new("../inputs/day15_ambiguous.txt", render_all_steps: true).run
part1 = Day15.new("../inputs/day15.txt", render_all_steps: false).run
puts "Day 15 - Part 1: #{part1}"

timeline = nil
file = "../inputs/day15.txt"
part2 = nil
(4..200).each do |i|
  begin
    timeline = Day15.new(file, elf_power: i, raise_on_elf_death: true)
    part2 = timeline.run
    break
  rescue e
  end
end

puts "Day 15 - Part 2: #{part2}"
