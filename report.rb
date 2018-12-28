# -*- coding: utf-8 -*-

langs = Dir.glob('in_*').map { |d| d.gsub("in_", "") }.sort

solutions = {
  "Day 01" => { part1: "599", part2: "81204" },
  "Day 02" => { part1: "6370", part2: "rmyxgdlihczskunpfijqcebtv" },
  "Day 03" => { part1: "118223", part2: "412" },
  "Day 04" => { part1: "103720", part2: "110913" },
  "Day 05" => { part1: "9462", part2: "4952" },
  "Day 06" => { part1: "3722", part2: "44634" },
  "Day 07" => { part1: "OCPUEFIXHRGWDZABTQJYMNKVSL", part2: "992" },
  "Day 08" => { part1: "44893", part2: "27433" },
  "Day 09" => { part1: "402398", part2: "3426843186" },
  "Day 10" => { part1: [
    "",
    "######  #####     ##    #####   #    #  #    #     ###   #### ",
    "     #  #    #   #  #   #    #  #    #  #    #      #   #    #",
    "     #  #    #  #    #  #    #   #  #    #  #       #   #     ",
    "    #   #    #  #    #  #    #   #  #    #  #       #   #     ",
    "   #    #####   #    #  #####     ##      ##        #   #     ",
    "  #     #  #    ######  #    #    ##      ##        #   #     ",
    " #      #   #   #    #  #    #   #  #    #  #       #   #     ",
    "#       #   #   #    #  #    #   #  #    #  #   #   #   #     ",
    "#       #    #  #    #  #    #  #    #  #    #  #   #   #    #",
    "######  #    #  #    #  #####   #    #  #    #   ###     #### ",
  ].join("\n"), part2: "10710" },
  "Day 11" => { part1: "20,37", part2: "90,169,15" },
  "Day 12" => { part1: "2840", part2: "2000000001684" },
  "Day 13" => { part1: "46,18", part2: "124,103" },
}

results = {}
langs.each do |lang|
  puts "## #{lang}"
  results[lang] = `cd in_#{lang} && make 2> /dev/null`
  puts results[lang]
end

table = [
  [""] + langs,
  [":---:"] * (langs.count + 1)
] + solutions.map do |day, parts|
  [day] + langs.map do |lang|
    if results[lang].include?("#{day} - Part 1: #{parts.fetch(:part1)}")
      "*"
    else
      ""
    end +
    if results[lang].include?("#{day} - Part 2: #{parts.fetch(:part2)}")
      "*"
    else
      ""
    end
  end
end

report = table.map { |line| "|" + line.map { |c| c.ljust(10) }.join("|") + "|" }.join("\n")

puts ""
puts ""
puts "Progress:"
puts ""
puts report

readme = File.read("README.md")
readme[/\|.*\|/m] = report.tr("*","⭐")
File.write("README.md", readme)
