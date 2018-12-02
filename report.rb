# -*- coding: utf-8 -*-

langs = Dir.glob('in_*').map { |d| d.gsub("in_", "") }

solutions = {
  "Day 01" => { part1: "599", part2: "81204" },
  "Day 02" => { part1: "6370", part2: "rmyxgdlihczskunpfijqcebtv" }
}

results = {}
langs.map do |lang|
  results[lang] = `cd in_#{lang} && make 2> /dev/null`
end

star = "⭐"

table = [
  ["", ""] + langs + [""],
  [""] + [":---:"] * (langs.count + 1) + [""]
] + solutions.map do |day, parts|
  ["", day] + langs.map do |lang|
    if results[lang].include?("#{day} - Part 1: #{parts.fetch(:part1)}")
      star
    else
      ""
    end +
    if results[lang].include?("#{day} - Part 2: #{parts.fetch(:part2)}")
      star
    else
      ""
    end
  end + [""]
end

report = table.map { |line| line.map { |c| c.ljust(10) }.join("|") }.join("\n")

puts report

readme = File.read("README.md")
readme[/\s{10}\|.*\|/m] = report
File.write("README.md", readme)
