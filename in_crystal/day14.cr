input = "286051"
input_int = input.to_i
input_digits = input.chars.map(&.to_i)

elf_1 = 0
elf_2 = 1
board = [3, 7]

loop do
  # board_s = board.map(&.to_s)
  # board_s[elf_1] = "("+board_s[elf_1]+")"
  # board_s[elf_2] = "["+board_s[elf_2]+"]"
  # puts board_s.join(" ")

  elf_1_val = board[elf_1]
  elf_2_val = board[elf_2]
  val = elf_1_val + elf_2_val

  digits = (val >= 10 ? [1, val % 10] : [val])

  digits.each do |d|
    board << d
  end

  l = board.size

  elf_1 = (1 + elf_1 + elf_1_val) % l
  elf_2 = (1 + elf_2 + elf_2_val) % l

  break if l > (10 + input_int)
end

part1 = board[input_int .. input_int + 9].map(&.to_s).join("")

puts "Day 14 - Part 1: #{part1}"

part2 = 0

loop do
  # board_s = board.map(&.to_s)
  # board_s[elf_1] = "("+board_s[elf_1]+")"
  # board_s[elf_2] = "["+board_s[elf_2]+"]"
  # puts board_s.join(" ")

  elf_1_val = board[elf_1]
  elf_2_val = board[elf_2]
  val = elf_1_val + elf_2_val

  digits = (val >= 10 ? [1, val % 10] : [val])

  digits.each do |d|
    board << d
  end

  l = board.size

  elf_1 = (1 + elf_1 + elf_1_val) % l
  elf_2 = (1 + elf_2 + elf_2_val) % l

  if board.size < input_digits.size + 1
    :recur
  elsif board[-input_digits.size .. -1] == input_digits
    part2 = board.size - input_digits.size
    break
  elsif board[(-input_digits.size - 1) .. -2] == input_digits
    part2 = board.size - input_digits.size - 1
    break
  else
    :recur
  end
end

puts "Day 14 - Part 2: #{part2}"
