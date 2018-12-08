input = File.read("../inputs/day05.txt")

alphabet = ("a" .. "z").to_a
ALPHABET = ("A" .. "Z").to_a

reaction_regexp = Regexp.new(
  (
    alphabet.zip(ALPHABET) + ALPHABET.zip(alphabet)
  ).map { |c1,c2| c1 + c2 }.join("|")
)

loop do
  input[reaction_regexp] = ""
end rescue input.size
