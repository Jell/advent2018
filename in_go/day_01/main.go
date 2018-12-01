package main

import (
	"fmt"
	"github.com/Jell/advent2018/in_go/utils"
)

func input() []int {
	var lines = utils.ReadLines("../../inputs/day01.txt")

	var diffs []int
	for _, line := range lines {
		diffs = append(diffs, utils.StrToInt(line))
	}
	return diffs
}

func part1(diffs []int) {
	var freq = 0
	for _, i := range diffs {
		freq = freq + i
	}
	fmt.Print("Day 1 - part 1: ", freq, "\n")
}

func part2(diffs []int) {
	var freq int
	var found bool
	var seen = map[string]bool{}
	for !found {
		for _, i := range diffs {
			freq = freq + i
			found = seen[utils.IntToStr(freq)]
			if found {
				break
			}
			seen[utils.IntToStr(freq)] = true
		}
	}
	fmt.Print("Day 1 - part 2: ", freq, "\n")
}

func main() {
	var diffs = input()
	part1(diffs)
	part2(diffs)
}
