package main

import (
	"fmt"
	"github.com/Jell/advent2018/in_go/utils"
	"io/ioutil"
	"strconv"
	"strings"
)

func input() []int {
	raw, err := ioutil.ReadFile("../../inputs/day01.txt")
	utils.Check(err)

	var lines = strings.Split(string(raw), "\n")
	lines = lines[:len(lines)-1]

	var diffs []int
	for _, line := range lines {
		i, err := strconv.Atoi(line)
		utils.Check(err)
		diffs = append(diffs, i)
	}
	return diffs
}
func part1() {
	var freq = 0
	for _, i := range input() {
		freq = freq + i
	}
	fmt.Print("Day 1 - part 1: ", freq, "\n")
}

func part2() {
	var freq = 0
	var seen = map[int]bool{}
	var input = input()

	for !seen[freq] {
		for _, i := range input {
			seen[freq] = true
			freq += i
		}
	}
	fmt.Print("Day 1 - part 2: ", freq, "\n")
}

func main() {
	part1()
	part2()
}
