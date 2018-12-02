package main

import (
	"fmt"
	"github.com/Jell/advent2018/in_go/utils"
	"time"
)

func input() []string {
	return utils.ReadLines("../../inputs/day02.txt")
}

func part1(rows []string) {
	var twos int
	var threes int
	for _, row := range rows {
		var counts = map[rune]int{}
		for _, c := range row {
			counts[c]++
		}
		var hasTwos = false
		var hasThrees = false

		for _, v := range counts {
			if v == 2 {
				hasTwos = true
			}
			if v == 3 {
				hasThrees = true
			}
		}

		if hasTwos {
			twos++
		}
		if hasThrees {
			threes++
		}
	}
	fmt.Println("Day 02 - Part 01: ", twos*threes)
}

func oneLessOptions(row string) []string {
	var l = len(row)
	var options = []string{}
	for i := 0; i < l; i++ {
		options = append(options, row[0:i]+row[i+1:l])
	}
	return options
}

func part2(rows []string) {
	var matches = map[int]map[string]int{}
	var result = ""
	for _, row := range rows {
		for i, option := range oneLessOptions(row) {
			if matches[i] == nil {
				matches[i] = map[string]int{}
			}
			matches[i][option]++
			if matches[i][option] == 2 {
				result = option
				break
			}
		}
		if result != "" {
			break
		}
	}
	fmt.Println("Day 02 - Part 01: ", result)
}

func main() {
	rows := input()
	start := time.Now()
	part1(rows)
	part2(rows)
	t := time.Now()
	elapsed := t.Sub(start)
	fmt.Println(elapsed)
}
