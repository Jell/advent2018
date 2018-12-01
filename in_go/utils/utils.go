package utils

import (
	"io/ioutil"
	"strings"
)

// Check ...
// panic if error
func Check(err error) {
	if err != nil {
		panic(err)
	}
}

// ReadFile ...
func ReadFile(path string) string {
	raw, err := ioutil.ReadFile("../../inputs/day01.txt")
	Check(err)
	return string(raw)
}

// ReadLines ...
func ReadLines(path string) []string {
	var lines = strings.Split(ReadFile(path), "\n")
	return lines[:len(lines)-1]
}
