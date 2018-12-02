import { readFileSync } from 'fs';

let input: number[] = readFileSync('../inputs/day01.txt', "utf8").
    split("\n").
    map(i => parseInt(i)).
    slice(0, -1);

let part_1 = input.reduce((a, i) => a + i)
console.log("Day 01 - Part 1:", part_1)

var found = false;
var freq: number = 0;
var seen = new Set<number>([]);

while (!found) {
    for (let i of input) {
        freq += i;
        if (seen.has(freq)) {
            found = true;
            break;
        } else {
            seen.add(freq);
        }
    }
}

console.log("Day 01 - Part 2:", freq)
