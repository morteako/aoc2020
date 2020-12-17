# Advent of Code 2020 

My Haskell solutions for https://adventofcode.com/2020


Can read input from stdin, a file or fetched from adventofcode.com/DAY/input (saved into a file, to avoid spamming the server).
Can choose day from cmdargs.
  

## Days completed

- [x] [Day 1](https://github.com/morteako/aoc2020/blob/main/src/Day/Day01.hs)
- [x] [Day 2](https://github.com/morteako/aoc2020/blob/main/src/Day/Day02.hs)
- [x] [Day 3](https://github.com/morteako/aoc2020/blob/main/src/Day/Day03.hs)
- [x] [Day 4](https://github.com/morteako/aoc2020/blob/main/src/Day/Day04.hs)
- [x] [Day 5](https://github.com/morteako/aoc2020/blob/main/src/Day/Day05.hs)
- [x] [Day 6](https://github.com/morteako/aoc2020/blob/main/src/Day/Day06.hs)
- [x] [Day 7](https://github.com/morteako/aoc2020/blob/main/src/Day/Day07.hs)
- [x] [Day 8](https://github.com/morteako/aoc2020/blob/main/src/Day/Day08.hs)
- [x] [Day 9](https://github.com/morteako/aoc2020/blob/main/src/Day/Day09.hs)
- [x] [Day 10](https://github.com/morteako/aoc2020/blob/main/src/Day/Day10.hs)
- [x] [Day 11](https://github.com/morteako/aoc2020/blob/main/src/Day/Day11.hs)
- [x] [Day 12](https://github.com/morteako/aoc2020/blob/main/src/Day/Day12.hs)
- [x] [Day 13](https://github.com/morteako/aoc2020/blob/main/src/Day/Day13.hs)
- [x] [Day 14](https://github.com/morteako/aoc2020/blob/main/src/Day/Day14.hs)
- [x] [Day 15](https://github.com/morteako/aoc2020/blob/main/src/Day/Day15.hs)
- [x] [Day 16](https://github.com/morteako/aoc2020/blob/main/src/Day/Day16.hs)
- [x] [Day 17](https://github.com/morteako/aoc2020/blob/main/src/Day/Day17.hs)
- [ ] [Day 18](https://github.com/morteako/aoc2020/blob/main/src/Day/Day18.hs)
- [ ] [Day 19](https://github.com/morteako/aoc2020/blob/main/src/Day/Day19.hs)
- [ ] [Day 20](https://github.com/morteako/aoc2020/blob/main/src/Day/Day20.hs)
- [ ] [Day 21](https://github.com/morteako/aoc2020/blob/main/src/Day/Day21.hs)
- [ ] [Day 22](https://github.com/morteako/aoc2020/blob/main/src/Day/Day22.hs)
- [ ] [Day 23](https://github.com/morteako/aoc2020/blob/main/src/Day/Day23.hs)
- [ ] [Day 24](https://github.com/morteako/aoc2020/blob/main/src/Day/Day24.hs)
- [ ] [Day 25](https://github.com/morteako/aoc2020/blob/main/src/Day/Day25.hs)



## Usage

```
Usage: <interactive> [--day DAY] [--stdin | --file FILENAME]
  Run a advent of code challenge. Default is to run the last implemented
  challenge and fetch the corresponding input

Available options:
  --day DAY                Run challenge for the provided day
  --stdin                  Read from stdin
  --file FILENAME          Read from input file
  -h,--help                Show this help text
```


### Build

`stack build`

### Run

From terminal : `stack exec aoc2020 -- [cmdargs]`

Interactive :  `stack ghci` and then `:main [cmdargs]`

