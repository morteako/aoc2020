# Advent of Code 2020 

My Haskell solutions for https://adventofcode.com/2020


Can read input from stdin, a file or fetched from adventofcode.com/DAY/input (saved into a file, to avoid spamming the server).
Can choose day from cmdargs.
  

## Days completed

- [ ] [Day 1](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day01.hs)
- [ ] [Day 2](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day02.hs)
- [ ] [Day 3](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day03.hs)
- [ ] [Day 4](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day04.hs)
- [ ] [Day 5](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day05.hs)
- [ ] [Day 6](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day06.hs)
- [ ] [Day 7](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day07.hs)
- [ ] [Day 8](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day08.hs)
- [ ] [Day 9](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day09.hs)
- [ ] [Day 10](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day10.hs)
- [ ] [Day 11](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day11.hs)
- [ ] [Day 12](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day12.hs)
- [ ] [Day 13](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day13.hs)
- [ ] [Day 14](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day14.hs)
- [ ] [Day 15](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day15.hs)
- [ ] [Day 16](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day16.hs)
- [ ] [Day 17](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day17.hs)
- [ ] [Day 18](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day18.hs)
- [ ] [Day 19](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day19.hs)
- [ ] [Day 20](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day20.hs)
- [ ] [Day 21](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day21.hs)
- [ ] [Day 22](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day22.hs)
- [ ] [Day 23](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day23.hs)
- [ ] [Day 24](https://github.com/morteako/aoc2020/blob/main/rc/Day/Day24.hs)



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

