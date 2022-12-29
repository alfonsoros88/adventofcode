package main

import (
	"fmt"
	"os"
	"strings"
)

const (
	Rock     = 1
	Paper    = 2
	Scissors = 3
)

type Play (int)

func (p Play) win(other Play) bool {
	return (p == Rock && other == Scissors) ||
		(p == Paper && other == Rock) ||
		(p == Scissors && other == Paper)
}

func (p Play) score(other Play) int {
	if p == other {
		return 3
	}

	if p.win(other) {
		return 6
	} else {
		return 0
	}
}

func charToPlay(c byte) Play {
	switch c {
	case 'A':
		return Play(Rock)
	case 'X':
		return Play(Rock)
	case 'B':
		return Play(Paper)
	case 'Y':
		return Play(Paper)
	case 'C':
		return Play(Scissors)
	case 'Z':
		return Play(Scissors)
	default:
		panic("Invalid play")
	}
}

// ==== part 2 ====
const (
	Win  = 1
	Draw = 2
	Lose = 3
)

func charToResult(c byte) int {
	switch c {
	case 'X':
		return Win
	case 'Y':
		return Draw
	case 'Z':
		return Lose
	default:
		panic("Invalid result")
	}
}

func winAndLose(p Play) (Play, Play) {
	switch p {
	case Rock:
		return Play(Scissors), Play(Paper)
	case Paper:
		return Play(Rock), Play(Scissors)
	case Scissors:
		return Play(Paper), Play(Rock)
	default:
		panic("Invalid play")
	}
}

func getPlayFromResult(result int, other Play) Play {
	win, lose := winAndLose(other)
	switch result {
	case Win:
		return win
	case Draw:
		return other
	case Lose:
		return lose
	default:
		panic("Invalid result")
	}
}

func main() {
	argumentCount := len(os.Args)
	if argumentCount < 2 {
		panic("input file missing")
	}

	inputFile := os.Args[1]

	fileContent, err := os.ReadFile(inputFile)
	if err != nil {
		panic(err)
	}

	inputString := string(fileContent)
	lines := strings.Split(inputString, "\n")

	var score_part1 int = 0
	var score_part2 int = 0
	for _, line := range lines {
		if len(line) == 0 {
			break
		}

		otherPlay := charToPlay(line[0])
		myPlay := charToPlay(line[2])
		score_part1 += myPlay + myPlay.score(otherPlay)

		myPlay = getPlayFromResult(charToResult(line[2]), otherPlay)
		score_part2 += myPlay + myPlay.score(otherPlay)
	}

	fmt.Println("part1: ", score_part1)
	fmt.Println("part2: ", score_part2)
}
