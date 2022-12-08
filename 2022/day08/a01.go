package main

import (
    "fmt"
    "log"
    "os"
    "bufio"
    "math"
)

func createGraph(filename string) [][]int {
    var graph [][]int
    var row int

    readFile, err := os.Open("simple.txt")
    if err != nil {
        log.Fatal(err)
    }
    fileScanner := bufio.NewScanner(readFile)
    fileScanner.Split(bufio.ScanLines)
    for fileScanner.Scan() {
        line := fileScanner.Text()
        if graph == nil {
            graph = make([][]int, len(line))
            row = 0
            for i := range graph {
                graph[i] = make([]int, len(line))
            }
        }
        for i := range line {
            graph[row][i] = int (line[i] - '0')
        }
        row++ //took a while to find that I am never incrementing the row
    }

    return graph
}

func checkSourounding (graph [][]int, row int, col int, currTree int, dir []int) int {
    l := len(graph)
    nextRow := row + dir[0]
    nextCol := col + dir[1]

    //end 
    if nextRow >= l || nextRow < 0 || nextCol >= l || nextCol < 0 {
        return 1
    }
    //found taller tree
    if graph[nextRow][nextCol] >= currTree {
        return 0
    }

    return checkSourounding(graph, nextRow, nextCol, currTree, dir)
}

func score (graph [][]int, row int, col int, currTree int, dir []int, scoreT int) int {
    l := len(graph)
    nextRow := row + dir[0]
    nextCol := col + dir[1]

    if nextRow >= l || nextRow < 0 || nextCol >= l || nextCol < 0 {
        return 0
    }
    //found better tree
    if graph[nextRow][nextCol] >= currTree {
        return scoreT
    }

    return scoreT + score(graph, nextRow, nextCol, currTree, dir, 1)
}

func part1(graph [][]int, row int, col int, currTree int, dirArray [][]int) int {
    for k := range dirArray {
        count := checkSourounding(graph, row, col, currTree, dirArray[k])
        if count == 1 {
            return 1
        }
    }
    return 0
}

func part2(graph [][]int, row int, col int, currTree int, dirArray [][]int) int {
    count := 1
    for k := range dirArray {
        scoreT := score(graph, row, col, currTree, dirArray[k], 1)
        count *= scoreT
    }
    return count
}

func main() {

    graph := createGraph("input.txt")

    visible := len(graph[0])*4-4 //visible at start (all outside trees)
    dirArray := [][]int{{-1,0}, {0,1}, {1,0}, {0,-1}}
    max := math.MinInt
    for i:=1; i < len(graph)-1; i++ {
        for j:=1; j < len(graph[i])-1; j++ {
            currTree := graph[i][j]
            visible += part1(graph, i, j, currTree, dirArray)
            see := part2(graph, i, j, currTree, dirArray)
            if see > max {
                max = see
            }
        }
    }

    fmt.Println("part1: ", visible)
    fmt.Println("part2: ", max)
}
