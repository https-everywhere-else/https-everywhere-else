package main

import "github.com/yorickvP/https-everywhere-else"

import "bufio"
import "fmt"
import "os"

func main() {
	rulemap, err := rules.Load()
	if err != nil {
		panic(err)
	}
	input := "blah"
	reader := bufio.NewReader(os.Stdin)
	for {
		line, _, err := reader.ReadLine()
		input = string(line)
		if err != nil {
			break
		}
		output, err := rulemap.Apply(input)
		if err != nil {
			fmt.Printf("%v\n", err)
		}
		fmt.Printf("%v\n", output)
	}
}
