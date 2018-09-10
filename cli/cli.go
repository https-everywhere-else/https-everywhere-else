package main

import "github.com/https-everywhere-else/https-everywhere-else/rules"

import "bufio"
import "fmt"
import "os"

func main() {
	rulemap, err := rules.Load("vendor/https-everywhere/rules")
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
