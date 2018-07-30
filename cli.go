package main

import "bufio"
import "fmt"
import "os"

func main() {
	rulemap, err := load()
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
		output, err := rulemap.apply(input)
		if err != nil {
			fmt.Printf("%v\n", err)
		}
		fmt.Printf("%v\n", output)
	}
}
