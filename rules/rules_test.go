package rules

import "math/rand"
import "testing"
import "log"

var rulesPath = "../vendor/https-everywhere/rules/"

func loadRulemap() Rulemap {
	rulemap, err := Load(rulesPath)
	if err != nil {
		log.Panic(err)
	}
	return rulemap
}

func loadTests() []Test {
	var tests []Test
	for _, ruleset := range rulemap {
		tests = append(tests, ruleset.Tests...)
	}
	return tests
}

var rulemap = loadRulemap()
var tests = loadTests()

func BenchmarkRulemapApply(b *testing.B) {
	for i := 0; i < b.N; i++ {
		rulemap.Apply(tests[rand.Intn(len(tests))].URL)
	}
}
