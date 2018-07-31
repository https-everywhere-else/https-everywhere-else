package rules

import "math/rand"
import "io/ioutil"
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
	data, err := ioutil.ReadFile("../tests.xml")
	if err != nil {
		log.Panic(err)
	}

	ruleset, err := loadXML(data)
	if err != nil {
		log.Panic(err)
	}

	return ruleset.Tests
}

var rulemap = loadRulemap()
var tests = loadTests()

func BenchmarkRulemapApply(b *testing.B) {
	for i := 0; i < b.N; i++ {
		rulemap.Apply(tests[rand.Intn(len(tests))].URL)
	}
}

func TestRulemapApply(t *testing.T) {
	for _, test := range tests {
		newurl, err := rulemap.Apply(test.URL)
		if err != nil {
			t.Error(err)
		}

		if newurl == test.URL && test.Rewrite {
			t.Errorf("Did not change: %s", test.URL)
		} else if newurl != test.URL && !test.Rewrite {
			t.Errorf("Changed: %s TO %s", test.URL, newurl)
		}
	}
}
