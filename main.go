package main

import "encoding/xml"
import "io/ioutil"
import "strings"
import "regexp"
import "bufio"
import "fmt"
import "os"

var generalizeRegex = regexp.MustCompile(`^(\*\.)?[^.]+(.|$)`)
var domainRegex = regexp.MustCompile(`^(?:https?://)?(?:[^@/]+@)?([^/:?]+)`)

type Target struct {
	Host string `xml:"host,attr"`
}

type Rule struct {
	From string `xml:"from,attr"`
	To   string `xml:"to,attr"`
}

type Ruleset struct {
	Name    string   `xml:"name,attr"`
	Targets []Target `xml:"target"`
	Rules   []Rule   `xml:"rule"`
}

type Rulemap map[string][]Rule

func loadXML(data []byte) (ruleset Ruleset, err error) {
	err = xml.Unmarshal(data, &ruleset)
	if err != nil {
		return
	}
	return
}

func load() (Rulemap, error) {
	files, err := ioutil.ReadDir("rules")
	if err != nil {
		return nil, err
	}

	rulemap := make(Rulemap)

	for _, file := range files {
		if !strings.HasSuffix(file.Name(), ".xml") {
			continue
		}
		data, err := ioutil.ReadFile("rules/" + file.Name())
		if err != nil {
			return nil, err
		}
		ruleset, err := loadXML(data)
		if err != nil {
			return nil, err
		}
		for _, target := range ruleset.Targets {
			rulemap[target.Host] = ruleset.Rules
		}
	}
	return rulemap, nil
}

func extractDomain(url string) (string, error) {
	groups := domainRegex.FindStringSubmatch(url)
	if len(groups) < 2 {
		return "", fmt.Errorf("domain not found in %v", url)
	} else {
		return groups[1], nil
	}
}

func (rulemap *Rulemap) lookup(domain string) []Rule {
	for domain != "*." {
		if rules, ok := (*rulemap)[domain]; ok {
			return rules
		}
		domain = generalizeRegex.ReplaceAllString(domain, "*.")
	}
	return nil
}

func (rulemap *Rulemap) applyHTTPS(url string) (string, error) {
	domain, err := extractDomain(url)
	if err != nil {
		return "", err
	}
	for _, rule := range rulemap.lookup(domain) {
		regex, err := regexp.Compile(rule.From)
		if err != nil {
			return "", err
		}
		url = regex.ReplaceAllString(url, rule.To)
	}
	return url, nil
}

func main() {
	rulemap, err := load()
	if err != nil {
		panic(err)
	}
	fmt.Printf("Loaded.\n")
	input := "blah"
	reader := bufio.NewReader(os.Stdin)
	for {
		line, _, err := reader.ReadLine()
		input = string(line)
		if err != nil {
			break
		}
		output, err := rulemap.applyHTTPS(input)
		if err != nil {
			fmt.Printf("%v\n", err)
		}
		fmt.Printf("%v\n", output)
	}
}
