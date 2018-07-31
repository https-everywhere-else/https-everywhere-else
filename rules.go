package rules

// TODO: Consider mmapped succinct tries
// TODO: Cache using encoding/gob for faster startup
// TODO: Benchmark
// TODO: Tests
// TODO: Compile regexes at startup
// TODO: Figure out if securecookies are applicable

import "encoding/xml"
import "io/ioutil"
import url_ "net/url"
import "strings"
import "regexp"
import "log"

var generalizeRegex = regexp.MustCompile(`^(\*\.)?[^.]+(\.|$)`)
var generalizeRegex2 = regexp.MustCompile(`\.[^.]+$`)

type Target struct {
	Host string `xml:"host,attr"`
}

type Test struct {
	URL string `xml:"url,attr"`
}

type Rule struct {
	From string `xml:"from,attr"`
	To   string `xml:"to,attr"`
}

type Exclusion struct {
	Pattern string `xml:"pattern,attr"`
}

type Ruleset struct {
	Name       string      `xml:"name,attr"`
	Exclusions []Exclusion `xml:"exclusion"`
	Targets    []Target    `xml:"target"`
	Rules      []Rule      `xml:"rule"`
	Tests      []Test      `xml:"test"`
}

type Rulemap map[string]Ruleset

func loadXML(data []byte) (ruleset Ruleset, err error) {
	err = xml.Unmarshal(data, &ruleset)
	if err != nil {
		return
	}
	return
}

func Load() (Rulemap, error) {
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
			rulemap[target.Host] = ruleset
		}
	}
	return rulemap, nil
}

func (ruleset *Ruleset) excludes(url string) bool {
	for _, excl := range ruleset.Exclusions {
		regex, err := regexp.Compile(excl.Pattern)
		if err != nil {
			log.Println(err)
			return false
		}
		if regex.MatchString(url) {
			return true
		}
	}
	return false
}

func (rulemap *Rulemap) lookup(key, url string) (*Ruleset, bool) {
	ruleset, ok := (*rulemap)[key]
	if !ok {
		return nil, false
	}
	if ruleset.excludes(url) {
		return nil, false
	}
	return &ruleset, true
}

func (rulemap *Rulemap) Get(url string) (*Ruleset, error) {
	urlObject, err := url_.Parse(url)
	if err != nil {
		return nil, err
	}

	domain := urlObject.Host
	attempt := domain
	for attempt != "*." && attempt != "" {
		if ruleset, ok := rulemap.lookup(attempt, url); ok {
			return ruleset, nil
		}
		attempt = generalizeRegex.ReplaceAllString(attempt, "*.")
	}

	attempt = generalizeRegex2.ReplaceAllString(domain, ".*")
	for attempt != "*.*" && attempt != "*." && attempt != "" {
		if ruleset, ok := rulemap.lookup(attempt, url); ok {
			return ruleset, nil
		}
		attempt = generalizeRegex.ReplaceAllString(attempt, "*.")
	}
	return nil, nil
}

func (ruleset *Ruleset) Apply(url string) (string, error) {
	for _, rule := range ruleset.Rules {
		regex, err := regexp.Compile(rule.From)
		if err != nil {
			return "", err
		}
		newurl := regex.ReplaceAllString(url, rule.To)
		if newurl != url {
			return newurl, nil
		}
	}
	return url, nil
}

func (rulemap *Rulemap) Apply(url string) (string, error) {
	ruleset, err := rulemap.Get(url)
	if err != nil {
		return "", err
	}
	if ruleset == nil {
		return url, nil
	}
	return ruleset.Apply(url)
}
