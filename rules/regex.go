package rules

import "github.com/dlclark/regexp2"

const options = 0

func Match(pattern, source string) (bool, error) {
	regex, err := regexp2.Compile(pattern, options)
	if err != nil {
		return false, err
	}
	return regex.MatchString(source)
}

func Replace(from, to, source string) (string, error) {
	regex, err := regexp2.Compile(from, options)
	if err != nil {
		return "", err
	}
	return regex.Replace(source, to, -1, 1)
}
