# HTTPS Everywhere Else

This project aims to do what HTTPS Everywhere does for the browser, for all
other HTTP connections instead.

## Running



## Benchmarks

Our current benchmarks show that applying the rules on arbitrary test urls costs
less than 0.15ms per url.

```
$ go test ./rules -bench=.
BenchmarkRulemapApply-4   	   10000	    131756 ns/op
```

We've also benchmarked the proxy
