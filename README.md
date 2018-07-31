# HTTPS Everywhere Else

HTTPS Everywhere Else aims to bring the same protections that HTTPS Everywhere
gives you in the browser to other applications on your computer.

## Benchmarks

URLs can be scanned, looked up and rewritten in less than 0.15ms.

```
$ go test ./rules -bench=.
BenchmarkRulemapApply-4   	   10000	    137766 ns/op
```

The proxy handles thousands of requests per second.

```
$ go run dummy/dummy.go &
$ go run hee-proxy/proxy.go &
$ ab -t5 -X 127.0.0.1:8080 http://127.0.0.1:1111/
Requests per second:    3642.66 [#/sec] (mean)
Time per request:       0.271 [ms] (mean)
```
