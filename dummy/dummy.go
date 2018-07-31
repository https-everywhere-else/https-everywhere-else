package main

import "net/http"
import "flag"
import "log"

func handler(w http.ResponseWriter, r *http.Request) {
	w.Write([]byte(r.RequestURI))
}

func main() {
	port := flag.String("port", "1111", "Port to listen on")
	flag.Parse()
	http.HandleFunc("/", handler)
	log.Fatal(http.ListenAndServe(":"+*port, nil))
}
