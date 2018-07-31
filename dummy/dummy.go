package main

import "net/http"
import "log"

func handler(w http.ResponseWriter, r *http.Request) {
	w.Write([]byte(r.RequestURI))
}

func main() {
	http.HandleFunc("/", handler)
	log.Fatal(http.ListenAndServe(":1111", nil))
}
