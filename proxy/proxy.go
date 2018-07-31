package main

import "github.com/yorickvP/https-everywhere-else/rules"

import "net/http"
import "log"
import "io"
import "os"

type Proxy struct {
	rulemap *rules.Rulemap
	info    *log.Logger
}

func copyHeaders(dst, src http.Header) {
	for key, values := range src {
		for _, value := range values {
			dst.Set(key, value)
		}
	}
}

func (p Proxy) ServeHTTP(srcWriter http.ResponseWriter, srcRequest *http.Request) {
	p.info.Printf("%v %v\n", srcRequest.Method, srcRequest.RequestURI)
	newURL, err := p.rulemap.Apply(srcRequest.RequestURI)
	if err != nil {
		log.Println(err)
		http.Error(srcWriter, "", http.StatusInternalServerError)
		return
	}
	if newURL != srcRequest.RequestURI {
		p.info.Printf("-> %v\n", newURL)
	}

	tgtRequest, err := http.NewRequest(srcRequest.Method, newURL, srcRequest.Body)
	if err != nil {
		log.Println(err)
		http.Error(srcWriter, "", http.StatusInternalServerError)
		return
	}

	copyHeaders(tgtRequest.Header, srcRequest.Header)
	client := &http.Client{}
	tgtResponse, err := client.Do(tgtRequest)
	srcRequest.Body.Close()
	if err != nil {
		log.Println(err)
		http.Error(srcWriter, "", http.StatusInternalServerError)
		return
	}

	copyHeaders(srcWriter.Header(), tgtResponse.Header)
	srcWriter.WriteHeader(tgtResponse.StatusCode)
	_, err = io.Copy(srcWriter, tgtResponse.Body)
	if err != nil {
		log.Println(err)
	}
	tgtResponse.Body.Close()
}

func main() {
	rulemap, err := rules.Load("vendor/https-everywhere/rules")
	if err != nil {
		panic(err)
	}
	log.Println("loaded")
	proxy := Proxy{&rulemap, log.New(os.Stderr, "I ", log.LstdFlags)}
	err = http.ListenAndServe(":8080", proxy)
	if err != nil {
		panic(err)
	}
}
