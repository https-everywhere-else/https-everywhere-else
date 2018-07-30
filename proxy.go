package main

import "net/http"
import "log"
import "io"

type Proxy struct {
	rulemap *Rulemap
}

func copyHeaders(dst, src http.Header) {
	for key, values := range src {
		for _, value := range values {
			dst.Set(key, value)
		}
	}
}

func (p Proxy) ServeHTTP(srcWriter http.ResponseWriter, srcRequest *http.Request) {
	newURL, err := p.rulemap.Apply(srcRequest.RequestURI)
	if err != nil {
		log.Println(err)
		http.Error(srcWriter, "", http.StatusInternalServerError)
		return
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
	rulemap, err := load()
	if err != nil {
		panic(err)
	}
	log.Println("loaded")
	proxy := Proxy{&rulemap}
	err = http.ListenAndServe(":1111", proxy)
	if err != nil {
		panic(err)
	}
}
