package main

import (
	"fmt"
	"log"
	"net/http"
	"strings"
)

var dict = make(map[string]string)

func main() {
	handleFunc("/put/", handlePut)
	handleFunc("/get/", handleGet)
	handleFunc("/erase/", handleErase)
	http.ListenAndServe(":1234", nil)
}

func handleFunc(path string, handler func(http.ResponseWriter, []string) (error, int)) {
	handler2 := func(w http.ResponseWriter, r *http.Request) {
		argsPart := r.URL.Path[len(path):]
		args := strings.Split(argsPart, "/")
		err, code := handler(w, args)
		if err != nil {
			log.Printf("Request failed. %v", err)
			http.Error(w, fmt.Sprintf("Request failed. %v", err), code)
		}
	}
	http.HandleFunc(path, handler2)
}

func handlePut(w http.ResponseWriter, args []string) (error, int) {
	key := args[0]
	val := args[1]
	oldVal, hasKey := dict[key]
	if hasKey {
		w.Write([]byte(oldVal))
	}
	dict[key] = val
	return nil, 200
}

func handleGet(w http.ResponseWriter, args []string) (error, int) {
	key := args[0]
	val := dict[key]
	w.Write([]byte(val))
	return nil, 200
}

func handleErase(w http.ResponseWriter, args []string) (error, int) {
	key := args[0]
	oldVal := dict[key]
	delete(dict, key)
	w.Write([]byte(oldVal))
	return nil, 200
}
