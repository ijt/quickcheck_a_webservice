.PHONY: test
test: kv_store/kv_store kv_store.beam test_kv_store.beam
	kv_store/kv_store &
	erl -noshell -eval 'proper:module(test_kv_store)' -s init stop
	killall kv_store

.PHONY: clean
clean:
	rm *.beam

kv_store.beam: kv_store.erl
	erlc $<
	
test_kv_store.beam: test_kv_store.erl
	erlc $<

kv_store/kv_store: kv_store/main.go
	cd kv_store && go build

