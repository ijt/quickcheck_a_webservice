test: kv_store/kv_store restdict.beam test_kv_store.beam
	kv_store/kv_store &
	erl -noshell -eval 'proper:module(test_kv_store)' -s init stop
	killall kv_store

clean:
	rm *.beam

restdict.beam: restdict.erl
	erlc $<
	
test_kv_store.beam: test_kv_store.erl
	erlc $<

kv_store/kv_store: kv_store/main.go
	cd kv_store && go build

