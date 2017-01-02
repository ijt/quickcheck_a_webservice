Example of testing a web service with PropEr (QuickCheck)
=========================================================

This project shows how a web service written in Go can be tested using a clone
of Erlang's QuickCheck package called PropEr. Before diving in, it's a good
idea to get a rough idea of how it works by having a look at this video:

	Testing the Hard Stuff and Staying Sane
	https://www.youtube.com/watch?v=zi0rHwfiX1Q

Some more introductory material can be found at
http://proper.softlab.ntua.gr/Tutorials/. To understand the code here, these
two pages are relevant:

	http://proper.softlab.ntua.gr/Tutorials/PropEr_introduction_to_Property-Based_Testing.html
	http://proper.softlab.ntua.gr/Tutorials/PropEr_testing_of_generic_servers.html

To get started, first make sure Erlang and Go are both installed.  Once that's
done, install PropEr. I recommend my fork because it pretty-prints the output.

	git clone https://github.com/ijt/proper
	pushd proper
	make
	export ERL_LIBS=`pwd`
	popd

Back in the quickcheck\_webservice directory

	make test

The tests should pass.
Now let's try introducing a bug into the web service and seeing if it's
detected.

	sed -i 's#delete#// delete#' kv_store/main.go
	git diff
	make test

The test should fail with a shrunken example like this:

	Shrinking .(1 time(s))
	[{set,{var,1},{call,kv_store,put,[a,'0']}},
	 {set,{var,3},{call,kv_store,erase,[a]}},
	 {set,{var,4},{call,kv_store,put,[a,'0']}}]
	History: [{[],''},{[{a,'0'}],'0'},{[],'0'}]
	State: []
	Res: {postcondition,false}

That means that calling the kv_store web service with 

	GET /put/a/0
	GET /erase/a
	GET /put/a/0

does something that violates one of the preconditions.
PropEr doesn't tell us which one that is, so we'll have
to find out on our own. So, let's fire up kv_store and
see what happens when we hit it with this combination
of calls:

	kv_store/kv_store &
	kvs=localhost:1234
	curl $kvs/put/a/0  # ==> no output
	curl $kvs/erase/a  # ==> 0
	curl $kvs/put/a/0  # ==> 0 

That's funny: the last call to put returns 0 but it should behave just like the
first call to put. It looks like erase is failing to do it's job, as we were
pretending not to know.

