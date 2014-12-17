all:
	./rebar get-deps
	./rebar compile

run:
	erl \
		-name jd \
		-pa `pwd`/apps/*/ebin \
		-pa `pwd`/deps/*/ebin \
		-eval "[application:start(A) || A <- []]"

doc:
	./rebar skip_deps=true doc

eunit:
	./rebar skip_deps=true eunit

clean:
	./rebar clean

