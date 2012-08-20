REBAR = rebar
clean:
	$(REBAR) clean

compile:
	$(REBAR) compile

test: clean
	$(REBAR) -C rebar.test.config eunit

devel: clean compile
	erl -pa ebin -config caterpillar.config -eval "application:start(caterpillar)."
