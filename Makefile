REBAR = rebar
clean:
	$(REBAR) clean

compile:
	$(REBAR) compile

test: clean
	$(REBAR) -C rebar.test.config eunit

devel: clean compile
	ERL_LIBS="../../smprc-logging/trunk/lib:../../smprc-cowboy/trunk/lib"\
			 erl -pa ebin -config test.config -eval "application:start(caterpillar)."
