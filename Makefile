REBAR = rebar
clean:
	$(REBAR) clean

compile:
	$(REBAR) compile

test: clean
	$(REBAR) -C rebar.test.config eunit
