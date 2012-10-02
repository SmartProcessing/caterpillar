include ../../devel-tools/trunk/Makefile.mk

LIB_PATH = "/var/lib/caterpillar/"
LOG_PATH = "/var/log/caterpillar/"
ETC_PATH = "/etc/caterpillar/"
REBAR = rebar
BEAMS = $(patsubst src/%.erl, ebin/%.beam, $(wildcard src/*.erl))
TEST_BEAMS = $(patsubst test_src/%.erl, ebin/%.beam, $(wildcard test_src/*.erl))

ifdef EXPORT_ALL
	ERLC_FLAGS += +export_all
else
	ERLC_FLAGS += -Werror
endif


.PHONY: clean test compile devel package export_all test_compile


ebin/%.beam: test_src/%.erl
	$(ERLC_LIBS) $(ERLC) $(ERLC_FLAGS) -o $(EBIN) $<


clean:
	$(REBAR) clean


compile:
	$(REBAR) compile



export_all:
	$(MAKE) EXPORT_ALL=true test_compile


ech:
	@echo $(BEAMS)

test_compile: $(BEAMS) $(TEST_BEAMS)


test: export_all
	$(ERL) -pa ebin/ -env ERL_LIBS "$(NORMALIZED_LIBS)" -noshell \
    	-eval 'test_runner:start({dir, "ebin"}, [verbose, {test_timeout, 15000}])' \
    	-s init stop 



devel: clean compile
	erl -pa ebin -env ERL_LIBS="$(NORMALIZED_LIBS)" \
		-config test.config -eval "application:start(caterpillar)."
			 


package: clean compile
	mkdir -p $(DEB_DIR)/DEBIAN
	cp control $(DEB_DIR)/DEBIAN
	chmod +x $(DEB_DIR)/DEBIAN/*
	mkdir -p $(DEB_DIR)/$(LIB_PATH)
	mkdir -p $(DEB_DIR)/$(LOG_PATH)
	mkdir -p $(DEB_DIR)/$(ETC_PATH)
	cp -R ebin $(DEB_DIR)/$(LIB_PATH)
	dpkg-deb --build $(DEB_DIR) $(DIST_DIR)
	
