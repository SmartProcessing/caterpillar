include ../../devel-tools/trunk/Makefile.mk

LIB_PATH = var/lib/caterpillar
LOG_PATH = var/log/caterpillar
ETC_PATH = etc/caterpillar
INITD = etc/init.d
SBIN_DIR = usr/sbin

PRIV_PATH = $(LIB_PATH)/priv
REBAR = rebar
BEAMS = $(patsubst src/%.erl, ebin/%.beam, $(wildcard src/*.erl))
TEST_BEAMS = $(patsubst test_src/%.erl, ebin/%.beam, $(wildcard test_src/*.erl))

ERLC_FLAGS += -W

ifdef EXPORT_ALL
	ERLC_FLAGS += +export_all
else
	ERLC_FLAGS += -pa $(EBIN)
endif


.PHONY: clean test compile devel package export_all test_compile


ebin/%.beam: test_src/%.erl
	$(ERLC_LIBS) $(ERLC) $(ERLC_FLAGS) -o $(EBIN) $<


clean:
	rm -rf dist
	rm -f erl_crash.dump
	rm -f $(EBIN)/*.beam


compile:
	$(REBAR) compile



export_all:
	$(MAKE) EXPORT_ALL=true test_compile


ech:
	@echo $(BEAMS)

test_compile: $(BEAMS) $(TEST_BEAMS)


test: export_all
	$(ERL) -pa ebin/ -env ERL_LIBS "$(NORMALIZED_LIBS)" -noshell \
    	-eval 'test_runner:start({application, caterpillar}, [verbose, {test_timeout, 15000}])' \
    	-s init stop 



devel: $(TEST_BEAMS) $(BEAMS) 
	$(ERL) -pa ebin -env ERL_LIBS "$(NORMALIZED_LIBS)" -config test.config \
		-s caterpillar_app start
			 


package: clean compile
	mkdir -p $(DEB_DIR)/DEBIAN
	cp control conffiles postinst postrm $(DEB_DIR)/DEBIAN
	chmod +x $(DEB_DIR)/DEBIAN/*
	mkdir -p $(DEB_DIR)/$(LIB_PATH)
	mkdir -p $(DEB_DIR)/$(LOG_PATH)
	mkdir -p $(DEB_DIR)/$(ETC_PATH)
	mkdir -p $(DEB_DIR)/$(INITD)
	mkdir -p $(DEB_DIR)/$(SBIN_DIR)
	mkdir -p $(DEB_DIR)/$(PRIV_PATH)
	cp caterpillar.config $(DEB_DIR)/$(ETC_PATH)/
	cp caterpillar_shell $(DEB_DIR)/$(SBIN_DIR)/caterpillar
	cp caterpillar.escript $(DEB_DIR)/$(SBIN_DIR)	
	chmod +x $(DEB_DIR)/$(SBIN_DIR)/*
	cp -R ebin $(DEB_DIR)/$(LIB_PATH)
	cp priv/* $(DEB_DIR)/$(PRIV_PATH)
	chmod +x $(DEB_DIR)/$(PRIV_PATH)/*
	ln -s /$(SBIN_DIR)/caterpillar $(DEB_DIR)/$(INITD)/caterpillar
	dpkg-deb --build $(DEB_DIR) $(DIST_DIR)
	
