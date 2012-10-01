include ../../devel-tools/trunk/Makefile.mk

LIB_PATH = "/var/lib/caterpillar/"
LOG_PATH = "/var/log/caterpillar/"
ETC_PATH = "/etc/caterpillar/"


.PHONY: clean, test, compile, devel, package

REBAR = rebar
clean:
	$(REBAR) clean

compile:
	$(REBAR) compile

test: clean
	$(REBAR) -C rebar.test.config eunit

devel: clean compile
	erl -pa ebin -env ERL_LIBS="$(NORMALIZED_LIBS)" \
		-conig test.config -eval "application:start(caterpillar)."
			 


package: clean compile
	mkdir -p $(DEB_DIR)/DEBIAN
	cp control $(DEB_DIR)/DEBIAN
	chmod +x $(DEB_DIR)/DEBIAN/*
	mkdir -p $(DEB_DIR)/$(LIB_PATH)
	mkdir -p $(DEB_DIR)/$(LOG_PATH)
	mkdir -p $(DEB_DIR)/$(ETC_PATH)
	cp -R ebin $(DEB_DIR)/$(LIB_PATH)
	dpkg-deb --build $(DEB_DIR) $(DIST_DIR)
	
