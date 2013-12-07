EMPTY :=
SPACE := $(EMPTY) $(EMPTY)

BRANCH ?= $(empty)
DIST_DIR ?= dist
DEB_DIR ?= dist/deb

APPLICATION = #{application}
CORE_PATH = #{path}
LIB_PATH = var/lib/$(CORE_PATH)/$(APPLICATION)
ETC_PATH = etc/$(CORE_PATH)/$(APPLICATION)
LOG_PATH = var/log/$(CORE_PATH)/$(APPLICATION)
SHARE_DIR = usr/share/$(CORE_PATH)/$(APPLICATION)
INITD = etc/init.d
BIN = usr/sbin

LIBS := $(subst $(SPACE),:../,../#{libs})
TEST_LIBS = $(subst $(SPACE),:../,../#{libs})
DEVEL_LIBS = $(subst $(SPACE),:../,../#{devel_libs})
INCLUDES = $(patsubst %, -I../%/include, #{libs} #{name})

SRC := src
TEST_SRC := src
MOCK_SRC := mock
EBIN := ebin
MOCK := mock_ebin
ERL := erl
ERLC := erlc
ERLC_FLAGS := -Wall $(INCLUDES)

BEAMS = $(patsubst $(SRC)/%.erl,$(EBIN)/%.beam,$(wildcard $(SRC)/*.erl))
MOCKS = $(patsubst $(MOCK_SRC)/%.erl,$(MOCK)/%.beam,$(wildcard $(MOCK_SRC)/*.erl))

all:

ebin/%.beam: $(SRC)/%.erl
	ERL_LIBS="$(LIBS)" $(ERLC) $(ERLC_FLAGS) -o $(EBIN) $<

$(MOCK)/%.beam: mock/%.erl
	mkdir -p $(MOCK)
	$(ERLC_LIBS) $(ERLC) $(ERLC_FLAGS) -o $(MOCK) $<

compile: $(BEAMS)

mocks: $(MOCKS)

clean:
	rm -f $(EBIN)/*.beam
	rm -rf $(MOCK)
	rm -rf ct/log

test_compile: compile

export_all:
	$(MAKE) EXPORT_ALL=true test_compile

test: ERLC_FLAGS += +export_all +debug_info -DTEST
test: compile mocks
	$(ERL) -pa $(MOCK) -pa $(EBIN) -env ERL_LIBS "$(LIBS)" -noshell \
    	-eval 'test_runner:start({dir, "ebin"}, [{test_timeout, 15000}, verbose])' \
    	-s init stop 

ct: compile mocks
	mkdir -p ct/log
	ct_run -pa $(MOCK) -pa $(EBIN) -logdir ct/log -dir ct/* -erl_args -env ERL_LIBS "$(LIBS)"

test_coverage: export_all $(BEAMS)
	$(ERL) -pa $(EBIN) -env ERL_LIBS "$(LIBS)" \
		-eval 'test_runner:start({dir, "ebin"},[{test_timeout, 150000}, coverage])' \
		-s init stop

devel: compile
	$(ERL) -pa $(EBIN) -env ERL_LIBS "$(DEVEL_LIBS)" -config test.config

package: clean compile
	rm -rf $(DIST_DIR)
	mkdir -p $(DEB_DIR)/DEBIAN
	cp control $(DEB_DIR)/DEBIAN
ifneq ($(EMPTY),$(wildcard postinst))
	cp postinst $(DEB_DIR)/DEBIAN
endif
ifneq ($(EMPTY),$(wildcard postrm))
	cp postrm $(DEB_DIR)/DEBIAN
endif
ifneq ($(EMPTY),$(wildcard conffiles))
	cp conffiles $(DEB_DIR)/DEBIAN
endif
	chmod +x $(DEB_DIR)/DEBIAN/*
	mkdir -p $(DEB_DIR)/$(LIB_PATH)
	for i in #{executables}; do \
		mkdir -p $(DEB_DIR)/$(BIN); \
		cp $$i $(DEB_DIR)/$(BIN)/; \
	done
	for i in #{services}; do \
		mkdir -p $(DEB_DIR)/$(INITD); \
		ln -s /$(BIN)/$$i $(DEB_DIR)/$(INITD)/; \
	done
	for i in #{share}; do \
		mkdir -p $(DEB_DIR)/$(SHARE_DIR); \
		cp -r $$i $(DEB_DIR)/$(SHARE_DIR)/; \
	done
	for i in #{config}; do \
		mkdir -p $(DEB_DIR)/$(ETC_PATH); \
		cp -r $$i $(DEB_DIR)/$(ETC_PATH)/; \
	done
	rm -f ebin/*_tests.beam
	cp -R ebin $(DEB_DIR)/$(LIB_PATH)
	dpkg --build $(DEB_DIR) $(DIST_DIR)
