include ../../../../../support/include.mk
include ../../../../../system/test/support/commontest.mk

# Args for Builing  and Testitesting SUT (SYSTEM UNDER TEST)
CONFD     = $(CONFD_DIR)/bin/confd
CONFDC    = $(CONFD_DIR)/bin/confdc
CDB_DIR   = ./confd-cdb
AAA_INIT  = $(CONFD_DIR)/var/confd/cdb/aaa_init.xml

# Args for Running Common test.
SUITES   ?= $(wildcard *_SUITE.erl)
FXS      ?= $(patsubst %.yang,%.fxs,$(wildcard *.yang))

# reverse the dir list to make .../confd/ebin end up before .../econfd/ebin,
# See bin/confd $libebin . Needed so we get the correct econfd_internal.beam.
CT_FLAGS += -pa $(shell ls -rd $(NATIVE_TAILFTOP_DIR)/lib/*/ebin) \
	    -pa $(shell cd $(TEST_DIR)/lib/meck/ebin; pwd) \
	    -conffile $(SRC_DIR)/confd.conf

build:  remove_db dirs copy_init $(FXS) confd.conf ctbuild

clean:	ctclean
	rm -f confd.conf $(FXS) *.xso *.log confderr.* *.beam
	rm -f confd_candidate.db global.data local.data
	rm -fr $(CDB_DIR) logs cli-history

desc:
	@echo "Common tests for the econfd API"

confd.conf: confd.conf.in
	rm -f $@
	sed -e 's|\$$PWD|$(SRC_DIR)|g' -e 's|\$$CONFD_DIR|$(CONFD_DIR)|g' < $< > $@

%.fxs: %.yang
	$(CONFDC) `ls $*-ann.yang > /dev/null 2>&1 && echo "-a $*-ann.yang"` \
		-c $< -o $@

remove_db: $(CDB_DIR)
	rm -f $(CDB_DIR)/*.cdb

dirs:
	mkdir -p logs

copy_init:
	cp $(AAA_INIT) $(CDB_DIR)
	cp *_init.xml $(CDB_DIR) 2>/dev/null || :

include $(CONFD_DIR)/src/confd/build/include.mk
