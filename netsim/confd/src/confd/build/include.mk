# Make sure makefiles which have a usage target first use it even if
# they include this file at the top...

iusage: usage

# Define common ConfD build tools and flags

OSNAME       := $(shell uname -s)


CONFD        = $(CONFD_DIR)/bin/confd
CONFDC       = $(CONFD_DIR)/bin/confdc
CONFD_CMD    = $(CONFD_DIR)/bin/confd_cmd
PYANG        = $(CONFD_DIR)/bin/pyang
SMIDUMP      = $(CONFD_DIR)/bin/smidump
INCLUDE      = -I$(CONFD_DIR)/include
CONFD_LIB   ?= $(CONFD_DIR)/lib/libconfd.a
CONFD_LIB_SO ?= $(CONFD_DIR)/lib/libconfd.so
CONFD_VSN    = $(shell cat $(CONFD_DIR)/src/confd/build/vsn.mk | \
                       grep CONFDVSN | sed 's/.*=//')

## If you get irritated by the fail on warnings, set this variable
ifeq ($(shell echo $$FXS_NO_FAIL_ON_WARNING), true)
FXS_WERR     =
else
FXS_WERR     ?= --fail-on-warnings
endif

LIBS         = $(CONFD_LIB) -lpthread -lm
LIBDL        = -ldl
CFLAGS       = -Wall -g $(INCLUDE) -DCONFD_C_PRODUCT_CONFD $(LUX_TEST_CFLAGS)
CDB_DIR      = ./confd-cdb

KILLALL      = killall

JARFILE     = $(CONFD_DIR)/java/jar/conf-api-$(CONFD_VSN).jar
LOG4JAPI    = $(CONFD_DIR)/java/jar/log4j-api.jar
LOG4JCORE   = $(CONFD_DIR)/java/jar/log4j-core.jar
LOG4JWRAP   = $(CONFD_DIR)/java/jar/log4j.jar
CLASSPATH   = $(JARFILE):$(LOG4JAPI):$(LOG4JCORE):$(LOG4JWRAP):.
JAVAC       = javac

ifndef PYTHON
	PYTHON := $(shell python3 -V 2>/dev/null || echo python)
ifneq ($(PYTHON),python)
	PYTHON := python3
endif
endif

ifeq ($(OSNAME),FreeBSD)
CFLAGS      += -I/usr/local/include
LIBS        += -L/usr/local/lib
LIBDL       =
endif
ifeq ($(OSNAME),Darwin)
ifneq ($(shell which port 2>/dev/null),)
## These paths are MacPorts-specific
CFLAGS      += -I/opt/local/include
LIBS        += -L/opt/local/lib
else

## Check what our libconfd is linked with, try to use the same library
CRYPTO_LIB_PATH := $(shell otool -L $(CONFD_LIB_SO) 2>/dev/null| grep -Eo '.*libcrypto[^ ]+ ' | xargs -I% dirname % 2>/dev/null)
CRYPTO_LIB_DIR := $(dir $(CRYPTO_LIB_PATH))
ifneq ($(CRYPTO_LIB_DIR),)
CFLAGS      += -I$(CRYPTO_LIB_DIR)/include
LIBS        += -L$(CRYPTO_LIB_DIR)/lib
else
# Fallback - in Jenkins test env these are links to OpenSSL
# and there seems to be no otool command to get proper path anyway
CFLAGS      += -I/opt/local/include
LIBS        += -L/opt/local/lib
endif

endif # which port
endif # Darwin

ifeq ($(OSNAME),Darwin)
SHARED_FLAGS= -dynamiclib
LD_ENV      = DYLD_LIBRARY_PATH
else
SHARED_FLAGS= -shared
LD_ENV      = LD_LIBRARY_PATH
endif

# Targets to require/reject specific OS

.PHONY: linux not_sunos

linux:
ifneq ($(OSNAME),Linux)
	@echo "This example only works on Linux" ; exit 1
endif

not_sunos:
ifeq ($(OSNAME),SunOS)
	@echo "This example does not work on Solaris" ; exit 1
endif

# Define default ConfD build rules

%.h: %.fxs $(CONFDC)
	$(CONFDC) --emit-h $*.h $*.fxs

%.java: %.fxs $(CONFDC)
	$(CONFDC) --emit-java $*.java $*.fxs

%_ns.py: %.fxs $(CONFDC)
	$(CONFDC) $(CONFDC_PYTHON_FLAGS) --emit-python $*_ns.py $*.fxs

%.fxs: %.yang #$(CONFDC)
	$(CONFDC) $(FXS_WERR) $(EXTRA_LINK_FLAGS) -c -o $@  $<

%.ccl: %.cli
	$(CONFDC) -c $<

%.o: %.c
	$(CC) -c -o $@ $< $(CFLAGS)

%.class: %.java
	$(JAVAC) -classpath $(CLASSPATH) $<

%.bin: %.fxs
	$(CONFDC) -c $*.mib $*.fxs -f $(CONFD_DIR)/etc/confd/snmp

ssh-keydir:
	ln -s $(CONFD_DIR)/etc/confd/ssh $@

iclean:
	-rm -rf \
		*.o *.a *.xso *.fxs *.xsd *.ccl \
		*_proto.h \
		$(CDB_DIR) *.db aaa_cdb.* \
		rollback*/rollback{0..999} rollback{0..999} \
		cli-history \
		host.key host.cert ssh-keydir \
		*.log confderr.log.* \
		etc *.access \
		running.invalid global.data _tmp* local.data

$(CDB_DIR):
	-mkdir -p $(CDB_DIR)
	cp $(CONFD_DIR)/var/confd/cdb/aaa_init.xml $(CDB_DIR)

INITS=$(patsubst %_init.xml,$(CDB_DIR)/%_init.xml,$(wildcard *_init.xml))
$(CDB_DIR)/%_init.xml: %_init.xml $(CDB_DIR)
	cp $< $@
