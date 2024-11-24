CONFIG_FILE = ../_common/confd.conf

include ../_common/common.mk

smp.hrl: smp.fxs
	$(CONFDC) --emit-hrl $@ $<

# Mandatory targets
build: smp.hrl
	$(MAKE) -f ../_common/common.mk $@
