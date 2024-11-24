
NETSIM_DIR = netsim
PACKAGES =

NED_PS = $(NCS_DIR)/packages/neds

NETWORK = \
    create-network packages/$(CISCO_IOS_VER) 1 ce \

EXTRA_WEBUI_CONFIG = \
	<login-dir>my-login-page</login-dir>\

.PHONY: build
build:
	ncs-setup --dest .
	cp ncs.conf ncs.conf.in
	sed -e "s|</webui>|$(EXTRA_WEBUI_CONFIG)</webui>|" \
		< ncs.conf.in > ncs.conf

.PHONY: stop
stop:
	ncs --stop || true

.PHONY: start
start:
	ncs