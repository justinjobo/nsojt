ROOT_DIR := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
TEST_STATUS := $(shell cat $(ROOT_DIR)/clint_test.status)

# The parallel execution includes a small file that defines the
# portnumber for the current ncs-node
ifneq (,$(wildcard $(ROOT_DIR)/erl-node-sname))
	NCS_IPC_PORT := $(shell cat $(ROOT_DIR)/erl-node-sname)
	NCS_ARGS := env CLINT_REPLAY=true TEST_STATUS=$(TEST_STATUS) NCS_IPC_PORT=$(NCS_IPC_PORT) sname=ncsd-$(NCS_IPC_PORT)
else
	NCS_ARGS := env CLINT_REPLAY=true TEST_STATUS=$(TEST_STATUS)
endif

test: stop build
	$(NCS_ARGS) lux $(ROOT_DIR)
.PHONY: test

clean:
	cd $(ROOT_DIR) && \
	rm -rf tabs logs ncs-cdb state iofiles 2>/dev/null || true
.PHONY: clean

build:
	cd $(ROOT_DIR) && \
	mkdir -p init/logs init/ncs-cdb init/state init/iofiles && \
	cp -r init/logs init/ncs-cdb init/state init/iofiles .
.PHONY: build

start:  stop
	cd $(ROOT_DIR) && ncs
	echo started
.PHONY: start

stop:
	cd $(ROOT_DIR) && ncs --stop || true
.PHONY: start
