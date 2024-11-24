######################################################################
# Example showing a NSO version upgrade of the upper and one lower NSO
# instance for the developing-with-ncs/28-lsa-multi-version-deployment
# example.
#
# (C) 2021 Tail-f Systems
#
# See the README file for more information
######################################################################
RED=\033[0;31m
NC=\033[0;m # No Color
BACKUP_FILE=backup.tar.gz
OS=$(shell uname -s)

all: stop clean

check:
	@if [ -z ${NCS_DIR} ]; then \
	  printf "${RED}Where is NSO installed? Source ncsrc or set NCS_DIR to"; \
		printf " point it out!\n${NC}"; \
		exit 1; \
	fi

upgrade:
	./multiver_nso_upgrade.sh -h
	./multiver_nso_upgrade.sh

start: check upgrade

stop: check
	if [ -d 28-lsa-multi-version-deployment/netsim ]; then \
		ncs-netsim --dir 28-lsa-multi-version-deployment/netsim stop; \
	fi
	-NCS_IPC_PORT=4569 ncs --stop
	-NCS_IPC_PORT=4570 ncs --stop
	-NCS_IPC_PORT=4571 ncs --stop
	@echo "##### All NSO and netsim instances stopped"

clean:
	rm -rf 28-lsa-multi-version-deployment packages $(BACKUP_FILE)

backup:
ifeq ($(OS),Darwin)
	-tar cfz $(BACKUP_FILE) 22-lsa-single-version-deployment packages
else
	-tar --warning=no-file-changed --gzip -cf $(BACKUP_FILE) 22-lsa-single-version-deployment packages
endif
	@printf "\n${GREEN}$(BACKUP_FILE) created\n${NC}"

restore:
ifneq ("$(wildcard $(BACKUP_FILE))","")
		rm -rf 28-lsa-multi-version-deployment packages
		tar xvfz $(BACKUP_FILE)
		@printf "\n${GREEN}$(BACKUP_FILE) restored\n${NC}"
else
		@printf "\n${RED}$(BACKUP_FILE) does not exist\n${NC}"
endif
