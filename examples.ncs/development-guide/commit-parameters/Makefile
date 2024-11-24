all:
	cp -r package-repository/* packages
	for f in packages/*/src; do \
		$(MAKE) -C $$f all || exit 1; \
	done
	$(MAKE) netsim

netsim:
	ncs-netsim create-network ./packages/router 1 ex --dir ./netsim

start:
	ncs --addloadpath .
	ncs-netsim start --dir ./netsim
	echo "devices sync-from" | ncs_cli -C -u admin

stop:
	-ncs --stop
	-ncs-netsim -a stop

clean:
	for f in packages/*/src; do \
		$(MAKE) -C $$f clean || exit 1; \
	done
	rm -rf ./netsim running.DB logs/* state/* ncs-cdb/*.cdb target storedstate scripts/*.class

showcase-commit-params:
	./showcase.sh
