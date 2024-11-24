#!/bin/sh

set -eu # Abort the script if a command returns with a non-zero exit code or if
        # a variable name is dereferenced when the variable hasn't been set

RED='\033[0;31m'
GREEN='\033[0;32m'
PURPLE='\033[0;35m'
NC='\033[0m' # No Color

printf "\n\n${GREEN}##### Showcase 1: Display commit params in Python\n${NC}"

printf "\n\n${PURPLE}##### Step 1: Start the ncs and routers\n\n${NC}"
make stop clean all start

printf "\n\n${PURPLE}##### Step 2: Display devices and NSO packages\n${NC}"
ncs_cli -n -C -u admin << EOF
show devices list
devices sync-from
packages reload
EOF

printf "\n\n${PURPLE}##### Step 3: Showcase commit param detection in Python\n${NC}"

sleep 1
ncs_cli -n -C -u admin << EOF
config
commit-params-py demo device ex0 if GigabitEthernet0 if-speed hundred
commit dry-run
commit
EOF

printf "\n\nDisplay service Python log\n"
cat logs/ncs-python-vm-commit-params-py.log | grep "Service create" -m 1 -A 5

printf "\n\n${PURPLE}##### Step 4: Showcase applying commit params in Python action\n${NC}"

ncs_cli -n -C -u admin << EOF
commit-params-py demo showcase-py
EOF

sleep 2
printf "\n\nDisplay ncs-python-vm log\n"
cat logs/ncs-python-vm-commit-params-py.log | grep "Apply commit parameter" -A 23

printf "\n\n${GREEN}##### Showcase 2: Display commit params in Java\n${NC}"

printf "\n\n${PURPLE}##### Step 1: Showcase commit param detection in Java'\n${NC}"

sleep 2
ncs_cli -n -C -u admin << EOF
config
commit-params-java demo device ex0 if GigabitEthernet2 if-speed hundred
commit dry-run
commit
EOF

printf "\n\nDisplay service Java log\n"
cat logs/ncs-java-vm.log | grep "Commit parameters:" -m 1 -A 1

printf "\n\n${PURPLE}##### Step 2: Showcase applying commit params in Java action\n${NC}"

ncs_cli -n -C -u admin << EOF
commit-params-java demo showcase-java
EOF

sleep 2
printf "\n\nDisplay ncs-java-vm log\n"
cat logs/ncs-java-vm.log | grep "Apply commit param" -A 22
