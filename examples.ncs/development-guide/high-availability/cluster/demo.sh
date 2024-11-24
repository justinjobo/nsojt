#!/usr/bin/env bash

# An NSO Built-in High Availability Three Node Cluster Example
#
# Demo script
# (C) 2022 Tail-f Systems
# Permission to use this code as a starting point hereby granted
#
# See the README file for more information

set -eu # Abort the script if a command returns with a non-zero exit code or if
        # a variable name is dereferenced when the variable hasn't been set

RED='\033[0;31m'
GREEN='\033[0;32m'
PURPLE='\033[0;35m'
NC='\033[0m' # No Color

function usage()
{
   printf "${GREEN}Example of a basic high availability setup with one primary"
   printf " and two secondary nodes\n\n"
   printf "  -a  IP address for node 1. Default: 127.0.1.1\n"
   printf "  -b  IP address for node 2. Default: 127.0.2.1\n"
   printf "  -c  IP address for node 2. Default: 127.0.3.1\n"
   printf "\nOn most Linux distributions the above default IP addresses are"
   printf " configured for\nthe loopback interface by default. On MacOS the"
   printf " three unique IP addresses can be\ncreated using for example the ip"
   printf " or ifconfig command:\n\n"
   printf "# MacOS setup:\n"
   printf "\$ sudo ifconfig lo0 alias 127.0.1.1/24 up\n"
   printf "\$ sudo ifconfig lo0 alias 127.0.2.1/24 up\n"
   printf "\$ sudo ifconfig lo0 alias 127.0.3.1/24 up\n\n"
   printf "# MacOS cleanup:\n"
   printf "\$ sudo ifconfig lo0 -alias 127.0.1.1\n"
   printf "\$ sudo ifconfig lo0 -alias 127.0.2.1\n"
   printf "\$ sudo ifconfig lo0 -alias 127.0.3.1\n\n${NC}"
}

while getopts "a:b:c:h" OPTION; do
    case "${OPTION}"
    in
        a)  IP1="${OPTARG}";;
        b)  IP2="${OPTARG}";;
        c)  IP3="${OPTARG}";;
        h)  usage; exit 0;;
        \?) echo "Invalid parameter"; usage; exit 1;;
    esac
done

set +u
if [ -z "$IP1" ]; then
    IP1="127.0.1.1"
fi
if [ -z "$IP2" ]; then
    IP2="127.0.2.1"
fi
if [ -z "$IP3" ]; then
    IP3="127.0.3.1"
fi
set -u

ID=1
printf "\n${PURPLE}##### Reset, setup, start node 1-3, and enable HA assuming start-up settings\n${NC}"
NSO_IP1="$IP1" NSO_IP2="$IP2" NSO_IP3="$IP3" make stop &> /dev/null
NSO_IP1="$IP1" NSO_IP2="$IP2" NSO_IP3="$IP3" make clean all start

# All nodes use the same IP for IPC but different ports
export NCS_IPC_ADDR=127.0.0.1
NODE1=4561
NODE2=4562
NODE3=4563

printf "\n\n${PURPLE}##### Initial high-availability config for all three nodes\n${NC}"
NCS_IPC_PORT=$NODE1 ncs_load -W -Fp -p /high-availability

printf "\n${PURPLE}##### Add some dummy config to node 1, replicated to secondary nodes 2 and 3\n${NC}"
NCS_IPC_PORT=$NODE1 ncs_cli -n -u admin -C << EOF
config
dummies dummy d1 dummy 1.2.3.4
commit
end
show high-availability | notab | nomore
show running-config dummies | nomore
EOF

NCS_IPC_PORT=$NODE2 ncs_cli -n -u admin -C << EOF
show high-availability status | notab | nomore
show running-config dummies | nomore
EOF

NCS_IPC_PORT=$NODE3 ncs_cli -n -u admin -C << EOF
show high-availability status | notab | nomore
show running-config dummies | nomore
EOF

printf "\n\n${PURPLE}##### Stop node 1 to make node 2 failover to primary role and node 3 connect to the new primary\n${NC}"
NCS_IPC_PORT=$NODE1 ncs --stop

printf "\n"
RCI=$(NCS_IPC_PORT=$NODE2 ncs_cmd -c 'mrtrans; maapi_get "/high-availability/settings/reconnect-interval"')
RCA=$(NCS_IPC_PORT=$NODE2 ncs_cmd -c 'mrtrans; maapi_get "/high-availability/settings/reconnect-attempts"')
RCT=$((RCI*RCA))
while [[ $(NCS_IPC_PORT=$NODE2 ncs_cmd -o -c 'mrtrans; maapi_get "/high-availability/status/mode"') != "primary" ]]; do
    printf "${RED}#### Waiting for node 2 to fail reconnect to node 1 and assume primary role... $RCT\n${NC}"
    if [[ $RCT > 0 ]]; then
        let RCT--
    fi
    sleep 1
done

NCS_IPC_PORT=$NODE2 ncs_cli -n -u admin -C << EOF
show high-availability status | notab | nomore
show alarms alarm-list | notab | nomore
EOF

printf "\n"
while [[ $(NCS_IPC_PORT=$NODE3 ncs_cmd -o -c 'mrtrans; mexists "/high-availability/status/be-secondary-result"') != "yes" ]]; do
    printf "${RED}#### Waiting for node 3 to connect to new primary node 2...\n${NC}"
    sleep 1
done

NCS_IPC_PORT=$NODE3 ncs_cli -n -u admin -C << EOF
show high-availability status | notab | nomore
show alarms alarm-list | notab | nomore
EOF

printf "\n\n${PURPLE}##### Start node 1 that will now assume secondary role\n${NC}"
ncs --cd nso-node1 -c $(pwd)/nso-node1/ncs.conf

printf "\n\n"
while [[ $(NCS_IPC_PORT=$NODE1 ncs_cmd -o -c 'mrtrans; maapi_get "/high-availability/status/mode"') != "secondary" ]]; do
    printf "${RED}#### Waiting for node 1 to become secondary to node 2...\n${NC}"
    sleep 1
done

NCS_IPC_PORT=$NODE1 ncs_cli -n -u admin -C << EOF
show high-availability status | notab | nomore
EOF

NCS_IPC_PORT=$NODE2 ncs_cli -n -u admin -C << EOF
show high-availability status | notab | nomore
EOF

NCS_IPC_PORT=$NODE3 ncs_cli -n -u admin -C << EOF
show high-availability status | notab | nomore
EOF

printf "\n\n${PURPLE}##### Role-revert the nodes back to start-up settings${NC}"

NCS_IPC_PORT=$NODE1 ncs_cli -n -u admin -C << EOF
high-availability disable
EOF

NCS_IPC_PORT=$NODE2 ncs_cli -n -u admin -C << EOF
high-availability disable
EOF

NCS_IPC_PORT=$NODE1 ncs_cli -n -u admin -C << EOF
high-availability enable
EOF

printf "\n\n"
while [[ $(NCS_IPC_PORT=$NODE1 ncs_cmd -o -c 'mrtrans; maapi_get "/high-availability/status/mode"') != "primary" ]]; do
    printf "${RED}#### Waiting for node 1 to revert to primary role...\n${NC}"
    sleep 1
done

NCS_IPC_PORT=$NODE2 ncs_cli -n -u admin -C << EOF
high-availability enable
EOF

printf "\n\n"
while [[ $(NCS_IPC_PORT=$NODE2 ncs_cmd -o -c 'mrtrans; maapi_get "/high-availability/status/mode"') != "secondary" ]] || \
      [[ $(NCS_IPC_PORT=$NODE3 ncs_cmd -o -c 'mrtrans; mexists "/high-availability/status/be-secondary-result"') != "yes" ]]; do
    printf "${RED}#### Waiting for node 2 & 3 to revert to secondary role for primary node 1...\n${NC}"
    sleep 1
done

NCS_IPC_PORT=$NODE1 ncs_cli -n -u admin -C << EOF
show high-availability status | notab | nomore
show running-config dummies | nomore
EOF

NCS_IPC_PORT=$NODE2 ncs_cli -n -u admin -C << EOF
show high-availability status | notab | nomore
show running-config dummies | nomore
EOF

NCS_IPC_PORT=$NODE3 ncs_cli -n -u admin -C << EOF
show high-availability status | notab | nomore
show running-config dummies | nomore
EOF

printf "\n\n${GREEN}##### Done!\n${NC}"
