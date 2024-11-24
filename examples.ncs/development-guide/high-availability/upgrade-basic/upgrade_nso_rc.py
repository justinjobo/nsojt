#!/usr/bin/env python3

"""A Basic NSO Built-in High Availability Two Node NSO Version Upgrade Example.

Upgrade script
(C) 2022 Tail-f Systems
Permission to use this code as a starting point hereby granted

See the README file for more information
"""
import subprocess
import json
import argparse
import time
import os
import re
import requests
from packaging import version

USAGE_STR = """Example of a basic high availability setup with one primary
and one secondary node performing a NSO version upgrade.
On most Linux distributions the above default IP addresses are configured for
the loopback interface by default. On MacOS the two unique IP addresses can be
created using for example the ip or ifconfig command:

        # MacOS setup:
        $ sudo ifconfig lo0 alias 127.0.1.1/24 up
        $ sudo ifconfig lo0 alias 127.0.2.1/24 up

        # MacOS cleanup:
        $ sudo ifconfig lo0 -alias 127.0.1.1
        $ sudo ifconfig lo0 -alias 127.0.2.1"""


def ha_upgrade_demo(ip1, ip2, olddir, newdir):
    """Run the HA upgrade demo"""
    auth = ('admin', 'admin')
    node1_url = 'http://{}:8080/restconf'.format(ip1)
    node2_url = 'http://{}:8080/restconf'.format(ip2)
    header = '\033[95m'
    okblue = '\033[94m'
    okgreen = '\033[92m'
    endc = '\033[0m'
    bold = '\033[1m'
    ipc1 = 4561
    ipc2 = 4562

    session = requests.Session()
    session.auth = auth
    headers = {'Content-Type': 'application/yang-data+json'}

    r = subprocess.run(['{}/bin/ncs'.format(olddir), '--version'], check=True,
                       stdout=subprocess.PIPE, encoding='utf-8')
    # Replace underscore characters with dot and remove anything after a
    # non numeric/dot character in the NSO version string.
    old_version = r.stdout.split('_')[0]
    r = subprocess.run(['{}/bin/ncs'.format(newdir), '--version'], check=True,
                       stdout=subprocess.PIPE, encoding='utf-8')
    new_version = r.stdout.split('_')[0]

    print(f"\n{okblue}##### Reset, setup, start node 1 & 2, and enable HA"
          f" with start-up settings\n{endc}")
    my_env = os.environ.copy()
    my_env['NSO_IP1'] = ip1
    my_env['NSO_IP2'] = ip2
    subprocess.run(['make', 'stop', 'clean', 'all', 'start'], check=True,
                   stderr=subprocess.PIPE, stdout=subprocess.PIPE,
                   env=my_env, encoding='utf-8')

    print(f"\n{okblue}##### Initial high-availability config for both"
          f" nodes\n{endc}")
    path = '/data/tailf-ncs:high-availability?content=config&' \
           'with-defaults=report-all'
    print(f"{bold}GET " + node1_url + path + f"{endc}")
    r = session.get(node1_url + path, headers=headers)
    print(r.text)

    print(f"\n{okblue}##### Add some dummy config to node 1, replicated to"
          f" secondary node 2\n{endc}")

    dummy_data = {}
    dummy_data["name"] = "d1"
    dummy_data["dummy"] = "1.2.3.4"
    dummies_data = {}
    dummies_data["dummy"] = [dummy_data]
    input_data = {"dummy:dummies": dummies_data}

    path = '/data'
    print(f"{bold}PATCH " + node1_url + path + f"{endc}")
    print(f"{header}" + json.dumps(input_data, indent=2) + f"{endc}")
    r = session.patch(node1_url + path, json=input_data, headers=headers)
    print("Status code: {}\n".format(r.status_code))

    path = '/data/tailf-ncs:high-availability?content=nonconfig'
    print(f"{bold}GET " + node1_url + path + f"{endc}")
    r = session.get(node1_url + path, headers=headers)
    print(r.text)

    path = '/data/dummy:dummies'
    print(f"{bold}GET " + node1_url + path + f"{endc}")
    r = session.get(node1_url + path, headers=headers)
    print(r.text)

    path = '/data/tailf-ncs:high-availability?content=nonconfig'
    print(f"{bold}GET " + node2_url + path + f"{endc}")
    r = session.get(node2_url + path, headers=headers)
    print(r.text)

    path = '/data/dummy:dummies'
    print(f"{bold}GET " + node2_url + path + f"{endc}")
    r = session.get(node2_url + path, headers=headers)
    print(r.text)

    print(f"\n{okblue}##### Enable read-only mode for node 1\n{endc}")
    path = '/operations/high-availability/read-only'
    print(f"{bold}POST " + node1_url + path + f"{endc}")
    r = session.post(node1_url + path, headers=headers)
    print("Status code: {}\n".format(r.status_code))

    print(f"\n{okblue}##### Enable read-only mode for node 2\n{endc}")
    path = '/operations/high-availability/read-only'
    print(f"{bold}POST " + node2_url + path + f"{endc}")
    r = session.post(node2_url + path, headers=headers)
    print("Status code: {}\n".format(r.status_code))

    print(f"\n{okblue}##### Backup before upgrading\n##### Since we are using"
          f" a local NSO install, we backup the runtime directories for"
          f" potential disaster recovery.\n{endc}")
    subprocess.run(['make', 'backup'],
                   check=True, encoding='utf-8')

    # NSO 5.5 removed the show-log-directory parameter.
    if version.parse(old_version) < version.parse("5.5") and \
       version.parse(new_version) >= version.parse("5.5"):
        with open("nso-node1/ncs.conf", "r", encoding='utf-8') as file:
            data = file.read().replace("<show-log-directory>./logs"
                                       "</show-log-directory>", "")
        with open("nso-node1/ncs.conf", "w", encoding='utf-8') as file:
            file.write(data)
        with open("nso-node2/ncs.conf", "r", encoding='utf-8') as file:
            data = file.read().replace("<show-log-directory>./logs"
                                       "</show-log-directory>", "")
        with open("nso-node2/ncs.conf", "w", encoding='utf-8') as file:
            file.write(data)

    # NSO 5.6 removed large-scale parameters.
    if version.parse(old_version) < version.parse("5.6") and \
       version.parse(new_version) >= version.parse("5.6"):
        with open("nso-node1/ncs.conf", "r", encoding='utf-8') as file:
            data = file.read().replace("<large-scale>", "<!-- large-scale>")
            data = data.replace("</large-scale>", "</large-scale -->")
        with open("nso-node1/ncs.conf", "w", encoding='utf-8') as file:
            file.write(data)
        with open("nso-node2/ncs.conf", "r", encoding='utf-8') as file:
            data = file.read().replace("<large-scale>", "<!-- large-scale>")
            data = data.replace("</large-scale>", "</large-scale -->")
        with open("nso-node2/ncs.conf", "w", encoding='utf-8') as file:
            file.write(data)

    print(f"\n{okblue}##### Disable node 1 high availability for node 2 to"
          f"automatically failover and assume primary role in read-only mode"
          f"\n{endc}")
    path = '/operations/high-availability/disable'
    print(f"{bold}POST " + node1_url + path + f"{endc}")
    r = session.post(node1_url + path, headers=headers)
    print("Status code: {}\n".format(r.status_code))

    print(f"\n{okblue}##### Switch VIP to point to node 2 instead node 1"
          f"\n{endc}")

    print(f"\n{okblue}##### Rebuild node 1 package(s) with " + new_version +
          f"\n{endc}")
    subprocess.run(['tar', 'xvfz', 'dummy-1.0.tar.gz'],
                   cwd='nso-node1/packages', check=True, encoding='utf-8')
    os.remove("nso-node1/packages/dummy-1.0.tar.gz")
    subprocess.run(['make', '-C', 'nso-node1/packages/dummy-1.0/src', 'clean',
                    'all'], check=True, encoding='utf-8')

    print(f"\n{okblue}##### Upgrade node 1 to " + new_version + f"\n{endc}")
    my_env = os.environ.copy()
    my_env['NCS_IPC_ADDR'] = '127.0.0.1'
    my_env["NCS_IPC_PORT"] = str(ipc1)
    my_env["sname"] = 'ncsd1'
    subprocess.run(['{}/bin/ncs'.format(olddir), '--stop'], check=True,
                   env=my_env, encoding='utf-8')
    subprocess.run(['{}/bin/ncs'.format(newdir), '--cd', 'nso-node1', '-c',
                    '{}/nso-node1/ncs.conf'.format(os.getcwd()),
                    '--with-package-reload'], check=True, env=my_env,
                   encoding='utf-8')

    print(f"\n{okblue}##### Switch VIP back to point to node 1\n{endc}")

    print(f"\n{okblue}##### Disable high availability in node 2\n{endc}")
    path = '/operations/high-availability/disable'
    print(f"{bold}POST " + node2_url + path + f"{endc}")
    r = session.post(node2_url + path, headers=headers)
    print("Status code: {}\n".format(r.status_code))

    print(f"\n{okblue}##### Enable high availability in node 1"
          f" that will assume primary role\n{endc}")
    path = '/operations/high-availability/enable'
    print(f"{bold}POST " + node1_url + path + f"{endc}")
    r = session.post(node1_url + path, headers=headers)
    print("Status code: {}\n".format(r.status_code))

    print(f"\n{okblue}##### Rebuild node 2 package(s) with " + new_version +
          f"\n{endc}")
    subprocess.run(['tar', 'xvfz', 'dummy-1.0.tar.gz'],
                   cwd='nso-node2/packages', check=True, encoding='utf-8')
    os.remove("nso-node2/packages/dummy-1.0.tar.gz")
    subprocess.run(['make', '-C', 'nso-node2/packages/dummy-1.0/src', 'clean',
                    'all'], check=True, encoding='utf-8')

    print(f"\n{okblue}##### Upgrade node 2 to " + new_version + f"\n{endc}")
    my_env = os.environ.copy()
    my_env['NCS_IPC_ADDR'] = '127.0.0.1'
    my_env["NCS_IPC_PORT"] = str(ipc2)
    my_env["sname"] = 'ncsd2'
    subprocess.run(['{}/bin/ncs'.format(olddir), '--stop'], check=True,
                   env=my_env, encoding='utf-8')
    subprocess.run(['{}/bin/ncs'.format(newdir), '--cd', 'nso-node2', '-c',
                    '{}/nso-node2/ncs.conf'.format(os.getcwd()),
                    '--with-package-reload'], check=True, env=my_env,
                   encoding='utf-8')

    while True:
        path = '/data/tailf-ncs:high-availability/status/mode'
        print(f"{bold}GET " + node1_url + path + f"{endc}")
        r = session.get(node1_url + path, headers=headers)
        if "primary" in r.text:
            break
        print(f"{header}#### Waiting for node 1 to become primary...{endc}")
        time.sleep(1)

    print(f"\n{okblue}##### Enable high availability in node 2"
          f" that will assume secondary role\n{endc}")
    path = '/operations/high-availability/enable'
    print(f"{bold}POST " + node2_url + path + f"{endc}")
    r = session.post(node2_url + path, headers=headers)
    print("Status code: {}\n".format(r.status_code))

    while True:
        path = '/data/tailf-ncs:high-availability/status/connected-secondary'
        print(f"{bold}GET " + node1_url + path + f"{endc}")
        r = session.get(node1_url + path, headers=headers)
        if "n2" in r.text:
            break
        print(f"{header}#### Waiting for node 2 to become secondary to node 1"
              f"...{endc}")
        time.sleep(1)

    path = '/data/tailf-ncs:high-availability?content=nonconfig'
    print(f"{bold}GET " + node1_url + path + f"{endc}")
    r = session.get(node1_url + path, headers=headers)
    print(r.text)

    path = '/data/dummy:dummies'
    print(f"{bold}GET " + node1_url + path + f"{endc}")
    r = session.get(node1_url + path, headers=headers)
    print(r.text)

    path = '/data/tailf-ncs:high-availability?content=nonconfig'
    print(f"{bold}GET " + node2_url + path + f"{endc}")
    r = session.get(node2_url + path, headers=headers)
    print(r.text)

    path = '/data/dummy:dummies'
    print(f"{bold}GET " + node2_url + path + f"{endc}")
    r = session.get(node2_url + path, headers=headers)
    print(r.text)

    print(f"\n{okgreen}##### Done!\n{endc}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description=USAGE_STR,
    )
    parser.add_argument('-a', '--ip1', nargs=1, type=str, default="127.0.1.1",
                        help='IP address for node 1')
    parser.add_argument('-b', '--ip2', nargs=1, type=str, default="127.0.2.1",
                        help='IP address for node 2')
    parser.add_argument('-o', '--olddir', nargs=1, type=str,
                        default="{}".format(os.getenv('NCS_DIR')),
                        help='Path to old NSO local install')
    parser.add_argument('-n', '--newdir', nargs=1, type=str,
                        default="{}".format(os.getenv('NCS_DIR')),
                        help='Path to new NSO local install')

    args = parser.parse_args()
    ha_upgrade_demo("".join(args.ip1), "".join(args.ip2), "".join(args.olddir),
                    "".join(args.newdir))
