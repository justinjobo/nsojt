package com.example.iface;

import com.example.iface.namespaces.*;
import java.util.List;
import com.tailf.conf.*;
import com.tailf.navu.*;
import com.tailf.ncs.ApplicationComponent;
import com.tailf.ncs.NcsMain;
import com.tailf.ncs.ns.Ncs;
import com.tailf.maapi.*;
import java.net.*;
import java.util.Random;

public class OperComp implements ApplicationComponent {

    public void init() throws Exception {
        int port = NcsMain.getInstance().getNcsPort();

        // Ensure socket gets closed on errors, also ending any ongoing
        // session and transaction
        try (Socket socket = new Socket("localhost", port)) {
            Maapi maapi = new Maapi(socket);
            maapi.startUserSession("admin", InetAddress.getByName("localhost"),
                "system", new String[] {}, MaapiUserSessionFlag.PROTO_TCP);

            NavuContext context = new NavuContext(maapi);
            context.startOperationalTrans(Conf.MODE_READ_WRITE);

            NavuContainer root =
                new NavuContainer(context).container(iface.hash);

            Random rnd = new Random();
            for (NavuContainer service : root.list("iface")) {
                NavuLeaf statusLeaf = service.leaf("last-test-status");
                if (statusLeaf.value() == null) {
                    statusLeaf.set(rnd.nextInt(2) == 0 ? "up" : "down");
                }
            }

            context.applyClearTrans();
        }
    }

    public void run() {
    }

    public void finish() {
    }
}
