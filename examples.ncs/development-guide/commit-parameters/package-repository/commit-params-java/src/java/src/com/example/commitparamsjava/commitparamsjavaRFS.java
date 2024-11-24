package com.example.commitparamsjava;

import com.example.commitparamsjava.namespaces.*;
import java.util.List;
import java.util.Properties;
import com.tailf.conf.*;
import com.tailf.navu.*;
import com.tailf.ncs.ns.Ncs;
import com.tailf.dp.*;
import com.tailf.dp.annotations.*;
import com.tailf.dp.proto.*;
import com.tailf.dp.services.*;
import com.tailf.ncs.template.Template;
import com.tailf.ncs.template.TemplateVariables;
import java.net.Socket;
import java.net.InetAddress;
import com.tailf.maapi.*;
import com.tailf.navu.*;
import com.tailf.conf.*;
import com.tailf.maapi.DryRunResult;
import com.tailf.maapi.DryRunResult.DryRunEntry;
import java.util.Iterator;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class commitparamsjavaRFS {
    private static Logger log = LogManager.getLogger(commitparamsjavaRFS.class);

    /**
     * Create callback method.
     * This method is called when a service instance committed due to a create
     * or update event.
     *
     * This method returns a opaque as a Properties object that can be null.
     * If not null it is stored persistently by Ncs.
     * This object is then delivered as argument to new calls of the create
     * method for this service (fastmap algorithm).
     * This way the user can store and later modify persistent data outside
     * the service model that might be needed.
     *
     * @param context - The current ServiceContext object
     * @param service - The NavuNode references the service node.
     * @param ncsRoot - This NavuNode references the ncs root.
     * @param opaque  - Parameter contains a Properties object.
     *                  This object may be used to transfer
     *                  additional information between consecutive
     *                  calls to the create callback.  It is always
     *                  null in the first call. I.e. when the service
     *                  is first created.
     * @return Properties the returning opaque instance
     * @throws ConfException
     */

    @ServiceCallback(servicePoint="commit-params-java-servicepoint",
        callType=ServiceCBType.CREATE)
    public Properties create(ServiceContext context,
                             NavuNode service,
                             NavuNode ncsRoot,
                             Properties opaque)
                             throws ConfException {

        Template myTemplate = new Template(context,
                                    "commit-params-java-template");
        TemplateVariables myVars = new TemplateVariables();

        try {

            // Detect transaction commit parameters
            NavuContext ctx = service.context();
            Maapi maapi = ctx.getMaapi();
            int th = ctx.getMaapiHandle();
            CommitParams cp = maapi.getTransParams(th);
            log.info("Commit parameters: " + cp);

            if (cp.isDryRun()) {
                log.info("Dry run detected!");
            }

            myTemplate.apply(service, myVars);

        } catch (Exception e) {
            throw new DpCallbackException(e.getMessage(), e);
        }
        return opaque;
    }

    /**
     * Showcase action
     */
    @ActionCallback(callPoint="showcase-java", callType=ActionCBType.ACTION)
    public ConfXMLParam[] selftest(DpActionTrans trans, ConfTag name,
                                   ConfObject[] kp, ConfXMLParam[] params)
    throws DpCallbackException {
        try {
            String nsPrefix = "commit-params-java";
            String str = ((ConfKey)kp[0]).toString();

            Socket socket = new Socket("127.0.0.1", Conf.NCS_PORT);
            Maapi maapi = new Maapi(socket);
            maapi.startUserSession("admin",
                                    InetAddress.getByName(null),
                                    "system",
                                    new String[]{"admin"},
                                    MaapiUserSessionFlag.PROTO_TCP);

            // Start a MAAPI transaction
            int th = maapi.startTrans(Conf.DB_RUNNING, Conf.MODE_READ_WRITE);

            // Perform some configuration changes on the device
            String intfc = "GigabitEthernet3";
            String device = "ex0";
            String ifPath =
                "/devices/device{%x}/config/r:sys/interfaces/interface{%x}";
            maapi.safeCreate(th,
                            ifPath,
                            new ConfBuf(device),
                            new ConfBuf(intfc));
            ConfPath confPath = new ConfPath(ifPath, device, intfc);
            maapi.safeCreate(th, confPath + "/enabled");
            ConfEnumeration enum_speed =
                ConfEnumeration.getEnumByLabel(confPath + "/speed",
                                                "hundred");
            maapi.setElem(th, enum_speed, confPath + "/speed");

            // Init and set commit parameters
            log.info("Apply commit param Trace ID and dry-run with an action");
            CommitParams cp = maapi.getTransParams(th);
            cp.setTraceId("foobar");
            cp.setDryRunNative();

            // Detect transaction commit parameters
            log.info("Commit parameters: " + cp);
            if (cp.getTraceId() != null) {
                log.info("Commit trace ID detected: " + cp.getTraceId());
            }

            // Apply the transaction and print out the dry-run results
            ApplyResult result = maapi.applyTransParams(th, true, cp);
            log.info("Dry run output: ");
            if (result instanceof DryRunResult) {
                    DryRunResult dryrunResult = (DryRunResult) result;
                    Iterator<DryRunEntry> itr = dryrunResult.iterator();
                    while (itr.hasNext()) {
                            DryRunEntry element = itr.next();
                            log.info(element.getData());
                    }
            }

            maapi.finishTrans(th);
            maapi.endUserSession();
            socket.close();

          return null;
        } catch (Exception e) {
            throw new DpCallbackException("showcase failed", e);
        }
    }
}
