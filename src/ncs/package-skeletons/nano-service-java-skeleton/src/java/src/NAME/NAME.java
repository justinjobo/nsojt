package %JAVAPACKAGE%;

import %JAVAPACKAGE%.namespaces.*;
import java.util.List;
import java.util.Properties;
import com.tailf.conf.*;
import com.tailf.navu.*;
import com.tailf.ncs.ns.Ncs;
import com.tailf.dp.*;
import com.tailf.dp.annotations.*;
import com.tailf.dp.proto.*;
import com.tailf.dp.services.*;

/**
  NSO Nano service example.

Implements a Nano service callback
(C) 2023 Cisco Systems
Permission to use this code as a starting point hereby granted

See the README file for more information
 **/
public class %CLASS% {


     /**
     * Create callback method.
     * This method is called when a service instance committed due to a create
     * or update event for nano service.
     *
     * This method returns a opaque as a Properties object that can be null.
     * If not null it is stored persistently by Ncs.
     * This object is then delivered as argument to new calls of the create
     * method for the nano plan component.
     * This way the user can store and later modify persistent data outside
     * the service model that might be needed.
     *
     * @param context - The current NanoServiceContext object
     * @param service - The NavuNode references the service node.
     * @param ncsRoot - This NavuNode references the ncs root.
     * @param opaque  - Parameter contains a Properties object.
     *                  This object may be used to transfer
     *                  additional information between consecutive
     *                  calls to the create callback.  It is always
     *                  null in the first call. I.e. when the service
     *                  is first created.
     * @return Properties the returning opaque instance
     * @throws DpCallbackException
     */
    @NanoServiceCallback(servicePoint="%NAME%-servicepoint",
                        componentType="ncs:self",
                        state="%NAME%:vm-requested",
                        callType=NanoServiceCBType.CREATE)
    public Properties createSelfComponentVmRequestedState(
                                NanoServiceContext context,
                                NavuNode service,
                                NavuNode ncsRoot,
                                Properties opaque,
                                Properties componentProperties)
                                throws DpCallbackException {
        try{
            service.leaf("vm-up-and-running").set(new ConfBool(true));
        }catch (Exception e){
            e.printStackTrace();
        }
        // ...
        return opaque;
    }


     /**
     * Delete callback method.
     * This method is called when a service instance is deleted due to a delete
     * event for a service, which has nano plan.
     *
     * This method returns a opaque as a Properties object that can be null.
     * If not null it is stored persistently by Ncs.
     * This object is then delivered as argument to new calls of the delete
     * method for the nano plan component.
     * This way the user can store and later modify persistent/delete data
     * outside the service model that might be needed.
     *
     * @param context - The current NanoServiceContext object
     * @param service - The NavuNode references the service node.
     * @param ncsRoot - This NavuNode references the ncs root.
     * @param opaque  - Parameter contains a Properties object.
     *                  This object may be used to transfer
     *                  additional information between consecutive
     *                  calls to the create callback.  It is always
     *                  null in the first call. I.e. when the service
     *                  is first created.
     * @return Properties the returning opaque instance
     * @throws DpCallbackException
     */
    @NanoServiceCallback(servicePoint="%NAME%-servicepoint",
                         componentType="ncs:self",
                         state="%NAME%:vm-requested",
                         callType=NanoServiceCBType.DELETE)
    public Properties deleteSelfComponentVmRequestedState(
                            NanoServiceContext context,
                            NavuNode service,
                            NavuNode ncsRoot,
                            Properties opaque,
                            Properties componentProperties)
                            throws DpCallbackException {
                // ...
        return opaque;
    }

}
