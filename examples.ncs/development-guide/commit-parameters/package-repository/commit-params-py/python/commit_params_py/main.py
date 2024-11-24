# -*- mode: python; python-indent: 4 -*-
import ncs, _ncs
from ncs.application import Service
from ncs.dp import Action

# ------------------------
# SERVICE CALLBACK EXAMPLE
# ------------------------
class ServiceCallbacks(Service):

    # The create() callback is invoked inside NCS FASTMAP and
    # must always exist.
    @Service.create
    def cb_create(self, tctx, root, service, proplist):
        self.log.info('Service create(service=', service._path, ')')

        # Read transaction parameters
        maapi = ncs.maagic.get_maapi(root)
        trans = maapi.attach(tctx)
        trans_param = trans.get_trans_params()

        for p in trans_param:
          self.log.info(f"Transaction param found: {_ncs.hash2str(p.tag)}")

        # Alternative
        params = trans.get_params()
        self.log.info(f"All transaction params: {params}")

        # Detect specific transaction commit parameters
        if params.is_dry_run():
            self.log.info(f"Dry run detected!")

        tvars = ncs.template.Variables()
        template = ncs.template.Template(service)
        template.apply('commit-params-py-template', tvars)

class ShowcaseCommitParams(Action):

    @Action.action
    def cb_action(self, uinto, name, keypath, ainput, aoutput):
        # Start a MAAPI transaction
        with ncs.maapi.single_write_trans('admin', 'python') as t:
            # Perform some configuration changes on the device
            root = ncs.maagic.get_root(t)
            device = root.devices.device['ex0']
            interfaces = device.config.sys.interfaces.interface
            interface = interfaces.create('GigabitEthernet1')
            interface.enabled
            interface.speed = 'hundred'

            # Init and set commit parameters
            params = t.get_params()
            self.log.info(
                f"Apply commit parameter Trace ID and dry-run with an action")
            params.trace_id("foobar")
            params.dry_run_native()

            # Display commit params
            self.log.info(f"All transaction params: {params}")
            if params.get_trace_id() is not None:
                self.log.info(
                    f"Commit trace ID detected: {params.get_trace_id()}")

            # Apply the transaction and print out the dry-run results
            result = t.apply_params(True, params)
            self.log.info(f"Dry run output:")
            self.log.info(result['device']['ex0'])

            # A full list of commit parameters with their explanations
            # can be found in NSO Python API documentation
            #commit_queue_async
            #commit_queue_atomic
            #commit_queue_block_others
            #commit_queue_bypass
            #commit_queue_error_option
            #commit_queue_lock
            #commit_queue_non_atomic
            #commit_queue_sync
            #commit_queue_tag
            #dry_run_cli
            #dry_run_cli_c
            #dry_run_cli_c_reverse
            #dry_run_native
            #dry_run_native_reverse
            #dry_run_xml
            #no_deploy
            #no_lsa
            #no_networking
            #no_out_of_sync_check
            #no_overwrite
            #no_revision_drop
            #reconcile_discard_non_service_config
            #reconcile_keep_non_service_config
            #set_dry_run_outformat
            #trace_id
            #use_lsa
            #wait_device

# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------
class Main(ncs.application.Application):
    def setup(self):
        # The application class sets up logging for us. It is accessible
        # through 'self.log' and is a ncs.log.Log instance.
        self.log.info('Main RUNNING')

        # Service callbacks require a registration for a 'service point',
        # as specified in the corresponding data model.
        #
        self.register_service('commit-params-py-servicepoint', ServiceCallbacks)

        # Register action
        self.register_action('showcase-py', ShowcaseCommitParams)
        # If we registered any callback(s) above, the Application class
        # took care of creating a daemon (related to the service/action point).

        # When this setup method is finished, all registrations are
        # considered done and the application is 'started'.

    def teardown(self):
        # When the application is finished (which would happen if NCS went
        # down, packages were reloaded or some error occurred) this teardown
        # method will be called.

        self.log.info('Main FINISHED')
