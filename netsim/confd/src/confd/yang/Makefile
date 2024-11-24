CONFDC=$(CONFD_DIR)/bin/confdc

YANG_SOURCES        = $(wildcard *.yang)
SUBMODULES          = $(shell grep -l belongs-to *.yang)
ANNOTATION_MODULES  = $(wildcard *-ann.yang)
DEVIATION_MODULES   = $(wildcard *-deviation.yang)

BUILTIN_MODULES     += ietf-yang-types.yang
BUILTIN_MODULES     += ietf-inet-types.yang

BUILTIN_MODULES     += ietf-datastores.yang
BUILTIN_MODULES     += ietf-yang-library.yang

BUILTIN_MODULES     += ietf-netconf.yang
BUILTIN_MODULES     += ietf-netconf-with-defaults.yang

BUILTIN_MODULES     += ietf-restconf.yang
BUILTIN_MODULES     += ietf-yang-patch.yang
BUILTIN_MODULES     += tailf-restconf-error.yang
BUILTIN_MODULES     += tailf-rest-query.yang

YANG_MODULES = $(filter-out $(SUBMODULES) $(ANNOTATION_MODULES) \
		  $(BUILTIN_MODULES) $(DEVIATION_MODULES),$(YANG_SOURCES))

ALL_FXS_FILES = $(YANG_MODULES:%.yang=%.fxs)

all: $(ALL_FXS_FILES)

# The following three YANG modules are only used by ConfD for import,
# from ietf-subscribed-notifications.  It is perfectly fine to
# replace these fxs files with custom-compiled fxs files, if they are
# implemented by the custom code.
ietf-interfaces.fxs: ietf-interfaces.yang
	$(CONFDC) --conformance import --fail-on-warnings \
	-c -o $@ $<
ietf-ip.fxs: ietf-ip.yang
	$(CONFDC) --conformance import --fail-on-warnings \
	-c -o $@ $<
ietf-network-instance.fxs: ietf-network-instance.yang
	$(CONFDC) --conformance import --fail-on-warnings \
	-c -o $@ $<

ietf-subscribed-notifications.fxs: ietf-subscribed-notifications.yang \
	ietf-subscribed-notifications-ann.yang \
	ietf-subscribed-notifications-deviation.yang
	$(CONFDC) -c --export netconf --internal-exs -o $@ \
	  -F ietf-subscribed-notifications:encode-xml,replay,subtree,xpath \
	  -a ietf-subscribed-notifications-ann.yang \
	  --deviation ietf-subscribed-notifications-deviation.yang -- $<

ietf-yang-push.fxs: ietf-yang-push.yang ietf-yang-push-ann.yang \
	ietf-yang-push-deviation.yang
	$(CONFDC) -c --export netconf --internal-exs -o $@ \
	  -F ietf-subscribed-notifications:encode-xml,replay,subtree,xpath \
	  -a ietf-yang-push-ann.yang \
	  --deviation ietf-yang-push-deviation.yang -- $<

# in NCS we implement schema mount, but in ConfD we just import it
ifeq ($(BUILDNCS),true)
ietf-yang-schema-mount.fxs: ietf-yang-schema-mount.yang \
		ietf-yang-schema-mount-ann.yang
	$(CONFDC) --fail-on-warnings -c --export netconf --export rest \
	-o $@ -a ietf-yang-schema-mount-ann.yang -- $<
else
ietf-yang-schema-mount.fxs: ietf-yang-schema-mount.yang \
		ietf-yang-schema-mount-ann.yang
	$(CONFDC) --fail-on-warnings -c --conformance import \
	--export netconf --export rest \
	-o $@ -a ietf-yang-schema-mount-ann.yang -- $<
endif

ietf-netconf-nmda.fxs: ietf-netconf-nmda.yang
	$(CONFDC) -c --export netconf --internal-exs -o $@ \
	-F ietf-netconf-nmda:with-defaults,origin -- $<

tailf-common-monitoring.fxs: tailf-common-monitoring.yang
	$(CONFDC) -c -o $@ -- $<

tailf-%-monitoring.fxs: tailf-%-monitoring.yang \
	tailf-common-monitoring2.yang \
	tailf-%-monitoring-ann.yang \
	tailf-common-monitoring2-ann.yang
	$(CONFDC) -c -o $@ \
	  -a tailf-$*-monitoring-ann.yang \
	  -a tailf-common-monitoring2-ann.yang -- $<

tailf-common-monitoring2.fxs: tailf-common-monitoring2.yang
	$(CONFDC) -c -o $@ \
	  -a tailf-common-monitoring2-ann.yang -- $<

tailf-%-monitoring2.fxs: tailf-%-monitoring2.yang \
	tailf-%-monitoring.yang \
	tailf-common-monitoring2.yang \
	tailf-%-monitoring-ann.yang \
	tailf-common-monitoring2-ann.yang
	$(CONFDC) --fail-on-warnings -c -o $@ \
	  -a tailf-$*-monitoring-ann.yang \
	  -a tailf-common-monitoring2-ann.yang -- $<

tailf-progress.fxs:  tailf-progress.yang \
	tailf-progress-ann.yang
	$(CONFDC) --use-description -c -o $@ \
	-a tailf-progress-ann.yang -- $<

tailf-%-progress.fxs:  tailf-%-progress.yang
	$(CONFDC) --use-description -c -o $@ -- $<

ietf-netconf-partial-lock.fxs: ietf-netconf-partial-lock.yang
	$(CONFDC) --internal-exs --export netconf -c -o $@ -- $<

tailf-netconf-%.fxs: tailf-netconf-%.yang
	$(CONFDC) --internal-exs --export netconf -c -o $@ -- $<

%.fxs: %.yang %-ann.yang %-deviation.yang
	$(CONFDC) -c -o $@ -a $*-ann.yang --deviation $*-deviation.yang -- $<

%.fxs: %.yang %-deviation.yang
	$(CONFDC) -c -o $@ -a --deviation $*-deviation.yang -- $<

%.fxs: %.yang %-ann.yang
	$(CONFDC) -c -o $@ -a $*-ann.yang -- $<

%.fxs: %.yang
	$(CONFDC) -c -o $@ -- $<

clean:
	-rm -f *.fxs 2>/dev/null
