"""NCS Alarm Manager module."""
import contextlib
import ncs
import _ncs


class CustomAttribute(object):
    """Class representing a custom attribute set on an alarm."""

    def __init__(self, prefix, tag, value):
        self.prefix = prefix
        self.tag = tag
        self.value = value


class CustomStatusAttribute(object):
    """Class representing a custom attribute set on an alarm."""

    def __init__(self, prefix, tag, value):
        self.prefix = prefix
        self.tag = tag
        self.value = value


class Alarm(object):
    """Class representing an alarm."""

    def __init__(self, managed_device, managed_object, alarm_type,
                 specific_problem, severity, alarm_text,
                 impacted_objects=None,
                 related_alarms=None,
                 root_cause_objects=None,
                 time_stamp=None,
                 custom_attributes=None):
        """Create an Alarm object.

        Arguments:
        managed_device
                The managed device this alarm is associated with. Plain string
                which identifies the device.
        managed_object
                The managed object this alarm is associated with. Also referred
                to as the "Alarming Object". This object may not be referred to
                in the root_cause_objects parameter. If an NCS Service
                generates an alarm based on an error state in a device used by
                that service, managed_object should be the service Id and the
                device should be included in the root_cause_objects list. This
                parameter must be a ncs.Value object. Use one of the methods
                managed_object_string(), managed_object_oid() or
                managed_object_instance() to create the value.
        alarm_type
                Type of alarm. This is a YANG identity. Alarm types are defined
                by the YANG developer and should be designed to be as specific
                as possible.
        specific_problem
                If the alarm_type isn't enough to describe the alarm, this
                field can be used in combination. Keep in mind that when
                dynamically adding a specific problem, there is no way for the
                operator to know in advance which alarms can be raised.
        severity
                State of the alarm; cleared, indeterminate, critical, major,
                minor, warning (enum).
        alarm_text
                A human readable description of this problem.
        impacted_objects
                A list of Managed Objects that may no longer function due to
                this alarm. Typically these point to NCS Services that are
                dependent on the objects on the device that reported the
                problem. In NCS 2.3 and later there is a backpointer attribute
                available on objects in the device tree that has been created by
                a Service. These backpointers are instance reference pointers
                that should be set in this list. Use one of the methods
                managed_object_string(), managed_object_oid() or
                managed_object_instance() to create the instances to populate
                this list.
        related_alarms
                References to other alarms that have been generated as a
                consequence of this alarm, or that has some other relationship
                to this alarm. Should be a list of AlarmId instances.
        root_cause_objects
                A list of Managed Objects that are likely to be the root cause
                of this alarm. This is different from the "Alarming Object". See
                managed_object above for details. Use one of the methods
                managed_object_string(), managed_object_oid() or
                managed_object_instance() to create the instances to populate
                this list.
        time_stamp
                A date-and-time when this alarm was generated.
        custom_attributes
                A list of custom leafs augmented into the alarm list.
        """
        if not managed_device:
            raise ValueError('managed_device must be set')
        if not managed_object:
            raise ValueError('managed_object must be set')
        if not alarm_type:
            raise ValueError('alarm_type must be set')

        self.managed_device = managed_device
        self.managed_object = managed_object
        self.alarm_type = alarm_type
        self.specific_problem = specific_problem
        self.severity = severity
        self.alarm_text = alarm_text
        self.impacted_objects = impacted_objects
        self.related_alarms = related_alarms
        self.root_cause_objects = root_cause_objects
        self.time_stamp = time_stamp or ncs.util.mk_yang_date_and_time()
        if custom_attributes:
            self.custom_attributes = {}
            for ca in custom_attributes:
                if isinstance(ca, CustomAttribute):
                    ca_type = ''
                else:
                    ca_type = 'status'
                self.custom_attributes[(ca_type, ca.prefix, ca.tag)] = ca
        else:
            self.custom_attributes = None

    def get_key(self):
        """Get alarm list key."""
        return (self.managed_device, self.alarm_type,
                self.managed_object.as_pyval(), self.specific_problem)
    key = property(get_key)

    def alarm_id(self):
        """Get the unique Id of this alarm as an AlarmId instance."""
        return AlarmId(self.alarm_type, self.managed_device,
                       self.managed_object, self.specific_problem)

    def add_attribute(self, prefix, tag, value):
        """Add or update custom attribute"""
        if self.custom_attributes is None:
            self.custom_attributes = {}
        ca = CustomAttribute(prefix, tag, value)
        self.custom_attributes[('', prefix, tag)] = ca

    def add_status_attribute(self, prefix, tag, value):
        """Add or update custom status change attribute"""
        if self.custom_attributes is None:
            self.custom_attributes = {}
        ca = CustomStatusAttribute(prefix, tag, value)
        self.custom_attributes[('status', prefix, tag)] = ca

    def __eq__(self, other):
        """Check for equality.

        Arguments:
            other -- An instance of Alarm or AlarmId
        """
        return (self.alarm_type == other.alarm_type and
                self.managed_device == other.managed_device and
                self.managed_object == other.managed_object and
                self.specific_problem == other.specific_problem)


class AlarmId(object):
    """Represents the unique Id of an Alarm."""

    def __init__(self, alarm_type, managed_device, managed_object,
                 specific_problem=None):
        """Create an AlarmId."""
        self.alarm_type = alarm_type
        self.managed_device = managed_device
        self.managed_object = managed_object
        self.specific_problem = specific_problem

    def __str__(self):
        """String representation of an AlarmId."""
        return 'AlarmId[{}, {}, {}{}]'.format(
            self.managed_device, self.alarm_type, self.managed_object,
            self.specific_problem and ', {}'.format(
                self.specific_problem) or '')

    def __eq__(self, other):
        """Check for equality.

        Arguments:
            other -- An instance of Alarm or AlarmId
        """
        return (self.alarm_type == other.alarm_type) and \
               (self.managed_device == other.managed_device) and \
               (self.managed_object == other.managed_object) and \
               (self.specific_problem == other.specific_problem)


def managed_object_string(strval):
    """Create a managed object of type string.

    Arguments:
        strval --- The string value
    """
    return ncs.Value(strval, ncs.C_BUF)


def managed_object_oid(oidval):
    """Create a managed object of type yang:object-identifier.

    Arguments:
        oidval -- The OID (string)
    """
    return ncs.Value(oidval, ncs.C_OID)


def managed_object_instance(instanceval):
    """Create a managed object of type instance-identifier.

    Arguments:
        instanceval -- The instance-identifier (string or HKeypathRef)
    """
    if isinstance(instanceval, ncs.HKeypathRef):
        hkp = instanceval
    else:
        with ncs.maapi.Maapi() as m:
            hkp = m.xpath2kpath(instanceval)
    return ncs.Value(hkp, ncs.C_OBJECTREF)


def raise_alarm(alarm):
    """Raise an alarm.

    Arguments:
        alarm -- An instance of Alarm.
    """
    with _alarm_list_trans(alarm) as al:
        if al is not None:
            al.last_changed = alarm.time_stamp
            if alarm.key in al.alarm:
                a = al.alarm[alarm.key]
            else:
                a = al.alarm.create(alarm.managed_device,
                                    alarm.alarm_type,
                                    alarm.managed_object,
                                    alarm.specific_problem)

            a.last_status_change = alarm.time_stamp
            if alarm.severity == 'cleared':
                a.is_cleared = True
            else:
                a.is_cleared = False
                a.last_perceived_severity = alarm.severity
                if alarm.alarm_text:
                    a.last_alarm_text = alarm.alarm_text

            _set_custom_attributes(a, alarm.custom_attributes, '')

            # related alarms
            if alarm.related_alarms is not None:
                for r in alarm.related_alarms:
                    a.related_alarms.create(r.managed_device,
                                            r.alarm_type,
                                            r.managed_object,
                                            r.specific_problem)
            # status change attributes
            sc = a.status_change.create(alarm.time_stamp)
            sc.received_time = ncs.util.mk_yang_date_and_time()
            sc.perceived_severity = alarm.severity
            sc.alarm_text = alarm.alarm_text
            _set_custom_attributes(sc, alarm.custom_attributes, 'status')


def clear_alarm(alarm):
    """Clear an alarm.

    Arguments:
        alarm -- An instance of Alarm.
    """
    alarm.severity = 'cleared'
    with _alarm_list_trans(alarm) as al:
        if alarm.key in al.alarm:
            a = al.alarm[alarm.key]
            a.is_cleared = True


def _set_custom_attributes(obj, attributes, include_ca_type):
    if attributes:
        for (ca_type, ca_prefix, ca_tag), ca in attributes.items():
            if ca_type == include_ca_type:
                key = '{}__{}'.format(ca_prefix, ca_tag)
                setattr(obj, key, ca.value)


@contextlib.contextmanager
def _alarm_list_trans(alarm, *args, **kwds):
    with ncs.maapi.single_write_trans('', 'system', db=ncs.OPERATIONAL) as t:
        root = ncs.maagic.get_root(t)
        if _should_send_alarm(root, alarm):
            yield root.al__alarms.alarm_list
            t.apply()
        else:
            yield None


def _should_send_alarm(root, alarm):
    if alarm.severity == 'cleared':
        return True
    else:
        csnode = _ncs.cs_node_cd(None, '/al:alarms/control/filter-level')
        cstype = csnode.info().type()
        v = _ncs.Value.str2val(alarm.severity, cstype)
        return v >= root.al__alarms.control.filter_level.value
