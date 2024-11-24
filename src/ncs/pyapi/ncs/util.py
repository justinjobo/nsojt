"""Utility module, low level abstrations"""

import contextlib
import datetime
import os
import select
import signal
import threading
import time
import traceback

import _ncs


# global self assign warning type for this python vm
_self_assign_warning = 'alarm'
_self_assign_data = threading.local()


class _TZ(datetime.tzinfo):
    def utcoffset(self, dt):
        s = (time.altzone if time.daylight else time.timezone)
        return datetime.timedelta(seconds=-s)


def mk_yang_date_and_time(dt=None):
    """Create a timezone aware datetime object in ISO8601 string format.

    This method is used to convert a datetime object to its timezone aware
    counterpart and return a string useful for a 'yang:date-and-time' leaf.
    If 'dt' is None the current time will be used.

    Arguments:
        dt -- a datetime object to be converted (optional)
    """
    dt = dt or datetime.datetime.now()
    d = dt.date()
    t = dt.time()
    dt = datetime.datetime(d.year, d.month, d.day,
                           t.hour, t.minute, t.second, t.microsecond,
                           tzinfo=_TZ())

    return dt.isoformat()


def is_multiprocessing():
    """Return True if the configured callpoint model is multiprocessing"""
    return get_callpoint_model() == 'multiprocessing'


def get_callpoint_model():
    """Get configured callpoint model"""
    return os.getenv('NCS_PYTHON_CALLPOINT_MODEL', 'threading')


def set_callpoint_model(model):
    """Update environment with provided callpoint model"""
    if model not in ('threading', 'multiprocessing'):
        raise ValueError(
            '{} unsupported callpoint model. '
            'threading and multiprocessing supported'.format(model))
    os.environ['NCS_PYTHON_CALLPOINT_MODEL'] = model


class _KQueueParentObserver(threading.Thread):
    """Thread waiting for the parent process to exit and then runs
    on_exit_fun"""

    def __init__(self, on_exit_fun):
        super(_KQueueParentObserver, self).__init__(daemon=True)
        self._ppid = os.getppid()
        self._on_exit_fun = on_exit_fun

    def run(self):
        # wait for parent process to exit
        ev = select.kevent(self._ppid,
                           filter=select.KQ_FILTER_PROC,
                           fflags=select.KQ_NOTE_EXIT)
        kq = select.kqueue()
        try:
            kq.control([ev], 1)
        finally:
            kq.close()

        self._on_exit_fun()


def set_kill_child_on_parent_exit():
    """Multi OS variant of _ncs.set_kill_child_on_parent_exit falling back
    to kqueue if the OS supports it."""

    if _ncs.set_kill_child_on_parent_exit():
        return True

    if hasattr(select, 'kqueue'):
        pid = os.getpid()
        obs = _KQueueParentObserver(lambda: os.kill(pid, signal.SIGKILL))
        obs.start()
        return True

    return False


def get_self_assign_warning():
    """Return current self assign warning type."""
    global _self_assign_warning
    return _self_assign_warning


def set_self_assign_warning(warning):
    """Set self assign warning type."""
    global _self_assign_warning
    _self_assign_warning = warning


@contextlib.contextmanager
def with_setattr_check(path):
    """Use as context manager enabling set attribute check for the
    current thread while in the manager."""
    _self_assign_data.setattr_check = path
    try:
        yield
    finally:
        _self_assign_data.setattr_check = None


def get_setattr_fun(obj, parent):
    """Return setattr fun to use for setting attributes, will use
    return a wrapped setattr function with sanity checks if enabled."""
    path = getattr(_self_assign_data, 'setattr_check', None)
    if path is None:
        return super(parent, obj).__setattr__

    setattr_op = get_self_assign_warning()
    if setattr_op == 'alarm':
        def setattr_fun(name, value):
            return _setattr_alarm(obj, parent, path, name, value)
    elif setattr_op == 'log':
        def setattr_fun(name, value):
            return _setattr_log(obj, parent, path, name, value)
    else:
        setattr_fun = super(parent, obj).__setattr__
    return setattr_fun


def _setattr_alarm(obj, parent, path, name, value):
    from . import alarm
    code_info = _retreive_self_assign()

    _alarm_text = "Assigning to self is not thread safe: {}\n{}"
    _alarm = alarm.Alarm(managed_device='ncs',
                         managed_object=alarm.managed_object_instance(path),
                         alarm_type='package-operation-failure',
                         specific_problem='',
                         severity='critical',
                         alarm_text=_alarm_text.format(path, code_info))
    alarm.raise_alarm(_alarm)

    super(parent, obj).__setattr__(name, value)


def _setattr_log(obj, parent, path, name, value):
    if hasattr(obj, 'log'):
        code_info = _retreive_self_assign()

        _log_text = "Assigning to self is not thread safe: {}\n{}"
        obj.log.warning(_log_text.format(path, code_info))
    super(parent, obj).__setattr__(name, value)

def _retreive_self_assign():
    stack = traceback.extract_stack()
    # we take the 4th index of the reversed stack as follows:
    # 0: _retreive_self_assign
    # 1: _setattr_alarm
    # 2: _setattr_fun
    # 3: __setattr__
    # 4: the line of code invoking the setattr
    file_name, line_number, function_name, code_line = list(reversed(stack))[4]
    fmtstr = "   File: {}, Line: {}\n    Function: {}, Code: {}"
    return fmtstr.format(file_name, line_number, function_name, code_line)

if __name__ == '__main__':
    import struct
    import sys
    import time
    import unittest

    class CallpointModelTest(unittest.TestCase):
        def test_set_invalid_callpoint_model(self):
            with self.assertRaises(ValueError) as cm:
                set_callpoint_model('invalid')
                self.assertEqual('invalid unsupported callpoint model. '
                                 'threading and multiprocessing supported',
                                 str(cm.exception))

        def test_no_env_is_multiprocessing(self):
            if 'NCS_PYTHON_CALLPOINT_MODEL' in os.environ:
                del os.environ['NCS_PYTHON_CALLPOINT_MODEL']
            self.assertFalse(is_multiprocessing())

        def test_threading_env_is_multiprocessing(self):
            set_callpoint_model('threading')
            self.assertFalse(is_multiprocessing())

        def test_multiprocessing_env_is_multiprocessing(self):
            set_callpoint_model('multiprocessing')
            self.assertTrue(is_multiprocessing())


    class SetKillChildOnParentExitTest(unittest.TestCase):
        def test_exit_child(self):
            """Test verifying standard child exit (and not doing so
            prematurely)"""
            read_fd, write_fd = os.pipe()
            pid = os.fork()
            if pid == 0:
                self.assertTrue(set_kill_child_on_parent_exit())
                os.write(write_fd, b'i')
                os.close(write_fd)

                # child, wait for input on pipe for a controlled
                # shutdown
                os.read(read_fd, 1)
                return 0

            # sync startup and ensure child is running
            os.read(read_fd, 1)
            os.close(read_fd)
            os.kill(pid, 0)

            # notify child to exit
            os.write(write_fd, b'x')
            os.close(write_fd)

            (_, res) = os.waitpid(pid, 0)
            self.assertEqual(0, os.WEXITSTATUS(res))

        def test_kill_parent(self):
            """Test verifying child exit after killing the parent"""
            pread_fd, pwrite_fd = os.pipe()
            ppid = os.fork()
            if ppid == 0:
                # double fork to not kill the unittest
                read_fd, write_fd = os.pipe()
                pid = os.fork()
                if pid == 0:
                    self.assertTrue(set_kill_child_on_parent_exit())
                    os.write(write_fd, b'i')
                    os.close(write_fd)
                    time.sleep(3600)
                    sys.exit(0)

                # sync startup, set_kill_child_on_parent_exit must be
                # called before to get ProcessLookupError triggering
                os.read(read_fd, 1)

                # write grand-child pid, will be used to verify that
                # the grand-child indeed got killed.
                os.write(pwrite_fd, struct.pack('i', pid))
                os.close(pwrite_fd)
                time.sleep(3600)
                sys.exit(0)

            gc_pid_packed = os.read(pread_fd, len(struct.pack('i', 0)))
            (gc_pid, ) = struct.unpack('i', gc_pid_packed)
            os.close(pwrite_fd)
            os.close(pread_fd)

            # kill child process, the grand childprocess should go
            # down in the process
            os.kill(ppid, signal.SIGKILL)
            (_, res) = os.waitpid(ppid, 0)
            self.assertEqual(0, os.WEXITSTATUS(res))

            # wait for the os to clean up the process (using time, better way?)
            time.sleep(1)

            # send signal 0, should return error if the process is
            # missing.
            with self.assertRaises(ProcessLookupError):
                os.kill(gc_pid, 0)

    unittest.main()
