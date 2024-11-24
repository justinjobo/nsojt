#
# ©2023 Cisco Systems, Inc. and/or its affiliates. All rights reserved.
#
"""
MAAPI progress trace high level module.

This module defines a high level interface to the low-level maapi functions.

In the Progress Trace a span is used to meassure duration of an event, the
'start' and 'stop' messages in the progress trace log:

start,2023-08-28T10:42:51.249865,,,,45,306,running,cli,,"foobar"...
...
stop,2023-08-28T10:42:51.284359,0.034494,,,45,306,running,cli,,"foobar"...

maapi.Transaction.start_progress_span() and
maapi.Maapi.start_progress_span() return progress.Span objects, which
contains the span_id and trace_id (if enabled) attributes. Once the object
is deleted/exited or manually obj.end() is called the stop message is
written to the progress trace.

Inside a span multiple sub spans can be created, sp2 in the below example.

    import ncs

    m = ncs.maapi.Maapi()
    m.start_user_session('admin', my context')
    t = m.start_read_trans()
    sp1 = t.start_progress_span('first span')
    t.progress_info('info message')
    sp2 = t.start_progress_span('second span')
    sp2.end()
    sp1.end()

Another way is to use context managers, which will handle all cleanup
related to transactions, user sessions and socket connections:

    with ncs.maapi.Maapi() as m:
        m.start_user_session('admin', my context')
        with m.start_read_trans() as t:
            with t.start_progress_span('first span'):
                t.progress_info('info message')
                with t.start_progress_span('second span'):
                    pass

Finally, a really compact way of doing this:

    with ncs.maapi.single_read_trans('admin', 'my context') as t:
        with t.start_progress_span('first span'):
            t.progress_info('info message')
            with t.start_progress_span( 'second span')
                pass

There are multiple optional fields.

    with ncs.maapi.single_read_trans('admin', 'my context') as t:
        with t.start_progress_span('calling foo',
                                   attrs={'sys':'Linux', 'hostname':'bob'}):
            foo()

    with ncs.maapi.Maapi() as m:
        m.start_user_session('admin', 'my context')
        action = '/devices/device{ex0}/sync-from'
        with m.start_progress_span('copy running from ex0', path=action):
            m.request_action([], 0, action)

    # trace_id1 from an already existing trace
    trace_id1 = 'b1ce20b4-0ca4-4a3e-a448-8df860e622e0'
    with ncs.maapi.single_read_trans('admin', 'my context') as t:
        with t.start_progress_span('perform op related to old trace',
                                   links=[{'trace_id':trace_id1]}):
            pass

"""

from . import tm

_tm = __import__(tm.TM)

class Span:
    def __init__(self, msock, span_id, trace_id=None):
        self._finished = False
        self.msock = msock
        self.span_id = span_id
        self.trace_id = trace_id

    def __del__(self):
        if not self._finished:
            try:
                self.end()
            except Exception:
                pass

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, tb):
        if not self._finished:
            try:
                self.end()
            except Exception:
                pass

    # TODO: test progress span leak if socket is closed
    def end(self, annotation=None):
        """ends a span, the stop event in the progress trace. this function
        is called automatically when the span is deleted i.e. when exiting a
        'with' context.

        * annotation -- sets the annotation field for stop events (str)
        """
        # the below call bypasses the high-level API
        # maapi.py:/^    def end_progress_span\(/
        # and instead calls the low-level API directly in
        # ../src/_maapi.c:/^EXT_API_FUN\(_maapi_end_progress_span,/
        _tm.maapi.end_progress_span(self.msock, self.span_id, annotation)
        self._finished = True

class EmptySpan(Span):
    def __init__(self, span_id=None, trace_id=None):
        self._finished = False
        super().__init__(-1, span_id, trace_id)

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, tb):
        pass

    def end(self, *args):
        "not implemented. no span to end."
        raise NotImplementedError

def conv_links(links):
    """convert from [Span() | dict()] -> [dict()]"""
    return [_conv_link(x) for x in links]

def _conv_link(blob):
    if isinstance(blob, dict):
        d = blob
    else:
        d = {'trace_id':blob.trace_id, 'span_id':blob.span_id}
    return _del_none(d)

def _del_none(d):
    return {k:d[k] for k in d if d[k] != None}
