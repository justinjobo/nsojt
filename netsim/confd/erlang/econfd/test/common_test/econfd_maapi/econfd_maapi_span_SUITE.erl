%%% ----------------------------------------------------------------------------
%%% @doc Erlang Common Test of progress spans in econfd_maapi
%%%
%%% ----------------------------------------------------------------------------
-module(econfd_maapi_span_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("econfd/include/econfd.hrl").

%% -----------------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%% -----------------------------------------------------------------------------
all() ->
    [progress_span,
     progress_span_attr,
     progress_span2,
     progress_info2,
     progress_span_attr_th,
     progress_span_th2,
     progress_span_links].

init_per_suite(Config) ->
    case whereis(confd_server) of
        undefined -> confd:start();
        _ -> ok
    end,
   Config.

end_per_suite(_Config) ->
    ok.

%% -----------------------------------------------------------------------------
%% TEST CASES
%% -----------------------------------------------------------------------------
progress_span() ->
    [{userdata, [{description,
        "smoke test. start and end a progress span. verify message
         was correctly received for both the start and stop under
         /progress/trace/event/message oper data"}]}].

progress_span(_Config) ->
    {ok, Sock} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
    ok = econfd_maapi:start_user_session(Sock, <<"admin">>, <<"system">>, [], {1,2,3,4}, 0),
    try
        {ok, {SpanId1, TraceId}} =
            econfd_maapi:start_progress_span(Sock, 0, <<"hello world">>, [], [], []),
        ?assertNotEqual(TraceId, undefined),
        {ok, {_SpanId0 = undefined, _}} =
            econfd_maapi:end_progress_span(Sock, SpanId1,
                ["some ", <<"men">>, " just want to watch the world ",
                [[[<<"burn">>]]]]),

        {ok, L1} = with_rtrans(Sock, fun(S0, Tid0) -> match_msg(S0, Tid0, SpanId1) end),
        [{_, <<"hello world">>}, {_, <<"hello world">>}] = L1,
        ok
    after
        econfd_maapi:close(Sock)
    end.

progress_span_attr() ->
    [{userdata, [{description,
        "start a progress span with attributes and service ikeypath
         set and verify the values of the attributes in the
         /progress/trace/event/attribute oper data"}]}].

progress_span_attr(_Config) ->
    {ok, Sock} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
    ok = econfd_maapi:start_user_session(Sock, <<"admin">>, <<"system">>, [], {1,2,3,4}, 0),
    try
        SIKP = [{<<"alice">>}, ['http://tail-f.com/ns/aaa/1.1'|user]],
        {ok, {SpanId1, TraceId}} =
            econfd_maapi:start_progress_span(Sock, 0, <<"hello world">>, SIKP,
                _Attributes = [{<<"foo">>, <<"bar">>},
                    {<<"device">>, <<"bob">>}],
                _Links = []),
        ?assertNotEqual(TraceId, undefined),
        timer:sleep(39),
        {ok, {_SpanId0 = undefined, _}} =
            econfd_maapi:end_progress_span(Sock, SpanId1, "bye world"),

        {ok, LS1} = with_rtrans(Sock, fun(S0, Tid0) ->
            match_attr(S0, Tid0, SpanId1, <<"service">>) end),
        SHKP = cs:ikeypath2hkeypath(SIKP),
        [{_, ?CONFD_INSTANCE_IDENTIFIER(SHKP)},
         {_, ?CONFD_INSTANCE_IDENTIFIER(SHKP)}] = LS1,
        {ok, LB1} = with_rtrans(Sock, fun(S0, Tid0) ->
            match_attr(S0, Tid0, SpanId1, <<"foo">>) end),
        [{_, <<"bar">>}, {_, <<"bar">>}] = LB1,
        {ok, LI1} = with_rtrans(Sock, fun(S0, Tid0) ->
            match_attr(S0, Tid0, SpanId1, <<"device">>) end),
        [{_, <<"bob">>}, {_, <<"bob">>}] = LI1,
        ok
    after
        econfd_maapi:close(Sock)
    end.

progress_span2() ->
    [{userdata, [{description,
        "start two progress spans. verify the child span has
         parent-span-id set to its parent in the
         /progress/trace/event/parent-span-id oper data"}]}].

progress_span2(_Config) ->
    {ok, Sock} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
    ok = econfd_maapi:start_user_session(Sock, <<"admin">>, <<"system">>, [], {1,2,3,4}, 0),
    try
        {ok, {SpanId1, TraceId}} =
            econfd_maapi:start_progress_span(Sock, 0, ["parent ", <<"span">>],
                _SIKP1 = [], _Attributes1 = [], _Links1 = []),
        {ok, {SpanId2, TraceId}} =
            econfd_maapi:start_progress_span(Sock, 0, "child span",
                _SIKP2 = [], _Attributes2 = [], _Links2 = []),
        {ok, {SpanId1, TraceId}} =
            econfd_maapi:end_progress_span(Sock, SpanId2, "bye child"),
        {ok, {undefined, _}} =
            econfd_maapi:end_progress_span(Sock, SpanId1, "bye parent"),

       {ok, L1} = with_rtrans(Sock, fun(S0, Tid0) -> match_msg(S0, Tid0, SpanId1) end),
        [{_, <<"parent span">>},
         {_, <<"parent span">>}] = L1,
        {ok, L2} = with_rtrans(Sock, fun(S0, Tid0) -> match_msg(S0, Tid0, SpanId2) end),
        [{_, <<"child span">>},
         {_, <<"child span">>}] = L2,
        {ok, LParentSpan2} = with_rtrans(Sock, fun(S0, Tid0) ->
            match_parent_span(S0, Tid0, SpanId2) end),
        [{_, SpanId1},
         {_, SpanId1}] = LParentSpan2,
        ok
    after
        econfd_maapi:close(Sock)
    end.

progress_info2() ->
    [{userdata, [{description,
        "start an info message inside a progress span.
         verify the info message has same span-id as the span under
         /progress/trace/event/span-id oper data"}]}].

progress_info2(_Config) ->
    {ok, Sock} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
    ok = econfd_maapi:start_user_session(Sock, <<"admin">>, <<"system">>, [], {1,2,3,4}, 0),
    try
        {ok, {SpanId1, TraceId}} =
            econfd_maapi:start_progress_span(Sock, 0, "parent span",
                _SIKP1 = [], _Attributes1 = [], _Links1 = []),
        ?assertNotEqual(TraceId, undefined),
        ok = econfd_maapi:progress_info(Sock, 0, "info message",
            _SIKP2 = [], _Attributes2 = [], _Links2 = []),
        {ok, {_SpanId0 = undefined, _}} =
            econfd_maapi:end_progress_span(Sock, SpanId1, "bye parent"),

       {ok, L1} = with_rtrans(Sock, fun(S0, Tid0) -> match_msg(S0, Tid0, SpanId1) end),
       [{_, <<"parent span">>},
        {_, <<"info message">>},
        {_, <<"parent span">>}] = L1,
       ok
     after
        econfd_maapi:close(Sock)
    end.

progress_span_attr_th() ->
    [{userdata, [{description,
        "start a progress span associated with a transaction.
         service ikeypath and other attributes are set.
         verify the values of the attributes under the
         /progress/trace/event/attribute oper data"}]}].

progress_span_attr_th(_Config) ->
    {ok, Sock} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
    ok = econfd_maapi:start_user_session(Sock, <<"admin">>, <<"system">>, [], {1,2,3,4}, 0),
    {ok, Tid} = econfd_maapi:start_trans(Sock, ?CONFD_RUNNING, ?CONFD_READ),
    try
        Msg = <<"hello world">>,
        SIKP = [{<<"alice">>}, ['http://tail-f.com/ns/aaa/1.1'|user]],
        {ok, {SpanId1, TraceId}} =
            econfd_maapi:start_progress_span_th(Sock, Tid, 0, Msg, SIKP,
                _Attributes = [{<<"b">>, <<"x">>},
                    {<<"i">>, 5}],
                _Links = []),
        ?assertNotEqual(TraceId, undefined),
        {ok, {_SpanId0 = undefined, _}} =
            econfd_maapi:end_progress_span(Sock, SpanId1, "bye world"),

        {ok, L1} = with_rtrans(Sock, fun(S0, Tid0) -> match_msg(S0, Tid0, SpanId1) end),
        [{_, Msg}, {_, Msg}] = L1,
        {ok, LS1} = with_rtrans(Sock, fun(S0, Tid0) ->
            match_attr(S0, Tid0, SpanId1, <<"service">>) end),
        SHKP = cs:ikeypath2hkeypath(SIKP),
        [{_, ?CONFD_INSTANCE_IDENTIFIER(SHKP)},
         {_, ?CONFD_INSTANCE_IDENTIFIER(SHKP)}] = LS1,
        {ok, LB1} = with_rtrans(Sock, fun(S0, Tid0) ->
            match_attr(S0, Tid0, SpanId1, <<"b">>) end),
        [{_, <<"x">>}, {_, <<"x">>}] = LB1,
        {ok, LI1} = with_rtrans(Sock, fun(S0, Tid0) ->
            match_attr(S0, Tid0, SpanId1, <<"i">>) end),
        [{_, ?CONFD_UINT64(5)}, {_, ?CONFD_UINT64(5)}] = LI1,
        ok
    after
        econfd_maapi:finish_trans(Sock, Tid),
        econfd_maapi:close(Sock)
    end.

progress_span_th2() ->
    [{userdata, [{description,
        "start a progress span associated with the user session
         and one more progress span associated with a transaction.
         verify the latter span is a child span of the former from
         the /progress/trace/event/parent-span-id oper data"}]}].

progress_span_th2(_Config) ->
    {ok, Sock} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
    ok = econfd_maapi:start_user_session(Sock, <<"admin">>, <<"system">>, [], {1,2,3,4}, 0),
    try
        {ok, {SpanId1, TraceId}} =
            econfd_maapi:start_progress_span(Sock, 0, "usess 1", [], [], []),
        {ok, Tid} = econfd_maapi:start_trans(Sock, ?CONFD_RUNNING, ?CONFD_READ),
        {ok, {SpanId2, TraceId}} =
            econfd_maapi:start_progress_span_th(Sock, Tid, 0, "hello world trans 1", [], [], []),
        timer:sleep(39),
        econfd_maapi:finish_trans(Sock, Tid),
        {ok, {SpanId1, TraceId}} =
            econfd_maapi:end_progress_span(Sock, SpanId2, "bye world"),
        {ok, {_SpanId0 = undefined, _}} =
            econfd_maapi:end_progress_span(Sock, SpanId1, "bye world"),
       {ok, L1} = with_rtrans(Sock, fun(S0, Tid0) -> match_msg(S0, Tid0, SpanId1) end),
        [{_, <<"usess 1">>},
         {_, <<"usess 1">>}] = L1,
        {ok, L2} = with_rtrans(Sock, fun(S0, Tid0) -> match_msg(S0, Tid0, SpanId2) end),
        [{_, <<"hello world trans 1">>},
         {_, <<"hello world trans 1">>}] = L2,
        {ok, LParentSpan2} = with_rtrans(Sock, fun(S0, Tid0) ->
            match_parent_span(S0, Tid0, SpanId2) end),
        [{_, SpanId1},
         {_, SpanId1}] = LParentSpan2,
        ok
    after
        econfd_maapi:close(Sock)
    end.

progress_span_links() ->
    [{userdata, [{description,
        "start a root span Span11. start a second root span Span21
         and link (backwards) to the first (Span11). start a child
         span Span12 from Span11 and link (forwards) to Span21.
         verify the spans are correctly linked under the
         /progress/trace/event/link oper data"}]}].

progress_span_links(_Config) ->
    TrapExit = process_flag(trap_exit, true),
    {ok, Sock1} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
    ok = econfd_maapi:start_user_session(Sock1, <<"admin">>, <<"system">>, [], {1,2,3,4}, 0),
    try
        {ok, {SpanId11, TraceId1}} =
            econfd_maapi:start_progress_span(Sock1, 0, "usess 1 parent span", [], [], []),
        ct:pal("SpanId11 ~ts TraceId1 ~ts", [SpanId11, TraceId1]),

        Pid1 = self(),
        Ref = make_ref(),
        Pid2 = proc_lib:spawn_link(fun () ->
            {ok, Sock2} = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
            ok = econfd_maapi:start_user_session(Sock2, <<"admin">>, <<"system">>, [], {1,2,3,4}, 0),
            try
                {ok, {SpanId21, TraceId2}} =
                    econfd_maapi:start_progress_span(Sock2, 0, "usess 2", [], [],
                        _Links = [{TraceId1, SpanId11}]),
                ct:pal("SpanId11 ~ts TraceId1 ~ts", [SpanId21, TraceId2]),
                Pid1 ! #{pid => self(), ref => Ref, span_id => SpanId21, trace_id => TraceId2},
                timer:sleep(10),

                ?assertNotEqual(TraceId1, TraceId2),
                {ok, {_SpanId10 = undefined, _}} =
                    econfd_maapi:end_progress_span(Sock2, SpanId21, "bye world")
            after
                econfd_maapi:close(Sock2)
            end
        end),

        receive
            #{ref := Ref0, span_id := SpanId21, trace_id := TraceId2} when Ref0 == Ref ->
                 {ok, {SpanId12, TraceId1}} =
                     econfd_maapi:start_progress_span(Sock1, 0, "usess 1 child span", [], [],
                         _Links = [{TraceId2, SpanId21}]),
                     ct:pal("SpanId12 ~ts TraceId1 ~ts", [SpanId12, TraceId1]),
                 {ok, {SpanId11, TraceId1}} =
                     econfd_maapi:end_progress_span(Sock1, SpanId12, "bye world"),
                 {ok, {_SpanId10 = undefined, _}} =
                     econfd_maapi:end_progress_span(Sock1, SpanId11, "bye world"),

                 ct:pal("forward link ~p -> ~p", [SpanId12, {TraceId2, SpanId21}]),
                 {ok, LL12} = with_rtrans(Sock1, fun (S0, Tid0) -> match_link(S0, Tid0, SpanId12) end),
                 [{[Link1 | _], undefined}, {[Link1 | _], undefined}] = LL12,
                 ?assertEqual({TraceId2, SpanId21}, Link1),

                 ct:pal("backward link ~p -> ~p", [SpanId21, {TraceId1, SpanId11}]),
                 {ok, LL21} = with_rtrans(Sock1, fun (S0, Tid0) -> match_link(S0, Tid0, SpanId21) end),
                 [{[Link2 | _], undefined} | _] = LL21,
                 ?assertEqual({TraceId1, SpanId11}, Link2)
         end,
         receive
             {'EXIT', Pid, normal} when Pid == Pid2 -> ok
         end
    after
        econfd_maapi:close(Sock1),
        process_flag(trap_exit, TrapExit)
    end.

%% -----------------------------------------------------------------------------
%% internal functions
%% -----------------------------------------------------------------------------
with_rtrans(Sock, F) ->
    {ok, Tid} = econfd_maapi:start_trans(Sock, ?CONFD_RUNNING, ?CONFD_READ),
    try F(Sock, Tid)
    after econfd_maapi:finish_trans(Sock, Tid)
    end.

match_msg(Sock, Tid, SpanId) ->
    match(Sock, Tid, pred(<<"span-id">>, SpanId), <<"message">>).

match_attr(Sock, Tid, SpanId, Attr) ->
    match(Sock, Tid, [pred(<<"span-id">>, SpanId), <<"/attribute">>,
        pred(<<"name">>, Attr)], <<"value">>).

match_parent_span(Sock, Tid, SpanId) ->
    match(Sock, Tid, pred(<<"span-id">>, SpanId), <<"parent-span-id">>).

match_link(Sock, Tid, SpanId) ->
    match(Sock, Tid, pred(<<"span-id">>, SpanId), <<"link">>).

pred(MatchField, MatchV) ->
    <<"[", MatchField/binary, "='", MatchV/binary, "']">>.

match(Sock, Tid, Pred, OutField) ->
    F = fun (IKP, V, Acc) ->
            {?ITER_CONTINUE, [{IKP, V} | Acc]}
    end,
    Q = <<"/progress/trace[name='test-oper']/event",
        (iolist_to_binary(Pred))/binary, "/", OutField/binary>>,
    ct:log("evaluating ~ts", [Q]),
    case econfd_maapi:xpath_eval(Sock, Tid, Q, F, _Init = [], []) of
        {ok, L} -> {ok, lists:reverse(L)};
        {error, _} = Error -> Error
    end.
