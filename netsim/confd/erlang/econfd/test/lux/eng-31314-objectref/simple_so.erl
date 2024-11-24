-module(simple_so).

-include("../../../include/econfd.hrl").
-include("smp.hrl").
-record(server, {name_number, obj, refs, attrs = []}).

-export([start/0]).

start() ->
    application:start(econfd),
    timer:sleep(1000),
    proc_lib:spawn(fun go/0).

go() ->
    init_db(),
    Trans = #confd_trans_cbs{init = fun s_init/1},
    Data = #confd_data_cbs{get_elem = fun get_elem/2,
                           get_next = fun get_next/3,
                           set_elem = undefined,
                           create = undefined,
                           remove = undefined,
                           get_attrs = fun get_attrs/3,
                           set_attr = undefined,
                           exists_optional = fun exists_optional/2,
                           callpoint = simplecp},

    process_flag(trap_exit, true),
    {ok,Daemon} = econfd:init_daemon(simple, ?CONFD_TRACE, user, none,
                                    {127,0,0,1}, ?CONFD_PORT),

    register(daemon, Daemon),
    ok = econfd:set_daemon_flags(Daemon, ?CONFD_DAEMON_FLAG_STRINGSONLY),
    ok = econfd:register_trans_cb(Daemon, Trans),
    ok = econfd:register_data_cb(Daemon, Data),
    ok = econfd:register_done(Daemon),
    receive
        {'EXIT', _From, _Reason} ->
            init:stop()
    end.


make_ets() ->
    spawn(fun() ->
                  (catch ets:delete(servers)),
                  ets:new(servers, [public,
                                    {keypos, 2},
                                    named_table,
                                    ordered_set]),
                  timer:sleep(infinity)
          end).

init_db() ->
    make_ets(),
    case restore("running.DB") of
        ok ->
            ok;
        _ ->
            io:format("setting servers to default values \n", []),
            ets:insert(servers, #server{name_number={<<"ssh">>, <<"1">>},
                                        obj=objref(<<"www">>, <<"2">>)
                                       }),
            ets:insert(servers,
                       #server{name_number={<<"www">>, <<"2">>},
                               obj=objref(<<"smtp">>, <<"3">>),
                               refs=[<<"/smp:servers/smp:server[smp:name='foo']"
                                       "[smp:number='1']/smp:obj">>,
                                     <<"/smp:servers/smp:server[smp:name='bar']"
                                       "[smp:number='17']/smp:obj">>
                                    ]
                              }),
            ets:insert(servers, #server{name_number={<<"smtp">>, <<"3">>},
                                        obj=objref(<<"ssh">>, <<"1">>)}),
            ok
    end.

restore(File) ->
    case file:read_file(File) of
        {ok, B} ->
            L = binary_to_term(B),
            ets:match_delete(servers, '_'),
            lists:foreach(fun(S) -> ets:insert(servers, S) end, L),
            ok;
        Err ->
            Err
    end.

find_server({Name, Number}) ->
    ets:lookup(servers, {Name, Number}).

%% get_next call for list server, initial call
get_next(_Tctx, [server | _] = _IKeypath, -1) ->
    case ets:first(servers) of
        '$end_of_table' ->
            {ok, {false, undefined}};
        Key = {Name, Number} ->
            Next = ets:next(servers, Key),
            {ok, {{Name, Number}, Next}}
    end;
%% get_next call for leaf-list refs, initial call
get_next(Tctx, [refs, Key | _Tail] = _IKeypath, -1) ->
    case find_server(Key) of
        [] ->
            {ok, {false, undefined}};
        [Srv] ->
            case Srv#server.refs of
                undefined ->
                    {ok, {false, undefined}};
                [] ->
                    {ok, {false, undefined}};
                [LeafListInstance|Rest] ->
                    {ok, {{LeafListInstance}, Rest}, Tctx}
            end
    end;
%% get_next call for list server, subsequent call
get_next(_Tctx, [server | _] = _IKeypath, Prev) ->
    case Prev of
        '$end_of_table' ->
            {ok, {false, undefined}};
        {Name, Number} ->
            Next = ets:next(servers, Prev),
            {ok, {{Name, Number}, Next}}
    end;
%% get_next call for leaf-list refs, subsequent call
get_next(_Tctx, [refs | _] = _IKeypath, Prev) ->
    case Prev of
        [] ->
            {ok, {false, undefined}};
        [LeafListInstance|Rest] ->
            {ok, {{LeafListInstance}, Rest}}
    end.

get_elem(_Tctx, [ElemTag, Key | _Tail]) ->
    case find_server(Key) of
        [] ->
            {ok, not_found};
        [Srv] ->
            if
                ElemTag == name ->
                    {Name, _Number} = Srv#server.name_number,
                    {ok, Name};
                ElemTag == obj ->
                    {ok, ?CONFD_OBJECTREF(Srv#server.obj)};
                true ->
                    {error, <<"Bad tag received">>}
            end
    end.

%% We only support attrs on the server instance itself
get_attrs(_Tctx, [{{?C_OBJECTREF, _X}}|_], _AttrL) ->
    {ok, []};
get_attrs(_Tctx, [Key | _Ikeypath], AttrL) when is_tuple(Key) ->
    case find_server(Key) of
        [] -> {ok, not_found};
        [Srv] -> {ok, get_attrs(AttrL, Srv#server.attrs)}
    end;
get_attrs(_Tctx, [_Elem, Key | _Ikeypath], _AttrL) when is_tuple(Key) ->
    case find_server(Key) of
        [] -> {ok, not_found};
        [_Srv] -> {ok, []}
    end;
get_attrs(_Tctx, _Ikeypath, _AttrL) ->
    {ok, []}.

get_attrs([], Attrs) ->
    Attrs;
get_attrs(L, Attrs) ->
    lists:zf(fun(Attr) ->
                     case lists:keysearch(Attr, 1, Attrs) of
                         {value, V} -> {true, V};
                         false      -> false
                     end
             end, L).

objref(Name, Number) ->
    [obj, {Name, Number}, server, [?smp__ns_uri|servers]].

s_init(Tctx) ->
    {ok, Tctx}.

%% exists_optional call for leaf-list refs
exists_optional(_Tctx, [ObjRef, refs, Key, server,
                        ['http://tail-f.com/ns/example/smp'|servers]]) ->
    case find_server(Key) of
        [] ->
            {ok, false};
        [Srv] ->
            case Srv#server.refs of
                undefined ->
                    {ok, false};
                Refs ->
                    {?CONFD_OBJECTREF(UnObjectRef)} = ObjRef,
                    {ok, lists:member(UnObjectRef, Refs)}
            end
    end.
