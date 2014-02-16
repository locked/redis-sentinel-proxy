%%% The contents of this file are Open Source.  They may be freely
%%% copied, used or enhanced in any way without restriction provided
%%% acknowledgement of the original author is included in all
%%% distributions of any derivative works.  This file is intended
%%% as an example tutorial only and is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
%%% The software is not guaranteed to work, nor is the author
%%% liable for any damage or lost opportunity caused by the use of
%%% this software.
%%% 
%%%-------------------------------------------------------------------
%%% File    : redis_proxy.erl
%%% Author  : Etienne Adam
%%% Description : Redis proxy
%%%
%%%   Date    Auth    Desc
%%% 12-02-14   EA   Initial creation
%%%-------------------------------------------------------------------
-module(redis_proxy).

-export([init/0, terminate/0, server_busy/1, react_to/3]).


init() ->
    {ok, []}.

% Write the table to disk in case new URLs were added    
terminate() ->
    ok.

% When the server is busy, send a real HTML page.
server_busy(Client) ->
    gen_tcp:send(Client, <<"SERVER_BUSY\n">>),
    ok.

% Display proxy stats
react_to(_Server, Client, <<"*1\r\n$5\r\nproxy\r\n", _Rest/binary>>) ->
    case get_masters_from_sentinels_conf() of
        {ok, Masters} ->
            % E = lists:nth(1, Masters),
            Lines = [io_lib:format("$~p\r~n~s\r~n", [length(E), E]) || E <- Masters],
            LinesStr = string:join(Lines, ""),
            gen_tcp:send(Client, io_lib:format("*~p\r~n~s", [length(Masters), LinesStr]));

        _Other ->
            gen_tcp:send(Client, <<"*2\r\n$5\r\nError\r\n">>)
    end,
    gen_tcp:close(Client),
    ok;

% Redis requests entry point
react_to(_Server, Client, Data = <<"*", _Rest/binary>>) ->
    %% Uncomment this line to see what pages are requested
    %% io:format("~s~n", [binary_to_list(URL)]),
    get_redis(Client, Data),
    gen_tcp:close(Client),
    ok;

% All others are considered Bad Requests (although some should be valid).
react_to(_Server, Client, Other) ->
    gen_tcp:send(Client, <<"BAD_REQUEST.\n">>),
    gen_tcp:close(Client),
    {error, {bad_request, Other}}.


%%--------------------------------------------------------------------

connect(Host) ->
    gen_tcp:connect(Host, 6379, [binary, {active, false}, {packet, 0}]).


send_bad_gateway(Client, Error) ->
    gen_tcp:send(Client, lists:flatten(io_lib:print(Error, 1, 30, 100))),
    gen_tcp:send(Client, <<"\nNO SERVER AVAILABLE\n">>).



extract_master_host(<<"$9\r\n", Host/binary>>) ->
    MasterHost = re:replace(Host, "\\s+", "", [global,{return,list}]),
    {ok, MasterHost};

extract_master_host(_Data) ->
    {error, "No Match"}.



read_sentinel_response(Socket) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, RawData} ->
            io:format("RawData: ~p~n", [RawData]),
            extract_master_host(RawData);

        Other ->
            io:format("Recv error: ~p~n", [Other])
    end.


get_masters_from_sentinels_conf() ->
    case application:get_env(redis, sentinels) of
        {ok, Sentinels} ->
            get_masters_from_sentinels(Sentinels, []);

        {error, _Reason} ->
            {error, "No config for sentinel host"}
    end.


get_masters_from_sentinels([Sentinel | T], Masters) ->
    case Sentinel of
        {host_port, SentinelHostPort} ->
            case get_master_from_sentinel(SentinelHostPort) of
                {ok, Master} ->
                    get_masters_from_sentinels(T, Masters ++ [Master]);
                {error, _Reason} ->
                    get_masters_from_sentinels(T, Masters)
            end;

        _Other ->
            io:format("No match~n")
    end;

get_masters_from_sentinels([], Masters) ->
    {ok, Masters}.


% Cache
get_master_from_cache() ->
    rediscache ! {self(), get_redis_master},
    receive
        {_Pid2, notfound} ->
            {notfound};

        {_Pid2, Master} ->
            % Master = lists:nth(1, Val),
            {ok, Master}
    after 1 ->
            {notfound}
    end.

save_master_in_cache(Master) ->
    rediscache ! {self(), {put_redis_master, Master}}.



get_master_from_sentinels() ->
    case get_master_from_cache() of
    {ok, Master} ->
        io:format("get_master_from_cache(): ~p~n", [Master]),
        {ok, Master};

    {notfound} ->
        io:format("get_master_from_cache(): notfound~n"),
        case get_masters_from_sentinels_conf() of
        {ok, Masters} ->
            case sets:size(sets:from_list(Masters)) of
                1 ->
                    Master = lists:nth(1, Masters),
                    save_master_in_cache(Master),
                    {ok, Master};

                _Other ->
                    {error, "Masters list mismatch"}
            end;

        _Other ->
            {error, "Masters list invalid"}
        end
    end.
    

get_master_from_sentinel([Host, Port]) ->
    io:format("Connection to sentinel: ~p:~p~n", [Host, Port]),
    case gen_tcp:connect(Host, Port, [binary, {active, false}, {packet, 0}]) of
        {ok, Socket} ->
            gen_tcp:send(Socket, <<"*2\r\n$3\r\nget\r\n$8\r\nsentinel\r\n">>),
            read_sentinel_response(Socket);

        {error, Reason} ->
            io:format("Sentinel is unreachable: ~p~n", [Reason]),
            {error, "Could not find master host"}
    end.


get_redis(Client, Data) ->
    case get_master_from_sentinels() of
        {ok, RedisHost} ->
            io:format("RedisHost: ~p~n", [RedisHost]),
            get_redis(Client, Data, RedisHost);

        {error, Reason} ->
            io:format("get_redis: ~p~n", [Reason])
    end.

get_redis(Client, Data, RedisHost) ->
    case connect(RedisHost) of
	% Get the page
	{ok, Socket} ->
	    gen_tcp:send(Socket, Data),
	    receive_redis_data(Client, Socket),
	    gen_tcp:close(Socket);
	
	% Report the error
	{error, Reason} ->
	    send_bad_gateway(Client, Reason);

	Other ->
	    send_bad_gateway(Client, Other)
    end.


receive_redis_data(Client, Socket) ->
    case gen_tcp:recv(Socket, 0, 5000) of
	{ok, Bin} ->
	    gen_tcp:send(Client, Bin),
	    receive_redis_data(Client, Socket);

	{error, closed} ->
	    ok;

	{error, timeout} ->
	    gen_tcp:send(Client, <<"GATEWAY_TIMEOUT\n">>);

	Other ->
	    send_bad_gateway(Client, Other)
    end.

