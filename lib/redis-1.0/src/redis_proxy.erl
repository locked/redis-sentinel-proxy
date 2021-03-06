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



extract_master_host(<<"*2\r\n", Host/binary>>) ->
    [_Size, MasterHost, _PortSize, _Port, _] = re:split(Host, "\r\n"),
    {ok, binary_to_list(MasterHost)};

extract_master_host(_Data) ->
    {error, "No Match"}.



read_sentinel_response(Socket) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, RawData} ->
            % io:format("RawData: ~p~n", [RawData]),
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
    % io:format("Connection to sentinel: ~p:~p~n", [Host, Port]),
    case gen_tcp:connect(Host, Port, [binary, {active, false}, {packet, 0}]) of
        {ok, Socket} ->
            gen_tcp:send(Socket, <<"*3\r\n$8\r\nSENTINEL\r\n$23\r\nget-master-addr-by-name\r\n$8\r\nmymaster\r\n">>),
            read_sentinel_response(Socket);

        {error, Reason} ->
            io:format("Sentinel is unreachable: ~p~n", [Reason]),
            {error, "Could not find master host"}
    end.


get_redis(Client, Data) ->
    case get_master_from_sentinels() of
        {ok, RedisHost} ->
            % io:format("RedisHost: ~p~n", [RedisHost]),
            get_redis(Client, Data, RedisHost);

        {error, Reason} ->
            io:format("get_redis: ~p~n", [Reason])
    end.

get_redis(Client, Data, RedisHost) ->
    case connect(RedisHost) of
	% Get the page
	{ok, Socket} ->
            io:format(" ToRedis> ~p~n", [Data]),
	    gen_tcp:send(Socket, Data),
	    receive_redis_data(Client, Socket, []),
	    gen_tcp:close(Socket);
	
	% Report the error
	{error, Reason} ->
            io:format(" [get_redis] Error: ~p~n", [Reason]),
	    gen_tcp:close(Client);

	Other ->
	    gen_tcp:close(Client)
    end.


receive_client_data(Client, Socket) ->
    case gen_tcp:recv(Client, 0, 60000) of
        {ok, Data} ->
            io:format(" ToRedis> ~p~n", [Data]),
	    gen_tcp:send(Socket, Data),
	    receive_redis_data(Client, Socket, []),
	    gen_tcp:close(Socket);

	{error, closed} ->
	    ok;

	{error, timeout} ->
            io:format("[receive_client_data] timeout~n")
    end.

cmd_done(<<"$", Count/binary>>, [First | _Rest]) ->
    io:format(" [$] Length:~p / ~p~n", [byte_size(First), list_to_integer(binary_to_list(Count))]),
    byte_size(First) == list_to_integer(binary_to_list(Count));

cmd_done(<<"*", Count/binary>>, Rest) ->
    io:format(" [*] Length:~p / ~p~n", [length(Rest), list_to_integer(binary_to_list(Count))]),
    length(Rest) - 1 == list_to_integer(binary_to_list(Count)) * 2;

cmd_done(Any, Rest) ->
    io:format(" Any:~p~n", [Any]),
    false.

to_client(Client, Socket, Bin = <<"+OK\r\n">>, SoFar) ->
    gen_tcp:send(Client, Bin),
    receive_client_data(Client, Socket);

to_client(Client, Socket, Bin, SoFar) ->
    gen_tcp:send(Client, Bin),
    NewSoFar = [Bin, SoFar],
    CurrentBuffer = erlang:iolist_to_binary(NewSoFar),
    % io:format(" CurrentBuffer: ~p~n", [CurrentBuffer]),
    [First | Rest] = re:split(CurrentBuffer, "\r\n"),
    io:format(" First: ~p Rest: ~p~n", [First, Rest]),
    case cmd_done(First, Rest) of
        true ->
            io:format("  Finished~n"),
            receive_client_data(Client, Socket);

        false ->
            io:format("  Not finished~n"),
            receive_redis_data(Client, Socket, NewSoFar)
    end.

receive_redis_data(Client, Socket, SoFar) ->
    case gen_tcp:recv(Socket, 0, 5000) of
	{ok, Bin} ->
            io:format(" ToClient> ~p~n", [Bin]),
            to_client(Client, Socket, Bin, SoFar);

	{error, closed} ->
	    ok;

	{error, timeout} ->
            io:format("[receive_redis_data] timeout~n"),
            gen_tcp:close(Client);

	Other ->
            gen_tcp:close(Client)
    end.

