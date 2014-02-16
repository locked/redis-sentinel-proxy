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
%%% File    : redis.erl
%%% Author  : Etienne Adam
%%% Description : Redis proxy
%%%
%%%   Date    Auth    Desc
%%% 12-02-14   EA   Initial creation
%%%-------------------------------------------------------------------
-module(redis).

-export([start/2, stop/1, rediscache/0]).


start(_StartType, _StartArgs) ->
    io:format("START~n"),
    Pid = spawn_link(redis, rediscache, []),
    % io:format("rediscache: ~p~n", [Pid]),
    register(rediscache, Pid),
    tcp_proxy:start_link(redis_proxy).

stop(_State) ->
    io:format("STOP~n").

rediscache() ->
    receive
        {_From, {put_redis_master, Master}} ->
            TTL = calendar:local_time(),
            % io:format("put in cache: ~p~n", [[Master, TTL]]),
            put(redis_master, [Master, TTL]),
            rediscache();

        {From, get_redis_master} ->
            Raw = get(redis_master),
            % io:format("got RAW from cache: ~p~n", [Raw]),
            case Raw of
                undefined ->
                    Master = notfound;

                [FoundMaster, TTL] ->
                    {_,{_,_,Seconds}} = calendar:time_difference(TTL, calendar:local_time()),
                    if Seconds > 5 -> Master = notfound;
                              true -> Master = FoundMaster
                    end;

                _Other ->
                    Master = notfound
            end,
            % io:format("got from cache: ~p~n", [Master]),
            From ! {self(), Master},
            rediscache();

        stop ->
            true
    end.

