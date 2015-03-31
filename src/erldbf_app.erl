%%%-------------------------------------------------------------------
%%% @author Vitali Kletsko
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. мар 2015 15:34
%%%-------------------------------------------------------------------
-module(erldbf_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    erldbf_sup:start_link().

stop(_State) ->
    ok.
