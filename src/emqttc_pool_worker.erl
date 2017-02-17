%%% @author Leonardo Rossi <leonardo.rossi@studenti.unipr.it>
%%% @copyright (C) 2015, 2016 Leonardo Rossi
%%%
%%% This software is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This software is distributed in the hope that it will be useful, but
%%% WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this software; if not, write to the Free Software Foundation,
%%% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%
%% @doc emqttc_pool app
%% @end

-module(emqttc_pool_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

%% Application callbacks
-export([start_link/1]).
-export([
  code_change/3,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  init/1,
  terminate/2
]).

-record(state, {connection}).

%%====================================================================
%% API connection
%%====================================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init({Args, Retries}) ->
  process_flag(trap_exit, true),
  Connection = wait_connection(Args, Retries),
  {ok, #state{connection=Connection}}.

%%====================================================================
%% API actions
%%====================================================================

handle_cast({subscribe, Topic}, #state{connection=Connection}=State) ->
  emqttc:subscribe(Connection, Topic),
  {noreply, State};
handle_cast({subscribe, Topic, Qos}, #state{connection=Connection}=State) ->
  emqttc:subscribe(Connection, Topic, Qos),
  {noreply, State};
handle_cast({publish, Topic, Payload}, #state{connection=Connection}=State) ->
  emqttc:publish(Connection, Topic, Payload),
  {noreply, State};
handle_cast(
    {publish, Topic, Payload, PubOpts}, #state{connection=Connection}=State) ->
  emqttc:publish(Connection, Topic, Payload, PubOpts),
  {noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.

handle_call(_Msg, _From, State) -> {reply, ok, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, #state{connection=Connection}) ->
  ok = emqttc:disconnect(Connection),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% Wait server is available for connection
wait_connection(_, 0) ->
  throw(emqttd_connection_error);
wait_connection(Args, Remaining_retries) ->
  error_logger:info_msg(
    "Emqttc-pool: remaining retries ~p~n", [Remaining_retries]),
  case emqttc:start_link(Args) of
    {ok, Connection} ->
      error_logger:info_msg("Connected!~n"),
      Connection;
    _Rest ->
      error_logger:info_msg("Sleep..~n"),
      timer:sleep(2000),
      wait_connection(Args, Remaining_retries - 1)
  end.
