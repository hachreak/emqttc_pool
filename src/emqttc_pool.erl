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
%% @doc emqttc_pool public API
%% @end

-module(emqttc_pool).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

%% Application callbacks
-export([
  publish/3,
  publish/4,
  subscribe/2,
  subscribe/3
]).

-include_lib("emqttc/include/emqttc_packet.hrl").

%%====================================================================
%% API
%%====================================================================

subscribe(PoolName, Topic) ->
  poolboy:transaction(PoolName, fun(Worker) ->
      gen_server:cast(Worker, {subscribe, Topic})
    end).

subscribe(PoolName, Topic, Qos) ->
  poolboy:transaction(PoolName, fun(Worker) ->
      gen_server:cast(Worker, {subscribe, Topic, Qos})
    end).

publish(PoolName, Topic, Payload) ->
  poolboy:transaction(PoolName, fun(Worker) ->
      gen_server:cast(Worker, {publish, Topic, Payload})
    end).

publish(PoolName, Topic, Payload, PubOpts) ->
  poolboy:transaction(PoolName, fun(Worker) ->
      gen_server:cast(Worker, {publish, Topic, Payload, PubOpts})
    end).

%%====================================================================
%% Internal functions
%%====================================================================
