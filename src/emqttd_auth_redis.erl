%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2016 eMQTT.IO, All Rights Reserved.
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%-----------------------------------------------------------------------------
%%% @doc Authentication with Redis.
%%%
%%% @author Feng Lee <feng@emqtt.io>
%%%-----------------------------------------------------------------------------

-module(emqttd_auth_redis).

-behaviour(emqttd_auth_mod).

-include("../../../include/emqttd.hrl").

-export([init/1, check/3, description/0]).

-record(state, {auth_cmd, hash_type}).

-define(UNDEFINED(S), (S =:= undefined orelse S =:= <<>>)).

init({AuthCmd, HashType}) -> 
    {ok, #state{auth_cmd = AuthCmd, hash_type = HashType}}.

check(#mqtt_client{username = Username}, Password, _State)
    when ?UNDEFINED(Username) orelse ?UNDEFINED(Password) ->
    {error, username_or_passwd_undefined};

check(#mqtt_client{username = Username}, Password,
      #state{auth_cmd = AuthCmd, hash_type = HashType}) ->
    case emqttd_redis_client:query(replvar(AuthCmd, Username)) of
        {ok, undefined} ->
            {error, not_found};
        {ok, HashPass} ->
            check_pass(HashPass, Password, HashType);
        {error, Error} ->
            {error, Error}
    end.

replvar(AuthCmd, Username) ->
    re:replace(AuthCmd, "%u", Username, [global, {return, binary}]).

description() -> "Authentication with Redis".

