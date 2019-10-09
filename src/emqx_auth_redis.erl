%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(emqx_auth_redis).

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").
-include("emqx_auth_redis.hrl").

-export([ register_metrics/0
        , check/2
        , description/0
        , load/1
        , unload/0
        , on_client_connected/4
        , on_client_disconnected/3
        ]).

register_metrics() ->
    [emqx_metrics:new(MetricName) || MetricName <- ['auth.redis.success', 'auth.redis.failure', 'auth.redis.ignore']].

check(Credentials = #{client_id := ClientId,
                      username := Username,
                      password := Password}, #{hash_type := HashType}) ->
    case check_user(ClientId, Username, Password, HashType) of
        ok ->
            emqx_metrics:inc('auth.redis.success'),
            {stop, Credentials#{is_superuser => false,
                                anonymous => false,
                                auth_result => success}};
        fail ->
            emqx_metrics:inc('auth.redis.failure'),
            {stop, Credentials}
    end.

description() -> "Authentication with Redis".

check_pass(Password, HashType) ->
    case emqx_passwd:check_pass(Password, HashType) of
        ok -> ok;
        {error, _Reason} -> {error, not_authorized}
    end.

q(Cmd) ->

  case ecpool:with_client(?APP, fun(C) -> eredis:q(C, Cmd) end) of
    {ok,[]} -> fail;
    {ok,[undefined]} ->fail;
    {ok,[undefined,undefined]} ->fail;
    {ok,Result} -> {ok,Result}
  end.

r_fold(Fun, Acc0, [Head | Tail]) ->
  case Fun(Head, Acc0) of
    {ok, Acc} ->
      r_fold(Fun, Acc, Tail);
    ok -> ok;
    fail -> io:format("~n ->funtion end with error"),fail
  end;
r_fold(_Fun, Acc0, []) ->
  case Acc0 of
    {ok, Acc} ->
      io:format("->end success"),
      Acc;
    fail ->
      io:format("->get no result "),fail;
    Other ->
      Other
  end.

%%登陆验证用户名
l_check_username({ClientId,Password,HashType}) ->
  io:format(string:concat("~n->l_check_username ,clientId =",binary_to_list(ClientId))),
  case q(["EXISTS",string:concat("dc:emqttd:login:",ClientId)]) of
    {ok,<<"0">>} ->
      {error, password_error};
    {ok,<<"1">>} -> {ok,{ClientId,Password,HashType}}
  end.
%%登陆验证密码
l_get_login({ClientId,Password,HashType}) ->
  io:format("~n->l_get_login"),
  case q(["HMGET",string:concat("dc:emqttd:login:",ClientId)|["pdSecret","kickOther","status"]]) of
    fail -> fail ;
    {ok,[P,K,S]} ->
      %%判断status
      io:format(string:concat("->pdSecret = ", binary_to_list(P))),
      io:format(string:concat("->kickOther = ", binary_to_list(K))),
      io:format(string:concat("->status = ", binary_to_list(S))),
      case check_pass({P, Password}, HashType) of
        ok -> io:format("-> check password , success"),
          case string:equal(binary_to_list(K),"1") of
            true ->
              {ok,ClientId};
            false ->
              io:format("~n-> dc:emqttd:online:"),
              io:format(ClientId),

              case q(["EXISTS",string:concat("dc:emqttd:online:",ClientId)]) of
                {ok,<<"0">>} ->
                  io:format("->online User not login----"),
                  io:format("->success"),
                  {ok,ClientId};
                {ok,<<"1">>} ->
                  io:format("->online User login----"),
                  fail
              end
          end;
        false -> io:format("->check password ,fail "),fail
      end
  end.

%%登陆成功添加online
lr_update_online(ClientId) ->
  io:format("~n->lr_update_online"),
  case q(["SET",string:concat("dc:emqttd:online:",ClientId),"1"]) of
    fail ->io:format("->update online fail "),fail ;
    {ok,_} -> io:format("->update online =1"),{ok,{ClientId,"1"}}
  end.
%%修改状态
lr_update_status({ClientId,Status}) ->
  io:format("~n->update status"),
  case q(["HSET",string:concat("dc:emqttd:login:",ClientId)|[ "status",Status]]) of
    fail -> io:format("->update status fails"),fail;
    {ok,_} -> io:format("->update status success"),ok
  end.
%%注册验证用户名
r_check_username({ClientId,Username_string,Password,HashType}) ->
  io:format(string:concat("~n->r_check_username ,Username_string =",Username_string)),
  case q(["EXISTS",string:concat("dc:emqttd:productLogin:",Username_string)]) of
    {ok,<<"0">>} -> fail;
    {ok,<<"1">>} -> {ok,{ClientId,Username_string,Password,HashType}}
  end.

%%验证register密码是否通过
r_get_login({ClientId,Username_string,Password,HashType}) ->
  io:format("~n->r_get_login"),
  case q(["HGET",string:concat("dc:emqttd:productLogin:",Username_string),"productSecret"]) of
    fail -> io:format("~n->did is undefined"),fail;
    {ok,P} ->
      io:format(string:concat("->P=",binary_to_list(P))),
      case check_pass({P, Password}, HashType) of
        ok -> io:format("->check password , success"),{ok,ClientId};
        false -> io:format("->check password ,fail "),fail
      end
  end.
%%检查用户是否已经登陆
r_check_online(ClientId) ->
  io:format("~n->check_online"),
  case q(["EXISTS",string:concat("dc:emqttd:online:",ClientId)]) of
    {ok,<<"0">>} -> io:format("->success"),{ok,ClientId};
    {ok,<<"1">>} -> fail
  end.
%%检查ClientID是否存在
r_check_clientId(ClientId) ->
  io:format("~n->r_check_clientId"),
  case q(["EXISTS",string:concat("dc:emqttd:login:",ClientId)]) of
    {ok,<<"0">>} -> io:format("->not exists"),ok;
    {ok,<<"1">>} -> io:format("->exists"),{ok,ClientId}
  end.
%%检查ClientID是否存在
r_check_status(ClientId) ->
  io:format("~n->r_check_status"),
  case q(["HGET",string:concat("dc:emqttd:login:",ClientId),"status"]) of
    fail -> io:format("->status undefined"),fail;
    {ok,S} ->
      io:format(string:concat("->S=",binary_to_list(S))),
      case string:equal(binary_to_list(S),"2") of
        true -> io:format("->fail"),fail;
        false -> io:format("->success"),{ok,ClientId}
      end
  end.

%%通过#mqtt_client{}方法，对Client对象的属性提取出来，username存储的是<<"">>类型的数据,需要通过binary_to_list方式转换，然后可以使用string命令操作
%%FunList是声明的一个函数数组，其中 fun l_check_usernmae/1表示的是函数l_check_usernmae参数个数1；
%%r_fold是自定义函数，参数是一个函数，一个保存连接信息的对象和一个之前声明的FunList函数数组，r_fold的作用是消除erlang语法的多层if嵌套问题，erlang中
%%对于if判断无法使用return或者其他的方式退出方法，所以引入r_fold方法，将if判断提取出来
check_user(ClientId,Username,Password,HashType) ->
  io:format("~n->compare (:)"),
  Username_string = binary_to_list(Username),
  Result = case string:str(Username_string,":")>0 of
    true ->
      io:format("~n->with colon(:)->start login"),
      FunList = [
        fun l_check_username/1,
        fun l_get_login/1,
        fun lr_update_online/1,
        fun lr_update_status/1
      ],
      r_fold(fun(Fun, CheckRecord) ->Fun(CheckRecord) end, {ClientId,Password,HashType}, FunList);
    false ->
      io:format("~n->no colon(:)->start register"),
      FunList = [
        fun r_check_username/1,
        fun r_get_login/1,
        fun r_check_online/1,
        fun r_check_clientId/1,
        fun r_check_status/1,
        fun lr_update_online/1,
        fun lr_update_status/1
      ],
      r_fold(fun(Fun, CheckRecord) ->Fun(CheckRecord) end, {ClientId,Username_string,Password,HashType}, FunList)
  end,
  io:format("~n~n"),
  Result.

%%emq钩子的使用
load(Env) ->
  emqx:hook('client.connected', fun ?MODULE:on_client_connected/3, [Env]),
  emqx:hook('client.disconnected', fun ?MODULE:on_client_disconnected/3, [Env]).

unload() ->
  emqx:unhook('client.connected', fun ?MODULE:on_client_connected/3),
  emqx:unhook('client.disconnected', fun ?MODULE:on_client_disconnected/3).

on_client_disconnected(#{client_id := ClientId}, Reason, _Env) ->
  %%断开连接，将状态status设置为0
  io:format("~nclient ~s disconnected, reason: ~w", [ClientId, Reason]),
  %%case [Reason] of
  %% "auth_failure" ->{ok,_Client};
  %%  <<"auth_failure">> ->{ok,_Client};
  %%  _ ->
  %%end.
  case q(["EXISTS",string:concat("dc:emqttd:login:",ClientId)]) of
    {error,Reason} ->fail;
    {ok,<<"1">>} ->
      %%用于设置，暂时不用过期时间
      %%q(["Expire",string:concat("mqconnect:lock:",ClientId),5]),
      io:format("~n->disconnected set status = 0 and delete online"),
      q(["Expire",string:concat("dc:emqttd:online:",ClientId),0]),
      q(["HSET",string:concat("dc:emqttd:login:",ClientId)|[ "status","0"]])
  end.

on_client_connected(_Client, _ConnAck, _ConnAttrs, _Env) ->
  ok.