%% =============================================================================
%%  bondy_SUITE.erl -
%%
%%  Copyright (c) 2016-2017 Ngineo Limited t/a Leapsight. All rights reserved.
%%
%%  Licensed under the Apache License, Version 2.0 (the "License");
%%  you may not use this file except in compliance with the License.
%%  You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%%  Unless required by applicable law or agreed to in writing, software
%%  distributed under the License is distributed on an "AS IS" BASIS,
%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%  See the License for the specific language governing permissions and
%%  limitations under the License.
%% =============================================================================

-module(bondy_security_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    common:all().

groups() ->
    [
        {main, [sequence], [
            create_realm,
            enable_security,
            security_enabled,
            disable_security,
            security_disabled,
            api_client_add,
            api_client_auth1,
            api_client_update,
            api_client_auth2,
            api_client_auth3,
            api_client_delete
        ]}
    ].

init_per_suite(Config) ->
    common:start_bondy(),
    [{realm_uri, <<"com.myrealm">>}|Config].

end_per_suite(Config) ->
    common:stop_bondy(),
    {save_config, Config}.


create_realm(Config) ->
    Realm = bondy_realm:add(?config(realm_uri, Config)),
    realm = element(1, Realm),
    {save_config, [{realm, Realm} | Config]}.

enable_security(Config) ->
    {create_realm, Prev} = ?config(saved_config, Config),
    ok = bondy_realm:enable_security(?config(realm, Prev)),
    {save_config, Prev}.

security_enabled(Config) ->
    {enable_security, Prev} = ?config(saved_config, Config),
    R = ?config(realm, Prev),
    enabled = bondy_realm:security_status(R),
    true = bondy_realm:is_security_enabled(R),
    {save_config, Prev}.

disable_security(Config) ->
    {security_enabled, Prev} = ?config(saved_config, Config),
    ok = bondy_realm:disable_security(?config(realm, Prev)),
    {save_config, Prev}.

security_disabled(Config) ->
    {disable_security, Prev} = ?config(saved_config, Config),
    R = ?config(realm, Prev),
    disabled = bondy_realm:security_status(R),
    false = bondy_realm:is_security_enabled(R),
    {save_config, Prev}.

api_client_add(Config) ->
    {security_disabled, Prev} = ?config(saved_config, Config),
    {ok, #{
        <<"client_id">> := Id,
        <<"client_secret">> := Secret
    }} = bondy_api_client:add(?config(realm_uri, Prev), #{}),
    {save_config, [{client_id, Id}, {client_secret, Secret} | Prev]}.

api_client_auth1(Config) ->
    {api_client_add, Prev} = ?config(saved_config, Config),
    Uri = ?config(realm_uri, Prev),
    Id = ?config(client_id, Prev),
    Secret = ?config(client_secret, Prev),
    {ok, _} = bondy_security:authenticate(Uri, Id, Secret, [{ip, {127,0,0,1}}]),
    {save_config, Prev}.

api_client_update(Config) ->
    {api_client_auth1, Prev} = ?config(saved_config, Config),
    Secret = <<"New-Password">>,
    ok = bondy_api_client:update(
        ?config(realm_uri, Prev),
        ?config(client_id, Prev),
        #{
            <<"client_secret">> => Secret,
            <<"meta">> => #{<<"foo">> => <<"bar">>}
        }
    ),
    {save_config,
        lists:keyreplace(client_secret, 1, Prev, {client_secret, Secret})}.

api_client_auth2(Config) ->
    {api_client_update, Prev} = ?config(saved_config, Config),
    Uri = ?config(realm_uri, Prev),
    Id = ?config(client_id, Prev),
    Secret = ?config(client_secret, Prev),
    {ok, _} = bondy_security:authenticate(Uri, Id, Secret, [{ip, {127,0,0,1}}]),
    {save_config, Prev}.

api_client_auth3(Config) ->
    {api_client_auth2, Prev} = ?config(saved_config, Config),
    Uri = ?config(realm_uri, Prev),
    Id = unicode_util_compat:uppercase(?config(client_id, Prev)),
    Secret = ?config(client_secret, Prev),
    {ok, _} = bondy_security:authenticate(Uri, Id, Secret, [{ip, {127,0,0,1}}]),
    {save_config, Prev}.

api_client_delete(Config) ->
    {api_client_auth3, Prev} = ?config(saved_config, Config),
    ok = bondy_api_client:remove(
        ?config(realm_uri, Config), ?config(client_id, Prev)),
    {save_config, Prev}.