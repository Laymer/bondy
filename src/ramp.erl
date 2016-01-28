-module(ramp).
-include ("ramp.hrl").

-export([error/2]).
-export([error/3]).
-export([error_uri/1]).
-export([send/2]).
-export([is_open/1]).
-export([start/0]).
-export([make/0]).


%% =============================================================================
%% API
%% =============================================================================


start() ->
    application:ensure_all_started(ramp).


%% @doc Sends a message over a trasport to a peer. If the transport is not open it fails with an exception.
send(#goodbye{} = _M, _Ctxt0) ->
    %% do_send(M, Ctxt#{goodbye_initiated => true});
    error(not_yet_implemented);

send(_Message, _Ctxt) ->
    error(not_yet_implemented).

is_open(_Transport) ->
    error(not_yet_implemented).


%% =============================================================================
%% API - SESSION
%% =============================================================================




%% =============================================================================
%% API - SUBSCRIBER ROLE
%% =============================================================================



%% =============================================================================
%% API - PUBLISHER ROLE
%% =============================================================================



%% =============================================================================
%% API - CALLER ROLE
%% =============================================================================




%% =============================================================================
%% API - CALLEE ROLE
%% =============================================================================




%% =============================================================================
%% API - UTILS
%% =============================================================================

make() ->
    make:all([load]).

error_uri(Reason) when is_atom(Reason) ->
    R = list_to_binary(atom_to_list(Reason)),
    <<"com.williamhill.error.", R/binary>>.

error(Code, Description) ->
    #{
        <<"code">> => Code,
        <<"description">> => Description
    }.

error(Code, Description, UserInfo) ->
    #{
    	<<"code">> => Code,
    	<<"description">> => Description,
        <<"userInfo">> => UserInfo
    }.
