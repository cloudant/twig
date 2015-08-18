% Copyright 2011 Cloudant
%
% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(twig).

-export([log/2, log/3, log/4, set_level/1]).

-include("twig_int.hrl").

set_level(LevelAtom) ->
    Level = twig_util:level(LevelAtom),
    application:set_env(twig, level, Level),
    gen_event:call(error_logger, twig_event_handler, {set_level, Level}).

log(LevelAtom, String) ->
    log(LevelAtom, String, [], []).

log(LevelAtom, Format, Data) ->
    log(LevelAtom, Format, Data, []).

log(LevelAtom, Format, Data, Options) ->
    %% TODO do something useful with options
    Level = twig_util:level(LevelAtom),
    Facility = get_facility(Options),
    case application:get_env(twig, level) of
        {ok, Threshold} when Level =< Threshold ->
            send_message(Level, Facility, Format, Data);
        undefined when Level =< ?LEVEL_INFO ->
            send_message(Level, Facility, Format, Data);
        _ ->
            ok
    end.

%% internal

send_message(Level, Facility, Format, Data) ->
    gen_event:sync_notify(error_logger, format(Level, Facility, Format, Data)).

format(Level, Facility, Format, Data) ->
    %% TODO truncate large messages
    #twig{
        level = Level,
        facility = Facility,
        msg = iolist_to_binary(twig_util:format(Format, Data)),
        msgid = erlang:get(nonce),
        pid = self()
    }.

get_facility(Options) ->
    case proplists:get_value(facility, Options) of
        undefined ->
            undefined;
        Facility ->
            twig_util:facility(Facility)
    end.
