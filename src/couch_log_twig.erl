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

-module(couch_log_twig).

-behaviour(couch_log).

-export([
    debug/2,
    info/2,
    notice/2,
    warning/2,
    error/2,
    critical/2,
    alert/2,
    emergency/2,
    set_level/1
]).

debug(Fmt, Args) ->
    twig:log(debug, Fmt, Args).

info(Fmt, Args) ->
    twig:log(info, Fmt, Args).

notice(Fmt, Args) ->
    twig:log(notice, Fmt, Args).

warning(Fmt, Args) ->
    twig:log(warning, Fmt, Args).

error(Fmt, Args) ->
    twig:log(error, Fmt, Args).

critical(Fmt, Args) ->
    twig:log(crit, Fmt, Args).

alert(Fmt, Args) ->
    twig:log(alert, Fmt, Args).

emergency(Fmt, Args) ->
    twig:log(emerg, Fmt, Args).

set_level(Level) ->
    twig:set_level(Level).
