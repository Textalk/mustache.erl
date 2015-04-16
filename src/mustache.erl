%% The MIT License
%%
%% Copyright (c) 2009 Tom Preston-Werner <tom@mojombo.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

%% See the README at http://github.com/mojombo/mustache.erl for additional
%% documentation and usage examples.

-module(mustache).
-author("Tom Preston-Werner").
-export([compile/1]).

-record(state, {
          left = <<"{{({|">>,
          right = <<"}}">>,
          path = []
         }).

-include_lib("eunit/include/eunit.hrl").

compile(Template) when is_binary(Template) ->
    tags(Template, #state{}, []).

tags(<<>>, _State, Acc) -> lists:reverse(Acc);
tags(Template, #state{left = Left} = State0) ->
    case re:run(Template, Left) of
        {match, [{M0, M1} | Type]} ->

            Front = erlang:binary_part(Template, 0, M0),
            Rest
            Type = tag_type(Type, Template) of
                none -> parse_tag(
        nomatch ->
            [Template | Acc]
    end.

kind(_Template, []) -> none;
kind(Template, [{K0, K1}]) ->erlang:binary_part(Template, K0, K1).

tag(<<"=">>, Content, State)  ->
    {[], set_delimiters(Content, State)};
tag(Type, Content, State) ->
    {{tag, Type, Content}, State}.

parse_key(<<".">>) -> this;
parse_key(Content) -> re:split(Content, <<"\\.">>, [unicode]).
