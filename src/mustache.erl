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
-export([compile/1, render/2]).

-record(state, {
          this,
          contexts = #{},
          section_re,
          tag_re
         }).

-include_lib("eunit/include/eunit.hrl").

compile(Template) when is_binary(Template) ->
    fun (Context) ->
        iolist_to_binary(sections(Template, set_delimiters(<<"{{ }}">>, #state{contexts = [Context]})))
    end.

render(Template, Context) when (is_list(Template) orelse is_binary(Template)) andalso is_map(Context) ->
    Render = compile(Template),
    Render(Context);
render(Render, Context) when is_function(Render, 1), is_map(Context) ->
    Render(Context).

sections(<<>>, _State) ->
    [];
sections(Template, #state{section_re = SectionRE} = State) ->
    case re:run(Template, SectionRE) of
        {match, [{M0, M1}, {K0, K1}, {N0, N1}, {C0, C1}]} ->
            Front = erlang:binary_part(Template, 0, M0),
            End = M0 + M1,
            Rest = erlang:binary_part(Template, End, byte_size(Template) - End),
            Kind = erlang:binary_part(Template, K0, K1),
            Name = erlang:binary_part(Template, N0, N1),
            Content = erlang:binary_part(Template, C0, C1),
            [tags(Front, State),
             section(Kind, Name, Content, State),
             sections(Rest, State)];
        nomatch ->
            tags(Template, State)
    end.

section(<<"#">>, Name, Content, #state{contexts = Contexts} = State) ->
    case get(Name, State) of
        undefined ->
            [];
        {ok, Value} ->
            case Value of
                %% TODO: Empty string is not falsy according to the specification
                False when False =:= false;
                           False =:= [];
                           False =:= <<>> ->
                    [];
                [Context | _] = SubContexts when is_map(Context) ->
                    [sections(Content, State#state{contexts = [SubContext | Contexts]})
                     || SubContext <- SubContexts];
                List when is_list(List) ->
                    [sections(Content, State#state{this = Item}) || Item <- List];
                SubContext when is_map(SubContext) ->
                    sections(Content, State#state{contexts = [SubContext | Contexts]});
                Fun when is_function(Fun, 1) ->
                    sections(Fun(Content), State);
                Other ->
                    sections(Content, State#state{this = Other})
            end
    end;
section(<<"^">>, Name, Content, State) ->
    case get(Name, State) of
        undefined -> sections(Content, State);
        %% TODO: Empty string is not falsy according to the specification
        {ok, False} when False =:= false;
                         False =:= [];
                         False =:= <<>> -> sections(Content, State);
        {ok, _Other} -> []
    end.

tags(Template, #state{tag_re = TagRE} = State0) ->
    case re:run(Template, TagRE) of
        {match, [{M0, M1}, K, {C0, C1} | _Rest]} ->
            Front = erlang:binary_part(Template, 0, M0),
            End = M0 + M1,
            Back = erlang:binary_part(Template, End, byte_size(Template) - End),
            Content = erlang:binary_part(Template, C0, C1),
            Kind = kind(Template, K),
            {Result, State1} = tag(Kind, Content, State0),
            Rest = case Kind of
                 <<"=">> -> sections(Back, State1);
                 _       -> tags(Back, State1)
             end,
            [Front, Result, Rest];
        nomatch ->
            Template
    end.

kind(_Template, {-1, 0}) -> none;
kind(Template, {K0, K1}) -> erlang:binary_part(Template, K0, K1).

tag(none, Content, State)     -> {escaped_tag(Content, State), State};
tag(<<"&">>, Content, State)  -> {unescaped_tag(Content, State), State};
tag(<<"{">>, Content, State)  -> {unescaped_tag(Content, State), State};
tag(<<"!">>, _Content, State) -> {[], State};
tag(<<"=">>, Content, State)  -> {[], set_delimiters(Content, State)}.

escaped_tag(Name, State) ->
    case get(Name, State) of
        undefined   -> [];
        {ok, Value} -> escape(Value)
    end.

unescaped_tag(Name, State) ->
    case get(Name, State) of
        undefined   -> [];
        {ok, Value} -> to_binary(Value)
    end.

dot_notation(<<".">>) -> this;
dot_notation(Content) -> re:split(Content, <<"\\.">>, [unicode]).

get(Name, State) when is_binary(Name) -> get(dot_notation(Name), State);
get(this, #state{this = This}) -> {ok, This};
get(Path, #state{contexts = Contexts}) -> context_get(Path, Contexts).

context_get(_Path, []) ->
    undefined;
context_get(Path, [Context | Contexts]) ->
    case get_path(Path, Context) of
        {ok, Value} -> {ok, Value};
        undefined   -> context_get(Path, Contexts)
    end.

get_path([], Value) ->
    {ok, Value};
get_path([Key | Keys], Map) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> get_path(Keys, Value);
        error       -> undefined
    end.

to_binary(Value) when is_integer(Value) -> integer_to_binary(Value);
to_binary(Value) when is_float(Value)   -> float_to_binary(Value, [{decimals, 2}]);
to_binary(Value) when is_atom(Value)    -> atom_to_binary(Value, utf8);
to_binary(Value) when is_binary(Value)  -> Value.

escape(Binary) when is_binary(Binary) -> escape(unicode:characters_to_list(Binary), []);
escape(Term)                          -> escape(to_binary(Term)).

escape([],          Acc) -> unicode:characters_to_binary(lists:reverse(Acc));
escape([$< | Rest], Acc) -> escape(Rest, [<<"&lt;">> | Acc]);
escape([$> | Rest], Acc) -> escape(Rest, [<<"&gt;">> | Acc]);
escape([$& | Rest], Acc) -> escape(Rest, [<<"&amp;">> | Acc]);
escape([X | Rest],  Acc) -> escape(Rest, [X | Acc]).

set_delimiters(Content, State) ->
    [Left, Right] = re:split(Content, <<" ">>, [{return, list}]),
    {ok, CompiledSectionRE} = section_re(Left, Right),
    {ok, CompiledTagRE} = tag_re(Left, Right),
    State#state{section_re = CompiledSectionRE, tag_re = CompiledTagRE}.

section_re(Left0, Right0) ->
    {{_, Left1}, {[EndChar | _], Right1}} = escape_delimiters(Left0, Right0),
    Stop = [<<"([^">>, escape_char(EndChar), <<"]*)">>],
    Regexp = [Left1, <<"(#|\\^)">>, Stop, Right1, <<"(.+?)">>, Left1, <<"/\\2">>, Right1],
    re:compile(Regexp, [dotall, unicode]).

tag_re(Left0, Right0) ->
    {{_,Left1}, {_,Right1}} = escape_delimiters(Left0, Right0),
    Regexp = [Left1, "(#|=|!|>|{|&)?\\h*(.+?)\\h*(=|})?", Right1],
    re:compile(Regexp, [dotall, unicode]).

escape_delimiters(Left0, Right0) ->
    {Left1, Right1} = {unicode:characters_to_list(Left0), unicode:characters_to_list(Right0)},
    {{Left1, escape_delimiter(Left1)}, {Right1, escape_delimiter(Right1)}}.

escape_delimiter(Delimiter) ->
    unicode:characters_to_binary(lists:map(fun escape_char/1, Delimiter)).

escape_char(Char) -> [$\\, Char].
