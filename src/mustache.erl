%% The MIT License
%%
%% Copyright (c) 2015 Emil Falk <emph@riseup.net>
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

-module(mustache).
-export([parse/1, tokenize/1, get/2, get_binary/2, escape/1]).

-include_lib("eunit/include/eunit.hrl").


get([], [Item | _Contexts]) -> {ok, Item};
get(Key, Contexts)          -> context_get(Key, Contexts).

get_binary(Key, Contexts) ->
    case get(Key, Contexts) of
        undefined   -> <<>>;
        {ok, Value} -> to_binary(Value)
    end.

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

escape(Binary) when is_binary(Binary) -> escape(unicode:characters_to_list(Binary));
escape(String) when is_list(String)   -> escape(String, []).

escape([], Acc)          -> unicode:characters_to_binary(lists:reverse(Acc));
escape([$< | Rest], Acc) -> escape(Rest, [<<"&lt;">> | Acc]);
escape([$> | Rest], Acc) -> escape(Rest, [<<"&gt;">> | Acc]);
escape([$& | Rest], Acc) -> escape(Rest, [<<"&amp;">> | Acc]);
escape([X | Rest], Acc)  -> escape(Rest, [X | Acc]).

parse(Template) when is_binary(Template) ->
    parse(tokenize(Template));
parse(Tokens) when is_list(Tokens) ->
  parse(Tokens, [], []).

parse([], [], Acc) ->
    lists:reverse(Acc);
parse([Token | Rest], Path, Acc) ->
    case Token of
        {Section, Key} when Section =:= section;
                            Section =:= inverted_section ->
            {Body, AfterSection} = parse(Rest, [Key | Path], []),
            parse(AfterSection, Path, [{Section, Key, Body} | Acc]);
        {end_section, Key} when Key =:= hd(Path) ->
            {lists:reverse(Acc), Rest};
        {Tag, _Key} when Tag =:= escaped;
                         Tag =:= unescaped ->
            parse(Rest, Path, [Token | Acc])
    end.

tokenize(Template) ->
    Regexp = compile_regexp(<<"{{">>, <<"}}">>),
    tokenize(Template, Regexp, []).

tokenize(<<>>, _Regexp, Acc) ->
  lists:reverse(Acc);
tokenize(Template, Regexp, Acc0) ->
    case re:run(Template, Regexp) of
        nomatch ->
            tokenize(<<>>, Regexp, [Template | Acc0]);
        {match, Match} ->
            {Front, Tag, Back} = split_template(Template, fix_match(Match)),
            Acc1 = case Front of
                     <<>> -> Acc0;
                     _    -> [Front | Acc0]
                   end,
            case Tag of
              {delimiters, Left, Right} -> tokenize(Back, compile_regexp(Left, Right), Acc1);
              {comment, _Comment}       -> tokenize(Back, Regexp, Acc1);
              _                         -> tokenize(Back, Regexp, [Tag | Acc1])
            end
    end.

fix_match([_, {-1, 0}, _, _, _ | Rest]) -> fix_match(Rest);
fix_match([_, Total, Left, Tag, Right]) -> [Left, Right, {Total, Tag}];
fix_match([Total, Left, Tag]) -> [Left, {Total, Tag}].

split_template(Template, [{-1, 0}, Rest]) ->
    split_template(Template, [none, Rest]);
split_template(Template, [{Left, 1} | Rest]) ->
    split_template(Template, [binary:at(Template, Left) | Rest]);
split_template(Template, [Left, {Right, 1} | Rest]) when is_integer(Left) ->
    true = matching(Left, binary:at(Template, Right)),
    split_template(Template, [Left | Rest]);
split_template(Template, [Type, {{Start, Size}, {TagStart, TagSize}}]) ->
    Front = erlang:binary_part(Template, 0, Start),
    Tag = erlang:binary_part(Template, TagStart, TagSize),
    BackStart = Start + Size,
    BackSize = byte_size(Template) - BackStart,
    Back = erlang:binary_part(Template, BackStart, BackSize),
    Token = token(Type, Tag),
    {Front, Token, Back}.

token(none, Tag) -> {escaped, parse_key(Tag)};
token($#, Tag) -> {section, parse_key(Tag)};
token($^, Tag) -> {inverted_section, parse_key(Tag)};
token($/, Tag) -> {end_section, parse_key(Tag)};
token(${, Tag) -> {unescaped, parse_key(Tag)};
token($&, Tag) -> {unescaped, parse_key(Tag)};
token($!, Comment) -> {comment, Comment};
token($=, Delimiters) ->
    [Left, Right] = re:split(Delimiters, <<" ">>, [unicode]),
    {delimiters, Left, Right}.

parse_key(Tag) -> re:split(Tag, <<"\\.">>, [trim, unicode]).

matching(${, $}) -> true;
matching($=, $=) -> true;
matching(_,  _)  -> false.

compile_regexp(Left0, Right0) ->
    {Left1, Right1} = tag_start_end(Left0, Right0),
    TagRE = ["(", Left1, "({|=)(.+?)(}|=)", Right1, ")|(", Left1, "(#|\\^|!|&|/)?\\s*(.+?)", Right1, ")"],
    {ok, CompiledTagRE} = re:compile(TagRE, [unicode]),
    CompiledTagRE.

tag_start_end(Left, Right) ->
    LeftStr = unicode:characters_to_list(Left),
    RightStr = unicode:characters_to_list(Right),
    {escape_string(LeftStr), escape_string(RightStr)}.

escape_string(String) -> lists:map(fun (Char) -> [$\\, Char] end, String).

-ifdef(EUNIT).

parse_test_() ->
    [?_assertEqual(
        [
         {section, [<<"a">>],
          [
           {section, [<<"aa">>], [{escaped, [<<"x">>]}]},
           {section, [<<"ab">>], [{unescaped, [<<"y">>]}]}
          ]},
         {inverted_section, [<<"b">>], [{escaped, [<<"z">>]}]}
        ],
        parse(<<"{{#a}}{{#aa}}{{x}}{{/aa}}{{#ab}}{{&y}}{{/ab}}{{/a}}{{^b}}{{z}}{{/b}}">>)
       )].

tokenize_test_() ->
    [?_assertEqual(
        [<<"a">>, {escaped, [<<"apa">>]}, <<"b">>, {escaped, [<<"bepa">>]}, <<"c">>, {escaped, [<<"cepa">>]}, <<"d">>],
        tokenize(<<"a{{apa}}b{{bepa}}c{{cepa}}d">>)
       ),
     ?_assertEqual(
        [{escaped, [<<"a">>]}, {unescaped, [<<"b">>]}, {section, [<<"c">>]}, {inverted_section, [<<"d">>]},
         {end_section, [<<"e">>]}, {unescaped, [<<"g">>]}],
        tokenize(<<"{{a}}{{{b}}}{{#c}}{{^d}}{{/e}}{{!f}}{{=[[ ]]=}}[[&g]]">>)
       )
    ].

compile_regexp_test_() ->
    TagRE = compile_regexp(<<"{{">>, <<"}}">>),
    TestTagRE = fun (Template) -> fix_match(element(2, re:run(Template, TagRE))) end,

    [
     %% Tag
     ?_assertEqual([{-1, 0}, {{1, 7}, {3, 3}}], TestTagRE(<<"X{{apa}}Y{{bepa}}Z">>)),
     ?_assertEqual([{3, 1}, {7, 1}, {{1, 9}, {4, 3}}], TestTagRE(<<"X{{{apa}}}Y{{bepa}}Z">>)),
     ?_assertEqual([{3, 1}, {7, 1}, {{1, 9}, {4, 3}}], TestTagRE(<<"X{{=apa=}}Y{{bepa}}Z">>)),
     ?_assertEqual([{3, 1}, {{1, 8}, {4, 3}}], TestTagRE(<<"X{{#apa}}Y{{bepa}}Z">>)),
     ?_assertEqual([{3, 1}, {{1, 8}, {4, 3}}], TestTagRE(<<"X{{^apa}}Y{{bepa}}Z">>)),
     ?_assertEqual([{3, 1}, {{1, 8}, {4, 3}}], TestTagRE(<<"X{{!apa}}Y{{bepa}}Z">>)),
     ?_assertEqual([{3, 1}, {{1, 8}, {4, 3}}], TestTagRE(<<"X{{/apa}}Y{{bepa}}Z">>)),
     ?_assertEqual([{3, 1}, {{1, 10}, {6, 3}}], TestTagRE(<<"X{{#  apa}}Y{{bepa}}Z">>))
    ].

-endif.
