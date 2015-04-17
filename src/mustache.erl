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
          contexts,
          path = [],
          depth = 0
         }).

-include_lib("eunit/include/eunit.hrl").

compile(Template) ->
    Tokens = tokenize(Template),
    Result = parse_ast(Tokens),
    erl_syntax:revert(Result).

parse_ast(Tokens) ->
    Contexts = [context_var(0)],
    State = #state{contexts = Contexts},
    erl_syntax:fun_expr([erl_syntax:clause(Contexts, [], [parse_ast(Tokens, State, [])])]).

parse_ast([], #state{path = [], depth = 0}, AST) ->
    erl_syntax:list(lists:reverse(AST));
parse_ast([{escaped, Key} | Tokens], #state{contexts = Contexts} = State, AST) ->
    Node = erl_syntax:application(
             erl_syntax:atom(?MODULE),
             erl_syntax:atom(escape),
             [erl_syntax:application(
                erl_syntax:atom(?MODULE),
                erl_syntax:atom(get_binary),
                [erl_syntax:abstract(Key),
                 erl_syntax:list(Contexts)]
               )]
            ),
    parse_ast(Tokens, State, [Node | AST]);
parse_ast([Binary | Tokens], State, AST) when is_binary(Binary) ->
    Node = erl_syntax:abstract(Binary),
    parse_ast(Tokens, State, [Node | AST]).

context_var(Depth) ->
    erl_syntax:variable("Ctx" ++ integer_to_list(Depth)).

tokenize(Template) ->
    tokenize(Template, compile_regexp(<<"{{">>, <<"}}">>), []).

tokenize(<<>>, _RE, Tokens) -> lists:reverse(Tokens);
tokenize(Template, RE, Tokens0) ->
    case re:run(Template, RE) of
        nomatch ->
            lists:reverse([Template | Tokens0]);
        {match, Match0} ->
            {Front, Token, Rest} = parse_token(Template, fix_match(Match0)),
            Tokens1 = case Front of <<>> -> Tokens0; _ -> [Front | Tokens0] end,
            case Token of
                {delimiters, Left, Right} ->
                    tokenize(Rest, compile_regexp(Left, Right), Tokens1);
                {comment, _Comment} ->
                    tokenize(Rest, RE, Tokens1);
                _Other ->
                    tokenize(Rest, RE, [Token | Tokens1])
            end
    end.

fix_match([_, {-1, 0}, _, _, _ | Rest]) -> fix_match(Rest);
fix_match([_, Total, Left, Tag, Right]) -> [Left, Right, {Total, Tag}];
fix_match([Total, Left, Tag]) -> [Left, {Total, Tag}].

parse_token(Template, [{-1, 0}, Rest]) ->
    parse_token(Template, [none, Rest]);
parse_token(Template, [{Left, 1} | Rest]) ->
    parse_token(Template, [binary:at(Template, Left) | Rest]);
parse_token(Template, [Left, {Right, 1} | Rest]) when is_integer(Left) ->
    true = matching(Left, binary:at(Template, Right)),
    parse_token(Template, [Left | Rest]);
parse_token(Template, [Type, {{Start, Size}, {TagStart, TagSize}}]) ->
    Front = erlang:binary_part(Template, 0, Start),
    Tag = erlang:binary_part(Template, TagStart, TagSize),
    RestStart = Start + Size,
    Rest = erlang:binary_part(Template, RestStart, byte_size(Template) - RestStart),
    Token = parse_tag(Type, Tag),
    {Front, Token, Rest}.

parse_tag(none, Tag) -> {escaped, parse_key(Tag)};
parse_tag($#, Tag) -> {section, parse_key(Tag)};
parse_tag($^, Tag) -> {inverted_section, parse_key(Tag)};
parse_tag($/, Tag) -> {end_section, parse_key(Tag)};
parse_tag(${, Tag) -> {unescaped, parse_key(Tag)};
parse_tag($&, Tag) -> {unescaped, parse_key(Tag)};
parse_tag($!, Comment) -> {comment, Comment};
parse_tag($=, Delimiters) ->
    [Left, Right] = re:split(Delimiters, <<" ">>, [unicode]),
    {delimiters, Left, Right}.

parse_key(Tag) -> re:split(Tag, <<"\\.">>, [trim, unicode]).

matching(${, $}) -> true;
matching($=, $=) -> true;
matching(_, _)   -> false.

compile_regexp(Left0, Right0) ->
    {Left1, Right1} = tag_start_end(Left0, Right0),
    TagRE = ["(", Left1, "({|=)(.+?)(}|=)", Right1, ")|(", Left1, "(#|\\^|!|&|/)?\\s*(.+?)", Right1, ")"],
    {ok, CompiledTagRE} = re:compile(TagRE, [unicode]),
    CompiledTagRE.

tag_start_end(Left, Right) ->
    LeftStr = unicode:characters_to_list(Left),
    RightStr = unicode:characters_to_list(Right),
    {escape(LeftStr), escape(RightStr)}.

escape(Str) -> lists:map(fun (Char) -> [$\\, Char] end, Str).

-ifdef(EUNIT).

compile_test_() ->
    CompileTest = fun (Template) -> erl_prettypr:format(compile(Template)) end,
    [?_assertEqual(
        "fun (Ctx0) -> [<<116, 101, 115, 116>>] end",
        CompileTest(<<"test">>)
     ),
     ?_assertEqual(
        "fun (Ctx0) -> [escape(mustache:get_binary([<<97>>], [Ctx0]))] end",
        CompileTest(<<"{{a}}">>)
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
