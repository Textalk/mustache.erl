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
-export([compile/1, get/2, get_binary/2, escape/1]).

-record(state, {
          regexp,
          contexts,
          path = [],
          depth = 0
         }).

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

context_var(Depth) ->
    erl_syntax:variable("Ctx" ++ integer_to_list(Depth)).

compile(Template) ->
    Context = context_var(0),
    Regexp = compile_regexp(<<"{{">>, <<"}}">>),
    State = #state{contexts = [Context], regexp = Regexp},
    case compile(Template, State, []) of
        {ok, Body} ->
            Clause = erl_syntax:clause(Context, [], [Body]),
            RenderFunExpr = erl_syntax:fun_expr([Clause]),
            {value, RenderFun, _} = erl_eval:expr(
                                      RenderFunExpr,
                                      erl_eval:new_bindings()
                                     ),
            {ok, RenderFun};
        {error, Reason} ->
            {error, Reason}
    end.

compile(<<>>, #state{path = [], depth = 0}, ASTAcc) ->
    {ok, erl_syntax:list(lists:reverse(ASTAcc))};
compile(<<>>, _State, _ASTAcc) ->
    {error, unterminated_sections};
compile(Template, State, ASTAcc0) ->
    #state{regexp = Regexp, contexts = Contexts} = State0,
    case re:run(Template, Regexp) of
        nomatch ->
            compile(<<>>, State, [erl_syntax:abstract(Template) | ASTAcc0]);
        {match, Match0} ->
            {Front, Tag, Back} = split_template(Template, fix_match(Match0)),
            ASTAcc1 = case Front of
                          undefined -> ASTAcc0;
                          FrontNode -> [FrontNode | ASTAcc0]
                      end,
            compile_tag(Tag, Back, State, ASTAcc1)
    end.

compile_tag({delimiter, Left, Right}, Back, State, ASTAcc) ->
    compile(Back, State#state{regexp = compile_regexp(Left, Right)}, ASTAcc);
compile_tag({comment, _Comment}, Back, State, ASTAcc) ->
    compile(Back, State, ASTAcc);
compile_tag({escaped, Key}, Back, State, ASTAcc) ->
    compile(Back, State, [get_escaped_ast(Key, State) | ASTAcc]);
compile_tag({unescaped, Key}, Back, State, ASTAcc) ->
    compile(Back, State, [get_unescaped_ast(Key, State) | ASTAcc]);
compile_tag({section, Key}, Back0, State0, ASTAcc) ->
    {Back1, SectionAST, State1} = section_ast(Key, Back0, State0)
    compile(Back1, State2, [SectionAST | ASTAcc]);
compile_tag({end_section, Key}, Back, #state{path = [Key | Path]} = State0,
            ASTAcc) ->
    #state{contexts = [_ | Contexts], depth = Depth} = State0,
    State1 = State0#state{
               contexts = Contexts,
               depth    = Depth - 1,
               path     = Path
              },
    {Back, erl_syntax:list(lists:reverse(ASTAcc)), State1}.

get_ast(Key, #state{contexts = Contexts0}) ->
    Contexts1 = erl_syntaxt:list(
                  lists:map(fun (Context) -> erl_syntax:variable(Context) end)
                 ),
    erl_syntax:application(
      erl_syntax:atom(?MODULE), erl_syntax:atom(get),
      [erl_syntax:abstract(Key), Contexts1]
     ).

get_escaped_ast(Key, State) ->
    erl_syntax:application(
      erl_syntax:atom(?MODULE),
      erl_syntax:atom(escape),
      [get_unescaped_ast(Key, State)]
     ).

get_unescaped_ast(Key, State) ->
    erl_syntax:application(
      erl_syntax:atom(?MODULE), erl_syntax:atom(to_binary),
      [get_ast(Key, State)]
     ).

section_ast(Key, Back, #state{contexts = Contexts} = State) ->
    erl_syntax:case_expr(
      get_ast(Key, Contexts),
      [false_clause_ast(Contexts),
       list_clause_ast(Var)]
     ).

false_clause_ast(Contexts) ->
    VariableName = erl_syntax_lib:new_variable_name(sets:from_list(Contexts)),
    Variable = erl_syntax:variable(VariableName),
    erl_syntax:clause(
      [Variable],
      [false_guard_ast(Variable, false),
       false_guard_ast(Variable, []),
       false_guard_ast(Variable, <<>>)],
      [erl_syntax:nil()]
     ).

false_guard_ast(Var, Falsy) ->
       erl_syntax:infix_expr(
         Var,
         erl_syntax:operator('=:='),
         erl_syntax:abstract(Falsy)
        ).

fix_match([_, {-1, 0}, _, _, _ | Rest]) -> fix_match(Rest);
fix_match([_, Total, Left, Tag, Right]) -> [Left, Right, {Total, Tag}];
fix_match([Total, Left, Tag]) -> [Left, {Total, Tag}].

split_template(Template, Match) ->
    {Front0, Token, Back} = split_template_(Template, Match),
    Front1 = case Front0 of
                 <<>> -> undefined;
                 _    -> erl_syntax:abstract(Front0)
             end,
    {Front1, Token, Back}.

split_template_(Template, [{-1, 0}, Rest]) ->
    split_template_(Template, [none, Rest]);
split_template_(Template, [{Left, 1} | Rest]) ->
    split_template_(Template, [binary:at(Template, Left) | Rest]);
split_template_(Template, [Left, {Right, 1} | Rest]) when is_integer(Left) ->
    true = matching(Left, binary:at(Template, Right)),
    split_template_(Template, [Left | Rest]);
split_template_(Template, [Type, {{Start, Size}, {TagStart, TagSize}}]) ->
    Front = erlang:binary_part(Template, 0, Start),
    Tag = erlang:binary_part(Template, TagStart, TagSize),
    BackStart = Start + Size,
    BackSize = byte_size(Template) - BackStart,
    Back = erlang:binary_part(Template, BackStart, BackSize),
    Token = parse_tag(Type, Tag),
    {Front, Token, Back}.

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
