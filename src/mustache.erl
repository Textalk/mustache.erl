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
-export([render/2, compile/1, get/2, to_binary/2]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {
          path  = []                                 :: [[binary()]],
          depth = 0                                  :: non_neg_integer(),
          ctx                                        :: erl_syntax:synaxTree(),
          re    = compile_regexp(<<"{{">>, <<"}}">>) :: re:mp(),
          ix    = 0                                  :: non_neg_integer()
         }).

-define(VAR(N), erl_syntax:variable(N)).
-define(ABS(T), erl_syntax:abstract(T)).
-define(APP(M, F, A), erl_syntax:application(erl_syntax:atom(M), erl_syntax:atom(F), A)).
-define(BIN(Start, Length), ?APP(erlang, binary_part, [?VAR('Template'), ?ABS(Start), ?ABS(Length)])).
-define(LIST(I), erl_syntax:list(I)).
-define(GET(Key, Ctx), ?APP(?MODULE, get, [?ABS(Key), Ctx])).
-define(GETB(Key, Ctx, Escaped), ?APP(?MODULE, to_binary, [?GET(Key, Ctx), ?ABS(Escaped)])).
-define(ADD(Ctx, Var, Prev), erl_syntax:match_expr(Ctx, erl_syntax:list([Var], Prev))).
-define(CASE(Expr, Clauses), erl_syntax:case_expr(Expr, Clauses)).

get([], [Item | _Contexts]) -> {ok, Item};
get(Key, Contexts)          -> context_get(Key, Contexts).

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

to_binary({ok, Value}, true)   -> escape(to_binary(Value));
to_binary({ok, Value}, false)  -> to_binary(Value);
to_binary(undefined, _Escaped) -> <<>>.

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

render(Template, Ctx) when is_binary(Template), is_map(Ctx) ->
    Render = compile(Template),
    Render(Ctx);
render(Render, Ctx) when is_function(Render, 1), is_map(Ctx) ->
    Render(Ctx).

var(Prefix, Depth) when is_list(Prefix), is_integer(Depth) ->
    ?VAR(Prefix ++ integer_to_list(Depth)).

compile(Template) when is_list(Template) ->
    compile(unicode:characters_to_binary(Template));
compile(Template) when is_binary(Template) ->
    State = #state{ctx = var("C", 0)},
    Ctx = ?VAR('Ctx'),
    Match = ?ADD(State#state.ctx, Ctx, erl_syntax:nil()),
    Body = ?APP(erlang, iolist_to_binary, compile(Template, State, [])),
    FunST = erl_syntax:fun_expr([erl_syntax:clause([Ctx], [], [Match, Body])]),
    FunExpr = erl_syntax:revert(FunST),
    Bindings0 = erl_eval:new_bindings(),
    Bindings1 = erl_eval:add_binding('Template', Template, Bindings0),
    {value, Render, _Bindings} = erl_eval:expr(FunExpr, Bindings1),
    Render.

compile(<<>>, #state{path = []}, Acc) ->
  [?LIST(lists:reverse(Acc))];
compile(Rest, State0, Acc0) ->
    #state{ix = Ix0, re = Re0, ctx = Ctx0, path = Path, depth = Depth} = State0,
    case re:run(Rest, Re0) of
        nomatch ->
            Size = byte_size(Rest),
            State1 = State0#state{ix = Ix0 + Size},
            compile(<<>>, State1, [?BIN(Ix0, Size) | Acc0]);
        {match, Match} ->
            {FrontLength, Tag, Back, Total} = split_template(Rest, fix_match(Match)),
            #state{ix = Ix1} = State1 = State0#state{ix = Ix0 + Total},
            Acc1 = case FrontLength of 0 -> Acc0; _ -> [?BIN(Ix0, FrontLength) | Acc0] end,
            case Tag of
                {tag, Escaped, Key} ->
                    Acc2 = [?GETB(Key, Ctx0, Escaped) | Acc1],
                    compile(Back, State1, Acc2);
                {section, Inverted, Key} ->
                    Next = Depth + 1,
                    InnerState = State1#state{
                                   depth = Next,
                                   path  = [Key | Path],
                                   ctx   = var("C", Next)
                                  },
                    {Inner, End, Rest1, Ix2} = compile(Back, InnerState, []),
                    Size = End - Ix1,
                    State2 = State1#state{ix = Ix2},
                    Section = case Inverted of
                                  false -> compile_section(Key, Depth, Inner, Ix1, Size);
                                  true  -> compile_inverted_section(Key, Depth, Inner)
                              end,
                    compile(Rest1, State2, [Section | Acc1]);
                {end_section, Key} when hd(Path) =:= Key ->
                    {?LIST(lists:reverse(Acc1)), Ix0 + FrontLength, Back, Ix1};
                {delimiters, Left, Right} ->
                    State2 = State1#state{re = compile_regexp(Left, Right)},
                    compile(Back, State2, Acc1);
                {comment, _Comment} ->
                    compile(Back, State1, Acc1)
            end
    end.

compile_section(Key, Depth, Inner0, Start, Size) ->
    Var = var("V", Depth),
    {Ctx1, Ctx0} = {var("C", Depth + 1), var("C", Depth)},
    Match = erl_syntax:match_expr(Ctx1, erl_syntax:list([Var], Ctx0)),
    Inner1 = erl_syntax:block_expr([Match, Inner0]),
    Val = var("Val", Depth),
    List = var("VL", Depth),
    Binary = erl_syntax:application(
               erl_syntax:atom(binary_part),
               [erl_syntax:variable('Template'),
                erl_syntax:integer(Start),
                erl_syntax:integer(Size)]
              ),
    Function = erl_syntax:application(
                 erl_syntax:atom(?MODULE),
                 erl_syntax:atom(render),
                 [
                  erl_syntax:application(
                    none,
                    erl_syntax:atom(apply),
                    [Var, erl_syntax:list([Binary])]
                   ),
                  erl_syntax:application(none, erl_syntax:atom(hd), [Ctx0])
                 ]
                ),
    InnerCase = erl_syntax:case_expr(
                  Val,
                  [
                   erl_syntax:clause([erl_syntax:underscore()], false_guard(Val), [erl_syntax:nil()]),
                   erl_syntax:clause([List],
                                     [erl_syntax:application(none, erl_syntax:atom(is_list), [List])],
                                     [erl_syntax:list_comp(Inner1, [erl_syntax:generator(Var, List)])]),
                   erl_syntax:clause([Var],
                                     [erl_syntax:application(none, erl_syntax:atom(is_function), [Var, erl_syntax:integer(1)])],
                                     [Function]),
                   erl_syntax:clause([Var], [], [Inner1])
                  ]
                 ),
    erl_syntax:case_expr(
      ?GET(Key, Ctx0),
      [
       erl_syntax:clause([erl_syntax:atom(undefined)], [], [erl_syntax:nil()]),
       erl_syntax:clause([erl_syntax:tuple([erl_syntax:atom(ok), Val])], [], [InnerCase])
      ]
     ).

compile_inverted_section(Key, Depth, Inner0) ->
    {Ctx1, Ctx0} = {var("C", Depth + 1), var("C", Depth)},
    Val = var("Val", Depth),
    Inner1 = [erl_syntax:match_expr(Ctx1, Ctx0), Inner0],
    erl_syntax:case_expr(
      ?GET(Key, Ctx0),
      [
       erl_syntax:clause([erl_syntax:atom(undefined)], [], Inner1),
       erl_syntax:clause([erl_syntax:tuple([erl_syntax:atom(ok), Val])], false_guard(Val), Inner1),
       erl_syntax:clause([erl_syntax:underscore()], [], [erl_syntax:nil()])
      ]
     ).

false_guard(Var) ->
    ExactlyEqual = erl_syntax:operator('=:='),
    erl_syntax:disjunction([erl_syntax:infix_expr(Var, ExactlyEqual, erl_syntax:atom(false)),
                            erl_syntax:infix_expr(Var, ExactlyEqual, erl_syntax:nil())]).

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
    Tag = erlang:binary_part(Template, TagStart, TagSize),
    BackStart = Start + Size,
    BackSize = byte_size(Template) - BackStart,
    Back = erlang:binary_part(Template, BackStart, BackSize),
    Token = token(Type, Tag),
    {Start, Token, Back, BackStart}.

token(none, Tag) -> {tag, true, parse_key(Tag)};
token($#, Tag) -> {section, false, parse_key(Tag)};
token($^, Tag) -> {section, true, parse_key(Tag)};
token($/, Tag) -> {end_section, parse_key(Tag)};
token(${, Tag) -> {tag, false, parse_key(Tag)};
token($&, Tag) -> {tag, false, parse_key(Tag)};
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
    {ok, CompiledTagRE} = re:compile(TagRE, [unicode, dotall]),
    CompiledTagRE.

tag_start_end(Left, Right) ->
    LeftStr = unicode:characters_to_list(Left),
    RightStr = unicode:characters_to_list(Right),
    {escape_string(LeftStr), escape_string(RightStr)}.

escape_string(String) -> lists:map(fun (Char) -> [$\\, Char] end, String).
