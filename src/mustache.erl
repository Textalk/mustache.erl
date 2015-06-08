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
-export([render/2, render/3, compile/1, compile/2, get/2, to_binary/2]).

-include("mustache.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {
          path       = []              :: [[binary()]],
          list_index = 1               :: pos_integer(),
          depth      = 0               :: non_neg_integer(),
          ctx        = ?VAR("C", 0, 1) :: erl_syntax:synaxTree(),
          re                           :: re:mp(),
          delimiters                   :: {binary(), binary()},
          ix         = 0               :: non_neg_integer()
         }).

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
    end;
get_path(_Path, _Value) ->
    undefined.

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
escape([$" | Rest], Acc) -> escape(Rest, [<<"&quot;">> | Acc]);
escape([C  | Rest], Acc) -> escape(Rest, [C | Acc]).

render(Template, Ctx) when is_binary(Template), is_map(Ctx) ->
    Render = compile(Template),
    Render([Ctx]);
render(Template, Ctxs) when is_list(Ctxs) ->
    Render = compile(Template),
    Render(Ctxs);
render(Render, Ctx) when is_function(Render, 1), is_map(Ctx) ->
    Render([Ctx]);
render(Render, Ctxs) when is_function(Render, 1), is_list(Ctxs) ->
    Render(Ctxs).

render(Template, Delimiters, Ctx) when is_map(Ctx) ->
    render(Template, Delimiters, [Ctx]);
render(Template, Delimiters, Ctxs) when is_binary(Template), is_list(Ctxs) ->
    Render = compile(Template, Delimiters),
    Render(Ctxs).

compile(Template) when is_list(Template) ->
    compile(unicode:characters_to_binary(Template));
compile(Template) when is_binary(Template) ->
    compile(Template, {<<"{{">>, <<"}}">>}).

compile(Template, {Left, Right} = Delimiters)
  when is_binary(Template), is_binary(Left), is_binary(Right) ->
    State = set_delimiters(Delimiters, #state{}),
    Body = ?APP(erlang, iolist_to_binary, do_compile(Template, State, [])),
    FunST = ?FUN([?CLAUSE([State#state.ctx], [], [Body])]),
    FunExpr = erl_syntax:revert(FunST),
    Bindings0 = erl_eval:new_bindings(),
    Bindings1 = erl_eval:add_binding('T', Template, Bindings0),
    {value, Render, _Bindings} = erl_eval:expr(FunExpr, Bindings1),
    Render.

do_compile(<<>>, #state{path = []}, Acc) ->
  [?LIST(lists:reverse(Acc))];
do_compile(Rest, State0, Acc0) ->
    #state{ix = Ix0, re = Re0, ctx = Ctx0, path = Path, depth = Depth,
           list_index = ListIx} = State0,
    case re:run(Rest, Re0) of
        nomatch ->
            Size = byte_size(Rest),
            State1 = State0#state{ix = Ix0 + Size},
            do_compile(<<>>, State1, [?BIN(Ix0, Size) | Acc0]);
        {match, Match} ->
            {PrefixLength, Tag, Back, Total} = split(Rest, Match),
            #state{ix = Ix1} = State1 = State0#state{ix = Ix0 + Total},
            Acc1 = case PrefixLength of
                       0 -> Acc0;
                       _ -> [?BIN(Ix0, PrefixLength) | Acc0]
                   end,
            case Tag of
                {tag, Escaped, Key} ->
                    Acc2 = [?TOBIN(?GET(Key, Ctx0), Escaped) | Acc1],
                    do_compile(Back, State1, Acc2);
                {section, Inverted, Key} ->
                    Next = Depth + 1,
                    InnerState = State1#state{
                                   depth      = Next,
                                   path       = [Key | Path],
                                   ctx        = ?VAR("C", Next, ListIx),
                                   list_index = 1
                                  },
                    {Inner, End, Rest1, Ix2} = do_compile(Back, InnerState, []),
                    Size = End - Ix1,
                    State2 = State1#state{ix = Ix2, list_index = ListIx + 1},
                    Section = case Inverted of
                                  false -> compile_section(Key, Inner, ?BIN(Ix1, Size), State0);
                                  true  -> compile_inverted_section(Key, Inner, State0)
                              end,
                    do_compile(Rest1, State2, [Section | Acc1]);
                {end_section, Key} when hd(Path) =:= Key ->
                    {?LIST(lists:reverse(Acc1)), Ix0 + PrefixLength, Back, Ix1};
                {delimiters, Left, Right} ->
                    State2 = set_delimiters({Left, Right}, State1),
                    do_compile(Back, State2, Acc1);
                {comment, _Comment} ->
                    do_compile(Back, State1, Acc1)
            end
    end.

compile_section(Key, Inner0, Binary, State) ->
    #state{ctx = Ctx0, depth = Depth, list_index = ListIx, delimiters = Delimiters} = State,
    {Var, Val} = {?VAR("V", Depth, ListIx), ?VAR("Val", Depth, ListIx)},
    Ctx1 = ?VAR("C", Depth + 1, ListIx),
    Inner1 = ?BLOCK([?MATCH(Ctx1, ?LIST([Var], Ctx0)), Inner0]),
    RenderArgs = [?APP(apply, [Var, ?LIST([Binary])]), ?ABS(Delimiters), Ctx0],
    Function = ?APP(?MODULE, render, RenderArgs),
    Clauses = [
               ?CLAUSE([?WILD], false_guard(Val), [?NIL]),
               ?CLAUSE(
                  [?WILD],
                  [?APP(is_list, [Val])],
                  [?LISTCOMP(Inner1, [?GEN(Var, Val)])]
                 ),
               ?CLAUSE([Var], [?APP(is_function, [Var, ?INT(1)])], [Function]),
               ?CLAUSE([Var], [], [Inner1])
              ],
    ?CASE(?GET(Key, Ctx0),
      [
       ?CLAUSE([?ATOM(undefined)], [], [?NIL]),
       ?CLAUSE([?TUPLE([?ATOM(ok), Val])], [], [?CASE(Val, Clauses)])
      ]
     ).

compile_inverted_section(Key, Inner0, State) ->
    #state{ctx = Ctx0, depth = Depth, list_index = ListIx} = State,
    Ctx1 = ?VAR("C", Depth + 1, ListIx),
    Val = ?VAR("Val", Depth, ListIx),
    Inner1 = [?MATCH(Ctx1, Ctx0), Inner0],
    ?CASE(
      ?GET(Key, Ctx0),
      [
       ?CLAUSE([?ATOM(undefined)], [], Inner1),
       ?CLAUSE([?TUPLE([?ATOM(ok), Val])], false_guard(Val), Inner1),
       ?CLAUSE([?WILD], [], [?NIL])
      ]
     ).

false_guard(Var) -> ?DISJUNCTION([?EQUAL(Var, ?ATOM(false)), ?EQUAL(Var, ?NIL)]).

split(Template, Match) -> split_template(Template, fix_match(Match)).

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

parse_key(Tag) ->
    Segments = re:split(Tag, <<"\\.">>, [trim, unicode]),
    ?LIST(lists:map(fun erl_syntax:abstract/1, Segments)).

matching(${, $}) -> true;
matching($=, $=) -> true;
matching(_,  _)  -> false.

set_delimiters({Left, Right} = Delimiters, State) ->
    State#state{
      re         = compile_regexp(Left, Right),
      delimiters = Delimiters
     }.

compile_regexp(Left0, Right0) ->
    {Left1, Right1} = tag_start_end(Left0, Right0),
    TagRE = ["(", Left1, "({|=)(.+?)(}|=)", Right1, ")"
             "|"
             "(", Left1, "(#|\\^|!|&|/)?\\s*(.+?)", Right1, ")"],
    {ok, CompiledTagRE} = re:compile(TagRE, [unicode, dotall]),
    CompiledTagRE.

tag_start_end(Left, Right) ->
    LeftStr = unicode:characters_to_list(Left),
    RightStr = unicode:characters_to_list(Right),
    {escape_string(LeftStr), escape_string(RightStr)}.

escape_string(String) -> lists:map(fun (Char) -> [$\\, Char] end, String).
