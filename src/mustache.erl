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

-module(mustache).  %% v0.1.0
-author("Tom Preston-Werner").
-export([compile/1, compile/2, render/1, render/2, render/3, get/2, get/3, escape/1, start/1]).

-record(mstate, {
          depth = 0,
          mod = undefined,
          section_re = undefined,
          tag_re = undefined
         }).

-include_lib("eunit/include/eunit.hrl").

-define(MUSTACHE_CTX, mustache_ctx).
-define(MUSTACHE_CTX_STR, "mustache_ctx").
-define(MUSTACHE_STR, "mustache").

compile(Body) when is_binary(Body) ->
  State = #mstate{},
  CompiledTemplate = pre_compile(Body, State),
  %io:format("~p~n~n", [CompiledTemplate]),
  {ok, Tokens, _} = erl_scan:string(CompiledTemplate),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  Bindings = erl_eval:new_bindings(),
  {value, Fun, _} = erl_eval:expr(Form, Bindings),
  Fun;
compile(Body) when is_list(Body) ->
    compile(unicode:characters_to_binary(Body));
compile(Mod) when is_atom(Mod) ->
  TemplatePath = template_path(Mod),
  compile(Mod, TemplatePath).

compile(Mod, File) ->
  code:purge(Mod),
  {module, _} = code:load_file(Mod),
  {ok, TemplateBin} = file:read_file(File),
  Template = re:replace(TemplateBin, "\"", "\\\\\"", [global, {return,list}]),
  State = #mstate{mod = Mod},
  CompiledTemplate = pre_compile(Template, State),
  % io:format("~p~n~n", [CompiledTemplate]),
  % io:format(CompiledTemplate ++ "~n", []),
  {ok, Tokens, _} = erl_scan:string(CompiledTemplate),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  Bindings = erl_eval:new_bindings(),
  {value, Fun, _} = erl_eval:expr(Form, Bindings),
  Fun.

render(Mod) ->
  TemplatePath = template_path(Mod),
  render(Mod, TemplatePath).

render(Body, Ctx) when is_list(Body) ->
  TFun = compile(Body),
  render(undefined, TFun, Ctx);
render(Body, Ctx) when is_binary(Body) ->
    TFun = compile(Body),
    unicode:characters_to_binary(render(undefined, TFun, Ctx));
render(Mod, File) when is_list(File) ->
  render(Mod, File, []);
render(CompiledTemplate, Ctx) when is_function(CompiledTemplate, 1) ->
    render(undefined, CompiledTemplate, Ctx);
render(Mod, CompiledTemplate) ->
  render(Mod, CompiledTemplate, []).

render(Mod, File, Ctx) when is_list(File) ->
  CompiledTemplate = compile(Mod, File),
  render(Mod, CompiledTemplate, Ctx);
render(Mod, CompiledTemplate, CtxData) ->
  Ctx0 = ?MUSTACHE_CTX:new(CtxData),
  Ctx1 = ?MUSTACHE_CTX:module(Mod, Ctx0),
  lists:flatten(CompiledTemplate(Ctx1)).

pre_compile(Template, State0) ->
  State1 = set_delimiters(<<"{{ }}">>, State0),
  unicode:characters_to_list([<<"fun(Ctx) -> ">>, compiler(Template, State1), <<" end.">>]).

compiler(Template, State) ->
  Res = re:run(Template, State#mstate.section_re),
  case Res of
    {match, [{M0, M1}, {K0, K1}, {N0, N1}, {C0, C1}]} ->
      Front = erlang:binary_part(Template, 0, M0),
      End = M0 + M1,
      Back = erlang:binary_part(Template, End, byte_size(Template) - End),
      Kind = erlang:binary_part(Template, K0, K1),
      Name = erlang:binary_part(Template, N0, N1),
      Content = erlang:binary_part(Template, C0, C1),
      [<<"[">>, compile_tags(Front, State),
       <<" | [">>, compile_section(Kind, Name, Content, State),
       <<" | [">>, compiler(Back, State), <<"]]]">>];
    nomatch ->
      compile_tags(Template, State)
  end.

compile_section(<<"#">>, Name, Content, State0) ->
  #mstate{mod = Mod, depth = Depth} = State0,
  ModBin = atom_to_binary(Mod, utf8),
  KeyBin = compile_dot_notation(Name),
  State1 = State0#mstate{depth = Depth + 1},
  Result = compiler(Content, State1),
  Var = ["Var", integer_to_binary(Depth)],
  [<<"fun() -> "
         "case mustache:get(">>, KeyBin, <<", Ctx, ">>, ModBin,  <<") of "
             "<<\"true\">> -> ">>, Result, <<"; "
             "<<\"false\">> -> []; ">>,
              Var, <<" when is_binary(">>, Var, <<") -> ">>,
                 Var, <<";">>,
              Var, <<" when is_list(">>, Var, <<") -> "
                 "[fun(Ctx) -> ">>, Result, <<" end(mustache_ctx:merge(SubCtx, Ctx)) || SubCtx <- ">>, Var, <<"]; ">>,
              Var, <<" when is_map(">>, Var, <<") -> "
                 "fun(Ctx) -> ">>, Result, <<" end(">>, Var, <<");">>,
              Var, <<" when is_function(">>, Var ,<<", 1) -> ">>,
                  Var, <<"(">>, Result, <<");">>,
              Var, <<" -> ">>,
               <<"throw({template, io_lib:format(\"Bad context for ~p: ~p\", [\"">>, Name, <<"\", ">>, Var, <<"])}) "
    "end "
  "end()">>];
compile_section(<<"^">>, Name, Content, State) ->
  ModBin = atom_to_binary(State#mstate.mod, utf8),
  KeyBin = compile_dot_notation(Name),
  Result = compiler(Content, State),
  [<<"fun() -> "
         "case mustache:get(">>, KeyBin, <<", Ctx, ">>, ModBin, <<") of "
             "<<\"false\">> -> ">>, Result, <<"; "
             "[] -> ">>, Result, <<"; "
             "_ -> [] "
         "end "
     "end()">>].

compile_tags(Template, State0) ->
  Res = re:run(Template, State0#mstate.tag_re),
  case Res of
    {match, [{M0, M1}, K, {C0, C1} | _Rest]} ->
      Front = erlang:binary_part(Template, 0, M0),
      End = M0 + M1,
      Back = erlang:binary_part(Template, End, byte_size(Template) - End),
      Content = erlang:binary_part(Template, C0, C1),
      Kind = tag_kind(Template, K),
      {Result, State1} = compile_tag(Kind, Content, State0),
      Rest = case Kind of
                 <<"=">> -> compiler(Back, State1);
                 _ -> compile_tags(Back, State1)
             end,
      [<<"[<<\"">>, escape_special(Front), <<"\">>,">>, Result, <<",">>, Rest, <<"]">>];
    nomatch ->
      [<<"<<\"">>, escape_special(Template), <<"\">>">>]
  end.

tag_kind(_Template, {-1, 0}) ->
  none;
tag_kind(Template, {K0, K1}) ->
    erlang:binary_part(Template, K0, K1).

compile_tag(none, Content, State) ->
  {compile_escaped_tag(Content, State), State};
compile_tag(<<"&">>, Content, State) ->
  {compile_unescaped_tag(Content, State), State};
compile_tag(<<"{">>, Content, State) ->
  {compile_unescaped_tag(Content, State), State};
compile_tag(<<"!">>, _Content, State) ->
  {<<"[]">>, State};
compile_tag(<<"=">>, Content, State) ->
  {<<"[]">>, set_delimiters(Content, State)}.

compile_escaped_tag(Content, State) ->
  ModBin = atom_to_binary(State#mstate.mod, utf8),
  KeyBin = compile_dot_notation(Content),
  [<<"mustache:escape(mustache:get(">>, KeyBin, <<", Ctx, ">>, ModBin, <<"))">>].

compile_unescaped_tag(Content, State) ->
  ModBin = atom_to_binary(State#mstate.mod, utf8),
  KeyBin = compile_dot_notation(Content),
  [<<"mustache:get(">>, KeyBin, <<", Ctx, ">>, ModBin, <<")">>].

compile_dot_notation(<<".">>) -> <<"[]">>;
compile_dot_notation(Content) ->
    Parts = re:split(Content, <<"\\.">>, [unicode]),
    [$[, join(Parts), $]].

join([]) -> [];
join(List) -> join(List, <<>>).

join([Last], Acc) -> [Acc, Last];
join([Elem | Rest], Acc) -> join(Rest, [Acc, Elem, $,]).

template_dir(Mod) ->
  DefaultDirPath = filename:dirname(code:which(Mod)),
  case application:get_env(mustache, templates_dir) of
    {ok, DirPath} when is_list(DirPath) ->
      case filelib:ensure_dir(DirPath) of
        ok -> DirPath;
        _  -> DefaultDirPath
      end;
    _ ->
      DefaultDirPath
  end.
template_path(Mod) ->
  DirPath = template_dir(Mod),
  Basename = atom_to_list(Mod),
  filename:join(DirPath, Basename ++ ".mustache").

get(Key, Ctx, Mod) ->
  get(Key, mustache_ctx:module(Mod, Ctx)).

get(Key, Ctx) ->
  case mustache_ctx:get(Key, Ctx) of
    {ok, Value} -> to_bin(Value);
    {error, _} -> []
  end.

to_bin(Value) when is_integer(Value) -> integer_to_binary(Value);
to_bin(Value) when is_float(Value) -> float_to_binary(Value, [{decimals, 2}]);
to_bin(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
to_bin(Value) -> Value.

escape(Binary) when is_binary(Binary) ->
  escape(unicode:characters_to_list(Binary), <<>>);
escape(Term) ->
    Term.

escape([], Acc) ->
  Acc;
escape([$< | Rest], Acc) ->
  escape(Rest, <<Acc/binary, "&lt;">>);
escape([$> | Rest], Acc) ->
  escape(Rest, <<Acc/binary, "&gt;">>);
escape([$& | Rest], Acc) ->
  escape(Rest, <<Acc/binary, "&amp;">>);
escape([X | Rest], Acc) ->
  escape(Rest, <<Acc/binary, X>>).

escape_special(Binary) ->
    [escape_char(Char) || Char <- unicode:characters_to_list(Binary)].

escape_char($\0) -> <<"\\0">>;
escape_char($\n) -> <<"\\n">>;
escape_char($\t) -> <<"\\t">>;
escape_char($\b) -> <<"\\b">>;
escape_char($\r) -> <<"\\r">>;
escape_char($')  -> <<"\\'">>;
escape_char($")  -> <<"\\\"">>;
escape_char($\\) -> <<"\\\\">>;
escape_char(Char) -> Char.

set_delimiters(Content, State) ->
    [Left, Right] = re:split(Content, <<" ">>, [{return, list}]),
    {ok, CompiledSectionRE} = section_re(Left, Right),
    {ok, CompiledTagRE} = tag_re(Left, Right),
    State#mstate{section_re = CompiledSectionRE, tag_re = CompiledTagRE}.

section_re(Left0, Right0) ->
    {Left1, Right1} = {escape_delimiter(Left0), escape_delimiter(Right0)},
    Stop = ["([^", hd(Right0), "]*)"],
    Regexp = [Left1, "(#|\\^)", Stop, Right1, "\\s*(.+?)", Left1, "/\\2", Right1],
    re:compile(Regexp, [dotall, unicode]).

tag_re(Left0, Right0) ->
    {Left1, Right1} = {escape_delimiter(Left0), escape_delimiter(Right0)},
    Regexp = [Left1, "(#|=|!|>|{|&)?(.+?)(=|})?", Right1],
    re:compile(Regexp, [dotall, unicode]).

escape_delimiter(Delimiter) ->
    unicode:characters_to_binary(lists:map(fun (Char) -> [$\\, Char] end, Delimiter)).

%%---------------------------------------------------------------------------

start([T]) ->
  Out = render(list_to_atom(T)),
  io:format(Out ++ "~n", []).
