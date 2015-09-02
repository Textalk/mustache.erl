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

-define(MERL_NO_TRANSFORM, true).

-ifdef(HAS_MERL).
-include_lib("syntax_tools/include/merl.hrl").
-else.
-include_lib("merl/include/merl.hrl").
-endif.

-record(state, {
          path = []              :: [[binary()]],
          context                :: erl_syntax:synaxTree(),
          variables = sets:new() :: sets:set(atom()),
          regex                  :: re:mp(),
          delimiters             :: {binary(), binary()},
          index = 0              :: non_neg_integer()
         }).

%% @doc Get a value by key from given contexts.
%% The Key is a list of binaries. If the empty path is given
%% then this corresponds to the current context or {{.}} in mustache.
%% Otherwise the key is a path into a context. The tag {{apa.bepa}}
%% will be mapped to the key [<<"apa">>, <<"bepa">>].
get([], [Item | _Contexts]) -> {ok, Item};
get(Key, Contexts)          -> context_get(Key, Contexts).

%% @doc Get the first value from the given contexts
context_get(_Path, []) ->
    undefined;
context_get(Path, [Context | Contexts]) ->
    case get_path(Path, Context) of
        {ok, Value} -> {ok, Value};
        undefined   -> context_get(Path, Contexts)
    end.

%% @doc Get a value by a path of keys
get_path([], Value) ->
    {ok, Value};
get_path([Key | Keys], Map) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> get_path(Keys, Value);
        error       -> undefined
    end;
get_path(_Path, _Value) ->
    undefined.

%% @doc Helper function for turning a result from get/2 into a binary.
%% The second parameter specifies if the resulting binary should be escaped.
to_binary({ok, Value}, true)   -> escape(to_binary(Value));
to_binary({ok, Value}, false)  -> to_binary(Value);
to_binary(undefined, _Escaped) -> <<>>.

%% @doc Turn an erlang term into a binary
to_binary(Value) when is_integer(Value) -> integer_to_binary(Value);
to_binary(Value) when is_float(Value)   -> float_to_binary(Value, [{decimals, 2}]);
to_binary(Value) when is_atom(Value)    -> atom_to_binary(Value, utf8);
to_binary(Value) when is_binary(Value)  -> Value.

%% @doc Escape a string or binary
escape(Binary) when is_binary(Binary) -> escape(unicode:characters_to_list(Binary));
escape(String) when is_list(String)   -> escape(String, []).

%% @doc Escape a string
escape([], Acc)          -> unicode:characters_to_binary(lists:reverse(Acc));
escape([$< | Rest], Acc) -> escape(Rest, [<<"&lt;">> | Acc]);
escape([$> | Rest], Acc) -> escape(Rest, [<<"&gt;">> | Acc]);
escape([$& | Rest], Acc) -> escape(Rest, [<<"&amp;">> | Acc]);
escape([$" | Rest], Acc) -> escape(Rest, [<<"&quot;">> | Acc]);
escape([C  | Rest], Acc) -> escape(Rest, [C | Acc]).

%% @doc Render a template using given context.
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

%% @doc Render a template using given delimiters and context.
render(Template, Delimiters, Ctx) when is_map(Ctx) ->
    render(Template, Delimiters, [Ctx]);
render(Template, Delimiters, Ctxs) when is_binary(Template), is_list(Ctxs) ->
    Render = compile(Template, Delimiters),
    Render(Ctxs).

%% @doc Compile a template to a function.
compile(Template) when is_list(Template) ->
    compile(unicode:characters_to_binary(Template));
compile(Template) when is_binary(Template) ->
    compile(Template, {<<"{{">>, <<"}}">>}).

%% @doc Compile a template to a function using given delimiters.
compile(Template, {Left, Right} = Delimiters)
  when is_binary(Template), is_binary(Left), is_binary(Right) ->
    State0 = set_delimiters(Delimiters, #state{}),
    {Context, State1} = new_variable(State0),
    State2 = State1#state{context = Context},
    FunTree = ?Q(
                "fun (_@context) ->\n"
                "   iolist_to_binary(_@body)\n"
                "end",
                [{context, merl:var(Context)},
                 {body, do_compile(Template, State2, [])}]
               ),
    FunExpr = erl_syntax:revert(FunTree),
    Bindings = erl_eval:add_binding('Template', Template, erl_eval:new_bindings()),
    {value, Render, _Bindings} = erl_eval:expr(FunExpr, Bindings),
    Render.

do_compile(<<>>, #state{path = []}, Acc) ->
  [?Q("[_@list]", [{list, lists:reverse(Acc)}])];
do_compile(Template, State0, Acc0) ->
    #state{index = Index0, regex = Regex0, context = Context} = State0,
    case re:run(Template, Regex0) of
        %% No more tags in the template, just append the last binary
        nomatch ->
            Size = byte_size(Template),
            State1 = State0#state{index = Index0 + Size},
            do_compile(<<>>, State1, [template_part(Index0, Size) | Acc0]);
        %% Otherwise there are tags left
        {match, Match} ->
            %% We then split the match we got and parse it to check what tag we got
            {PrefixLength, Tag, Back, Total} = split(Template, Match),

            %% First we may have a prefix binary before the tag, this we just
            %% add to the accumulator
            Acc1 = case PrefixLength of
                       0 -> Acc0;
                       _ -> [template_part(Index0, PrefixLength) | Acc0]
                   end,

            %% Then depending on what tag we found we take appropriate action
            State1 = State0#state{index = Index0 + Total},
            case Tag of
                %% If it's a simple tag thats just compiled into a to_binary-wrapped get call
                {tag, Escaped, Key} ->
                    Get = ?Q(
                             "mustache:to_binary(\n"
                             "    mustache:get(_@key, _@context),\n"
                             "    _@escaped\n"
                             ")",
                             [{key, merl:term(Key)},
                              {context, merl:var(Context)},
                              {escaped, merl:term(Escaped)}]
                            ),
                    do_compile(Back, State1, [Get | Acc1]);
                %% If it's the start of a section then we compile the inner part of the section
                %% before we compile the section itself
                {section, Inverted, Key}  ->
                    {Section, Rest, State2} = case Inverted of
                                                      true  -> compile_inverted_section(Key, Back, State1);
                                                      false -> compile_section(Key, Back, State1)
                                                  end,
                    do_compile(Rest, State2, [Section | Acc1]);
                %% If we encounter a section terminating tag then we just return the accumulator
                {end_section, Key} when hd(State0#state.path) =:= Key ->
                    SectionEnd = Index0 + PrefixLength,
                    {?Q("[_@list]", [{list, lists:reverse(Acc1)}]), SectionEnd, Back, State1};
                %% If we encounter a switch in delimiters then we need to parse the rest of the template
                %% using them
                {delimiters, Left, Right} ->
                    do_compile(Back, set_delimiters({Left, Right}, State1), Acc1);
                %% Otherwise it's a comment which we just throw away
                {comment, _Comment} ->
                    do_compile(Back, State1, Acc1)
            end
    end.

%% @doc Compile a section.
%% We are given a compiled inner part and we need to construct the case-expressions to decide what
%% will happen in the different cases of which contexts
compile_section(Key, Template, State0) ->
    #state{context = Context, delimiters = Delimiters} = State0,
    {Value, State1} = new_variable(State0),
    {Variable, State2} = new_variable(State1),
    NextContextExpr = ?Q(
                         "[_@variable | _@context]",
                         [{variable, merl:var(Variable)}, {context, merl:var(Context)}]
                        ),
    {Inner, SectionPos, Rest, State3} = compile_inner_section(Key, NextContextExpr, Template, State2),
    {SectionStart, SectionSize} = SectionPos,
    Section = ?Q(
                 "case mustache:get(_@key, _@context) of\n"
                 "    undefined ->\n"
                 "        <<>>;\n"
                 "    {ok, _@value} ->\n"
                 "        case _@value of\n"
                 "            false ->\n"
                 "                <<>>;\n"
                 "            _ when is_list(_@value) ->\n"
                 "                [_@inner || _@variable <- _@value];\n"
                 "            _ when is_function(_@value, 1) ->\n"
                 "                mustache:render(apply(_@value, [_@binary]), _@delimiters, _@context);\n"
                 "            _@variable ->\n"
                 "                _@inner\n"
                 "        end\n"
                 "end",
                 [{key, merl:term(Key)},
                  {context, merl:var(Context)},
                  {value, merl:var(Value)},
                  {variable, merl:var(Variable)},
                  {binary, template_part(SectionStart, SectionSize)},
                  {delimiters, merl:term(Delimiters)},
                  {inner, Inner}]
                ),
    {Section, Rest, State3}.

%% @doc Compile an inverted section.
%% The inner compiled section will only be included if the value does not exist or
%% if the value is falsy.
%% Falsy values are: false, <<>> and [].
compile_inverted_section(Key, Template, #state{context = Context} = State0) ->
    NextContextExpr = merl:var(Context),
    {Inner, _SectionPos, Rest, State1} = compile_inner_section(Key, NextContextExpr, Template, State0),
    Section = ?Q(
                 "case mustache:get(_@key, _@context) of\n"
                 "    undefined   -> _@inner;\n"
                 "    {ok, false} -> _@inner;\n"
                 "    {ok, <<>>}  -> _@inner;\n"
                 "    {ok, []}    -> _@inner;\n"
                 "    _           -> <<>>\n"
                 "end",
                 [{key, merl:term(Key)},
                  {context, merl:var(Context)},
                  {inner, Inner}]
                ),
    {Section, Rest, State1}.

%% @doc Compile the inner part of a section.
%% We pass as an argument what expression the next context should be set to, this is needed to
%% pass information down from the outer part of the section.
compile_inner_section(Key, NextContextExpr, Template, State0) ->
    #state{context = Context, index = SectionStart, path = Path} = State0,
    {NextContext, State1} = new_variable(State0),
    State2 = State1#state{
               path    = [Key | Path],
               context = NextContext
              },
    {Inner0, SectionEnd, Rest, State3} = do_compile(Template, State2, []),
    SectionSize = SectionEnd - SectionStart,
    Inner1 = ?Q(
               "begin\n"
               "   _@next_context = _@next_context_expr,\n"
               "   _@inner\n"
               "end",
               [{next_context, merl:var(NextContext)},
                {next_context_expr, NextContextExpr},
                {inner, Inner0}]
              ),
    {Inner1, {SectionStart, SectionSize}, Rest, State3#state{context = Context, path = Path}}.

%% @doc Get a variable name
new_variable(#state{variables = Variables} = State) ->
    Variable = erl_syntax_lib:new_variable_name(Variables),
    {Variable, State#state{variables = sets:add_element(Variable, Variables)}}.

%% @doc Create a reference to the original template
template_part(Index, Size) ->
    ?Q(
       "erlang:binary_part(Template, _@index, _@size)",
       [{index, merl:term(Index)}, {size, merl:term(Size)}]
      ).

%% @doc Given a tag match, split the template into parts
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

%% @doc Create a token depending on the tag modifier
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

%% @doc Parse a key.
%% - Strip the binary from prefix and suffix whitespace
%% - Split the key on dots
%% - Construct the AST for the key
parse_key(Tag0) ->
    {match, [Tag1]} = re:run(Tag0, <<"^\\s*(\\S+)\\s*$">>, [{capture, all_but_first, binary}]),
    re:split(Tag1, <<"\\.">>, [trim, unicode]).

%% @doc Function to assert that modifiers match
matching(${, $}) -> true;
matching($=, $=) -> true;
matching(_,  _)  -> false.

%% @doc Change the delimiters in the state
set_delimiters({Left, Right} = Delimiters, State) ->
    State#state{
      regex      = compile_regexp(Left, Right),
      delimiters = Delimiters
     }.

%% @doc Compile a new regexp for the given delimiters
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
