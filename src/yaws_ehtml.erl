
-module(yaws_ehtml).

-export([ehtml_expand/1]).
-export_type([ehtml/0,tag/0,attr/0,key/0,value/0,body/0]).

-opaque ehtml() :: [ehtml()] | {tag(),attr(),body()} | {tag(),attr()} | {tag()} | {Module :: atom(),Fun :: atom(),[Args :: term()]} | fun() | binary() | char().
-opaque tag() :: atom().
-opaque attr() :: [{key(),value()}].
-opaque key() :: atom().
-opaque value() :: string() | binary() | atom() | integer() | float() | {Module :: atom(),Fun :: atom(),[Args :: term()]} | fun().
-opaque body() :: ehtml().

%% ------------------------------------------------------------
%% simple erlang term representation of HTML:
%% EHTML = [EHTML] | {Tag, Attrs, Body} | {Tag, Attrs} | {Tag} |
%%         {Module, Fun, [Args]} | fun/0 |
%%         binary() | character()
%% Tag   = atom()
%% Attrs = [{Key, Value}]
%% Key   = atom()
%% Value = string() | binary() | atom() | integer() | float() |
%%         {Module, Fun, [Args]} | fun/0
%% Body  = EHTML

-spec ehtml_expand(ehtml()) -> iolist().
ehtml_expand(Ch) when Ch >= 0, Ch =< 255 -> Ch; %yaws_api:htmlize_char(Ch);
ehtml_expand(Bin) when is_binary(Bin) -> Bin; % yaws_api:htmlize(Bin);


%%!todo (low priority) - investigate whether tail-recursion would be of any
%% benefit here instead of the current ehtml_expand(Body) recursion.
%%                - provide a tail_recursive version & add a file in the
%% benchmarks folder to measure it.
                                                %
ehtml_expand({Tag}) ->
    ["<", atom_to_list(Tag), ehtml_end_tag(Tag)];
ehtml_expand({pre_html, X}) -> X;
ehtml_expand({Mod, Fun, Args})
  when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ehtml_expand(Mod:Fun(Args));
ehtml_expand({Tag, Attrs}) ->
    NL = ehtml_nl(Tag),
    [NL, "<", atom_to_list(Tag), ehtml_attrs(Attrs), ehtml_end_tag(Tag)];
ehtml_expand({Tag, Attrs, Body}) when is_atom(Tag) ->
    Ts = atom_to_list(Tag),
    NL = ehtml_nl(Tag),
    [NL, "<", Ts, ehtml_attrs(Attrs), ">", ehtml_expand(Body), "</", Ts, ">"];
ehtml_expand([H|T]) -> [ehtml_expand(H)|ehtml_expand(T)];
ehtml_expand([]) -> [];
ehtml_expand(Fun) when is_function(Fun) ->
    ehtml_expand(Fun()).


ehtml_attrs([]) -> [];
ehtml_attrs([Attribute|Tail]) when is_atom(Attribute) ->
    [[$ |atom_to_list(Attribute)]|ehtml_attrs(Tail)];
ehtml_attrs([Attribute|Tail]) when is_list(Attribute) ->
    [" ", Attribute|ehtml_attrs(Tail)];
ehtml_attrs([{Name, {Mod, Fun, Args}} | Tail])
  when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ehtml_attrs([{Name,  Mod:Fun(Args)} | Tail]);
ehtml_attrs([{Name, Value} | Tail]) when is_function(Value) ->
    ehtml_attrs([{Name, Value()} | Tail]);
ehtml_attrs([{Name, Value} | Tail]) ->
    ValueString = [$", value2string(Value), $"],
    [[$ |atom_to_list(Name)], [$=|ValueString]|ehtml_attrs(Tail)];
ehtml_attrs([{check, Name, {Mod, Fun, Args}} | Tail])
  when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ehtml_attrs([{check, Name,  Mod:Fun(Args)} | Tail]);
ehtml_attrs([{check, Name, Value} | Tail]) when is_function(Value) ->
    ehtml_attrs([{check, Name, Value()} | Tail]);
ehtml_attrs([{check, Name, Value} | Tail]) ->
    Val = value2string(Value),
    Q = case deepmember($", Val) of
            true -> $';
            false -> $"
        end,
    ValueString = [Q,Val,Q],
    [[$ |atom_to_list(Name)], [$=|ValueString]|ehtml_attrs(Tail)].

value2string(Atom) when is_atom(Atom) -> atom_to_list(Atom);
value2string(String) when is_list(String) -> String;
value2string(Binary) when is_binary(Binary) -> Binary;
value2string(Integer) when is_integer(Integer) -> integer_to_list(Integer);
value2string(Float) when is_float(Float) -> float_to_list(Float).



%% Tags for which we must not add extra white space.
%% FIXME: should there be anything more in this list?

ehtml_nl(a) -> [];
ehtml_nl(br) -> [];
ehtml_nl(span) -> [];
ehtml_nl(em) -> [];
ehtml_nl(strong) -> [];
ehtml_nl(dfn) -> [];
ehtml_nl(code) -> [];
ehtml_nl(samp) -> [];
ehtml_nl(kbd) -> [];
ehtml_nl(var) -> [];
ehtml_nl(cite) -> [];
ehtml_nl(abbr) -> [];
ehtml_nl(acronym) -> [];
ehtml_nl(q) -> [];
ehtml_nl(sub) -> [];
ehtml_nl(sup) -> [];
ehtml_nl(ins) -> [];
ehtml_nl(del) -> [];
ehtml_nl(img) -> [];
ehtml_nl(tt) -> [];
ehtml_nl(i) -> [];
ehtml_nl(b) -> [];
ehtml_nl(big) -> [];
ehtml_nl(small) -> [];
ehtml_nl(strike) -> [];
ehtml_nl(s) -> [];
ehtml_nl(u) -> [];
ehtml_nl(font) -> [];
ehtml_nl(basefont) -> [];
ehtml_nl(input) -> [];
ehtml_nl(button) -> [];
ehtml_nl(object) -> [];
ehtml_nl(_) -> "\n".


%% Void elements must not have an end tag (</tag>) in HTML5, while for most
%% elements a proper end tag (<tag></tag>, not <tag />) is mandatory.
%%
%% http://www.w3.org/TR/html5/syntax.html#void-elements
%% http://www.w3.org/TR/html5/syntax.html#syntax-tag-omission

-define(self_closing, " />"). % slash ignored in HTML5

ehtml_end_tag(area) -> ?self_closing;
ehtml_end_tag(base) -> ?self_closing;
ehtml_end_tag(br) -> ?self_closing;
ehtml_end_tag(col) -> ?self_closing;
ehtml_end_tag(embed) -> ?self_closing;
ehtml_end_tag(hr) -> ?self_closing;
ehtml_end_tag(img) -> ?self_closing;
ehtml_end_tag(input) -> ?self_closing;
ehtml_end_tag(keygen) -> ?self_closing;
ehtml_end_tag(link) -> ?self_closing;
ehtml_end_tag(meta) -> ?self_closing;
ehtml_end_tag(param) -> ?self_closing;
ehtml_end_tag(source) -> ?self_closing;
ehtml_end_tag(track) -> ?self_closing;
ehtml_end_tag(wbr) -> ?self_closing;
ehtml_end_tag(Tag) -> ["></", atom_to_list(Tag), ">"].


%% ------------------------------------------------------------
%% ehtml_expander/1: an EHTML optimizer
%%
%% This is an optimization for generating the same EHTML multiple times with
%% only small differences, by using fast re-usable templates that contain
%% variables. The variables are atoms starting with a dollar sign, like
%% '$myvar'. There are two functions: ehtml_expander/1 to create an optimized
%% EHTML template, then ehtml_apply/2 takes a template and a dictionary of
%% variable values and generates the actual HTML.
%%
%% If you are spending a lot of time regenerating similar EHTML fragments then
%% this is for you.
%%
%% Variables can appear in three places:
%% - As a body element, where you would normally have a tag. The values of
%%   these variables are expanded as EHTML.
%% - As the name or value of an attribute. The values of these variables are
%%   strings.
%% - As the CDR of an attribute list. The values of these variables are
%%   key-value lists of more attributes.
%%
%% See ehtml_expander_test/0 for an example.
%%
%% The approach is inspired by the way that Yaws already treats .yaws files,
%% and the article ``A Hacker's Introduction To Partial Evaluation'' by Darius
%% Bacon (cool guy), http://www.lisp-p.org/htdocs/peval/peval.cgi
%%
%% (For now I flatter myself that this is some kind of partial evaluator, but
%% I don't really know :-) -luke)

ehtml_expander(X) ->
    ehtml_expander_compress(lists:flatten(ehtml_expander(X, [], [])), []).

%% Returns a deep list of text and variable references (atoms)

%% Text
ehtml_expander(Ch, Before, After) when Ch >= 0, Ch =< 255 ->
    ehtml_expander_done(htmlize_char(Ch), Before, After);
ehtml_expander(Bin, Before, After) when is_binary(Bin) ->
    ehtml_expander_done(htmlize(Bin), Before, After);


ehtml_expander({pre_html, X}, Before, After) ->
    ehtml_expander_done(X, Before, After);
%% Tags
ehtml_expander({Tag}, Before, After) ->
    ehtml_expander_done(["<", atom_to_list(Tag), ehtml_end_tag(Tag)],
                        Before, After);
ehtml_expander({Tag, Attrs}, Before, After) ->
    NL = ehtml_nl(Tag),
    ehtml_expander_done([NL, "<", atom_to_list(Tag), ehtml_attrs(Attrs),
                         ehtml_end_tag(Tag)],
                        Before,
                        After);
ehtml_expander({Tag, Attrs, Body}, Before, After) ->
    ehtml_expander(Body,
                   [["\n<", atom_to_list(Tag),
                     ehtml_attrs_expander(Attrs), ">"]|
                    Before],
                   ["</", atom_to_list(Tag), ">"|After]);
%% Variable references
ehtml_expander(Var, Before, After) when is_atom(Var) ->
    [lists:reverse(Before), {ehtml, ehtml_var_name(Var)}, After];
%% Lists
ehtml_expander([H|T], Before, After) ->
    ehtml_expander(T, [ehtml_expander(H, [], [])|Before], After);
ehtml_expander([], Before, After) ->
    ehtml_expander_done("", Before, After).

%% Expander for attributes. The attribute name and value can each be a
%% variable reference.
ehtml_attrs_expander([]) -> "";
ehtml_attrs_expander([{Var,Val}|T]) ->
    [[" ",
      ehtml_attr_part_expander(Var),
      "=",
      "\"", ehtml_attr_part_expander(Val), "\""]|
     ehtml_attrs_expander(T)];
ehtml_attrs_expander([Var|T]) ->
    [[" ",
      ehtml_attr_part_expander(Var)]|
     ehtml_attrs_expander(T)];
ehtml_attrs_expander(Var) when is_atom(Var) ->
    %% Var in the cdr of an attribute list
    [{ehtml_attrs, ehtml_var_name(Var)}].

ehtml_attr_part_expander(A) when is_atom(A) ->
    case atom_to_list(A) of
        [$$|_Rest] -> {preformatted, ehtml_var_name(A)};
        Other -> Other
    end;
ehtml_attr_part_expander(I) when is_integer(I) -> integer_to_list(I);
ehtml_attr_part_expander(S) when is_list(S) -> S.

ehtml_expander_done(X, Before, After) -> [lists:reverse([X|Before]), After].

%% Compress an EHTML expander, converting all adjacent bits of text into
%% binaries.
%% Returns: [binary() | {ehtml, Var} | {preformatted, Var}, {ehtml_attrs, Var}]
%% Var = atom()
ehtml_expander_compress([Tag|T], Acc) when is_tuple(Tag) ->
    [list_to_binary(lists:reverse(Acc)), Tag | ehtml_expander_compress(T, [])];
ehtml_expander_compress([], Acc) -> [list_to_binary(lists:reverse(Acc))];
ehtml_expander_compress([H|T], Acc) when is_integer(H) ->
    ehtml_expander_compress(T, [H|Acc]).

%% Apply an expander with the variable bindings in Env.  Env is a list of
%% {VarName, Value} tuples, where VarName is an atom and Value is an ehtml
%% term.
ehtml_apply(Expander, Env) -> [ehtml_eval(X, Env) || X <- Expander].

ehtml_eval(Bin, _Env) when is_binary(Bin) -> Bin;
ehtml_eval({Type, Var}, Env) ->
    case lists:keysearch(Var, 1, Env) of
        false -> erlang:error({ehtml_unbound, Var});
        {value, {Var, Val}} ->
            case Type of
                ehtml -> ehtml_expand(Val);
                preformatted -> Val;
                ehtml_attrs -> ehtml_attrs(Val)
            end
    end.

%% Get the name part of a variable reference.
%% e.g. ehtml_var_name('$foo') -> foo.
ehtml_var_name(A) when is_atom(A) ->
    case atom_to_list(A) of
        [$$|Rest] -> list_to_atom(Rest);
        _Other -> erlang:error({bad_ehtml_var_name, A})
    end.

ehtml_expander_test() ->
    %% Expr is a template containing variables.
    Expr = {html, [{title, '$title'}],
            {body, [],
             [{h1, [], '$heading'},
              '$text']}},
    %% Expand is an expander that can be used to quickly generate the HTML
    %% specified in Expr.
    Expand = ehtml_expander(Expr),
    %% Bs{1,2} are lists of variable bindings to fill in the gaps in the
    %% template. We can reuse the template on many sets of bindings, and this
    %% is much faster than doing a full ehtml of the whole page each time.
    Bs1 = [{title, "First page"},
           {heading, "Heading"},
           {text, {pre_html, "<b>My text!</b>"}}],
    Bs2 = [{title, "Second page"},
           {heading, "Foobar"},
           {text, {b, [], "My text again!"}}],
    %% Page1 and Page2 are generated from the template. They are I/O lists
    %% (i.e. deep lists of strings and binaries, ready to ship)
    Page1 = ehtml_apply(Expand, Bs1),
    Page2 = ehtml_apply(Expand, Bs2),
    %% We return the two pages as strings, plus the actual expander (which is
    %% an "opaque" data structure, but maybe interesting to see.)
    {binary_to_list(list_to_binary(Page1)),
     binary_to_list(list_to_binary(Page2)),
     Expand}.



%% htmlize
htmlize(Bin) when is_binary(Bin) ->
    list_to_binary(htmlize_l(binary_to_list(Bin)));
htmlize(List) when is_list(List) ->
    htmlize_l(List).



htmlize_char($>) ->
    <<"&gt;">>;
htmlize_char($<) ->
    <<"&lt;">>;
htmlize_char($&) ->
    <<"&amp;">>;
htmlize_char($") ->
    <<"&quot;">>;
htmlize_char(X) ->
    X.


%% htmlize list (usually much more efficient than above)
htmlize_l(List) ->
    htmlize_l(List, []).

htmlize_l([], Acc) -> lists:reverse(Acc);
htmlize_l([$>|Tail], Acc) ->
    htmlize_l(Tail, [$;,$t,$g,$&|Acc]);
htmlize_l([$<|Tail], Acc) ->
    htmlize_l(Tail, [$;,$t,$l,$&|Acc]);
htmlize_l([$&|Tail], Acc) ->
    htmlize_l(Tail, [$;,$p,$m,$a,$&|Acc]);
htmlize_l([$"|Tail], Acc) ->
    htmlize_l(Tail, [$; , $t, $o,  $u,  $q  ,$&|Acc]);

htmlize_l([X|Tail], Acc) when is_integer(X) ->
    htmlize_l(Tail, [X|Acc]);
htmlize_l([X|Tail], Acc) when is_binary(X) ->
    X2 = htmlize_l(binary_to_list(X)),
    htmlize_l(Tail, [X2|Acc]);
htmlize_l([X|Tail], Ack) when is_list(X) ->
    X2 = htmlize_l(X),
    htmlize_l(Tail, [X2|Ack]).

%%

deepmember(_C,[]) ->
    false;
deepmember(C,[C|_Cs]) ->
    true;
deepmember(C,[L|Cs]) when is_list(L) ->
    case deepmember(C,L) of
        true  -> true;
        false -> deepmember(C,Cs)
    end;
deepmember(C,[N|Cs]) when C /= N ->
    deepmember(C, Cs);
deepmember(_C,<<>>) ->
    false;
deepmember(C, <<C,_Cs/binary>>) ->
    true;
deepmember(C, <<_,Cs/binary>>) ->
    deepmember(C, Cs).
