ehtml
=====

An OTP library for simple erlang term representation of html.
the library converts erlang terms to and from html.

it's a thin wrapper around a popular erlang web server api for 
conversion of erlang terms to html and vice versa.

erlang to html
=====
to convert erlang to html you use the `ehtml:expand/1` functions.
you can use the rules below for the construction of the erlang terms.

```erlang
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
```

erlang examples
=====
```erlang
%% for constructing a simple table element
36> ehtml:ehtml_expand({table}).
"<","table",["></","table",">"]]

%% for constructing a div with a body
37> ehtml:ehtml_expand({'div',[],<<"this is the body of the div">>}).
["\n","<","div",[],">",<<"this is the body of the div">>,
"</","div",">"] 

%% for constructing a div with binary attributes and body
38> ehtml:ehtml_expand({'div',[{name,<<"div">>},{id,<<"id">>}],<<"this is the body of the div">>}). 
["\n","<","div", 
 [" name",[61,34,<<"div">>,34]," id",[61,34,<<"id">>,34]],
 ">",<<"this is the body of the div">>,"</","div",">"] 

%% for constructing a simple table element with attributes
40> ehtml:ehtml_expand({table,[{name,"name"},{id,"id"}]}).
["\n","<","table",
 [" name",[61,34,"name",34]," id",[61,34,"id",34]],
 ["></","table",">"]]

%% for constructing a simple table element with attributes and a body
41> Table_body = lists:map(fun(X)-> {tr,[],X} end,lists:seq(1,5) ). 
[{tr,[],1},{tr,[],2},{tr,[],3},{tr,[],4},{tr,[],5}] 
42> ehtml:ehtml_expand({table,[{name,"name"},{id,"id"}],Table_body}).
["\n","<","table", 
 [" name",[61,34,"name",34]," id",[61,34,"id",34]],
  ">", 
 [["\n","<","tr",[],">",1,"</","tr",">"], 
  ["\n","<","tr",[],">",2,"</","tr",">"],
  ["\n","<","tr",[],">",3,"</","tr",">"], 
  ["\n","<","tr",[],">",4,"</","tr",">"], 
  ["\n","<","tr",[],">",5,"</","tr",">"]],
 "</","table",">"]
```

elixir examples
=====
```elixir
%% for constructing a simple paragraph element
iex(10)> :ehtml.ehtml_expand({:p}) 
[~c"<", ~c"p", [~c"></", ~c"p", ~c">"]]

%% for constructing a div with attributes and body
iex(14)> :ehtml.ehtml_expand({:div,[{:name,"div"},{:id,"div"}],"this is my body"}) 
[ 
 ~c"\n",
  ~c"<", 
  ~c"div",
  [~c" name", [61, 34, "div", 34], ~c" id", [61, 34, "div", 34]],
 ~c">",
  "this is my body",
  ~c"</", 
  ~c"div", 
  ~c">" 
]

%% for constructing a table with attributes and a body
iex(20)> :ehtml.ehtml_expand({:table,[{:name,"table"},{:id,"table"}],Enum.map(1 .. 10,fn(x)-> {:tr,[],x} end)})
[
  ~c"\n", 
  ~c"<",
  ~c"table",
  [~c" name", [61, 34, "table", 34], ~c" id", [61, 34, "table", 34]],
  ~c">",
  [
    [~c"\n", ~c"<", ~c"tr", [], ~c">", 1, ~c"</", ~c"tr", ~c">"],
    [~c"\n", ~c"<", ~c"tr", [], ~c">", 2, ~c"</", ~c"tr", ~c">"],
    [~c"\n", ~c"<", ~c"tr", [], ~c">", 3, ~c"</", ~c"tr", ~c">"],
    [~c"\n", ~c"<", ~c"tr", [], ~c">", 4, ~c"</", ~c"tr", ~c">"],
    [~c"\n", ~c"<", ~c"tr", [], ~c">", 5, ~c"</", ~c"tr", ~c">"],
    [~c"\n", ~c"<", ~c"tr", [], ~c">", 6, ~c"</", ~c"tr", ~c">"],
    [~c"\n", ~c"<", ~c"tr", [], ~c">", 7, ~c"</", ~c"tr", ~c">"],
    [~c"\n", ~c"<", ~c"tr", [], ~c">", 8, ~c"</", ~c"tr", ~c">"],
    [~c"\n", ~c"<", ~c"tr", [], ~c">", 9, ~c"</", ~c"tr", ~c">"],
    [~c"\n", ~c"<", ~c"tr", [], ~c">", 10, ~c"</", ~c"tr", ~c">"]
  ],
  ~c"</",
  ~c"table",
  ~c">"
]

```

using templates
=====
this is for creating ehtml templates which can be used reused later 
with variable bindings.
```erlang
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
```

templating example below
```erlang
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
    Page2 = ehtml_apply(Expand, Bs2).
```


for converting html to erlang term format example below
```erlang
7> ehtml:h2e(<<"<html><head><title>hello world</title></head><body><p>hello world</p></body></html>">>).
{ehtml,[], 
	[{html,[], 
     [{head,[],{title,[],"hello world"}},
         {body,[],{p,[],"hello world"}}]}]}  	 
8> 
```



Build
-----
` $ rebar3 compile`

` $ mix compile`
