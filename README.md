ehtml
=====

An OTP library for simple erlang term representation of html.
the library converts erlang terms to and from html.

it's a thin wrapper around a popular erlang web server api for 
conversion of erlang terms to html and vice versa.

erlang to html
=====
to convert erlang to html you use the `ehtml:ehtml_expand/1` function.

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


for converting html to erlang
-----
```erlang
7> ehtml:h2e(<<"<html><head><title>hello world</title></head><body><p>hello world</p></body></html>">>).
{ehtml,[], 
	[{html,[], 
     [{head,[],{title,[],"hello world"}},
         {body,[],{p,[],"hello world"}}]}]}  	 
8> 
99> ehtml:h2e(<<"<p><h1 name=\"header\" id=\"header\">header1</h1></p>">>).
{ehtml,[],
    [{p,[],{h1,[{name,"header"},{id,"header"}],"header1"}}]}

```



Build
-----
` $ rebar3 compile`

` $ mix compile`
