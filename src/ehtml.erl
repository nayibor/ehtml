-module(ehtml).

-export([ehtml_expand/1,ehtml_expander/1,ehtml_apply/2]).
-export([format/1,format/2,format/3]).
-export([sformat/1,sformat/2]).
-export([parse/1,parse/2,h2e/1]).



%% @doc This is for expanding erlang terms into html based on erlang input terms.
%% @end
ehtml_expand(Ch)->
    yaws_ehtml:ehtml_expand(Ch).
    

%% @doc  This is an expander that can be used for creating optimized ehtml template.
%% @end
ehtml_expander(Ch)->
    yaws_ehtml:ehtml_expander(Ch).


%% @doc This is a template applyer that takes a template and a proplist and generates the html
%% @and 
ehtml_apply(Expander,Env)->
    yaws_ehtml:ehtml_apply(Expander, Env).


%% @doc this is for expanding erlang terms into exhtml  based on erlang input terms
format(Data)->
    yaws_exhtml:format(Data).


%% @doc this is for expanding erlang term into exhtml and repeating it n times
format(Data,N)->
    yaws_exhtml:format(Data, N).


%% @doc this is for expanding erlang terms,repeating it and has a function for converting values to strings
format(Data,N,Value2StringF)->
    yaws_exhtml:format(Data, N, Value2StringF).


%% @doc this is for expanding erlang terms into exhtml based on erlang input terms 
sformat(Data)->
    yaws_exhtml:sformat(Data).


%% @doc this is for expanding erlang terms into exhtml and has an extra function for converting values to strings
sformat(Data,Value2StringF)->
    yaws_exhtml:sformat(Data, Value2StringF).


%% @doc this function is for converting html into ehtml given an html input filename
parse(Name)->
    yaws_html:parse(Name).


%% @doc this function is for converting html into ehtml givng path for input and output filename
parse(Name,Out)->
    yaws_html:parse(Name, Out).


%% @doc this function is for converting html into ehtml giving the input binary or string
h2e(Input)->
    yaws_html:h2e(Input).
