-module(ehtml).

-export([ehtml_expand/1,ehtml_expander/1,ehtml_apply/2]).





%% @doc This is for expanding erlang terms into html based on input terms.
%% @end
ehtml_expand(Ch)->
    yaws_ehtml:ehtml_expand(Ch).
    

%% @doc  This is an expander that can be used for creating optimized ehtml template.
%% @end
ehtml_expander(Ch)->
    yaws_ehtml:ehtml_expander(Ch).


%% @doc This is an applyer that takes a template and a proplist and generates the html
%% @and 
ehtml_apply(Expander,Env)->
    yaws_ehtml:ehtml_apply(Expander, Env).
