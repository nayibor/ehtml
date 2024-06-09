-module(ehtml).

-export([ehtml_expand/1]).
-export([h2e/1]).

-spec ehtml_expand(yaws_ehtml:ehtml()) -> iolist().
%% @doc This is for expanding erlang terms into html based on erlang input terms.
%% @end
ehtml_expand(Ch)->
    yaws_ehtml:ehtml_expand(Ch).

-spec h2e(list() | binary) -> yaws_ehtml:ehtml().
%% @doc this function is for converting html into ehtml giving the input binary or string
h2e(Input)->
    yaws_html:h2e(Input).
