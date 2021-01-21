-module(simple_options_example).

%%%-------------------------------------------------------------------
%%% @doc
%%% Since in Erlang you can define complex functions in macros,
%%% you can put all you option definition into one.
%%% @end
%%%-------------------------------------------------------------------
-define(OPTIONS_DEFINITION, [
    {name, [
        {required, false},
        {default, local},
        {validation, fun(X) -> is_atom(X) orelse is_binary(X) end},
        {documentation, <<"Name of the node - either an atom or a binary.">>},
        {error_message, <<"must be an atom or a binary">>}
    ]},
    {pool_size, [
        required,
        {validation, fun(X) ->
            case {is_integer(X), X > 0} of
                {true, true} -> true;
                {false, _} -> expected_integer;
                {true, false} -> expected_larger_than_zero
            end
        end},
        {documentation, <<"Pool size as an integer. Must be bigger than 0">>}
    ]}
]).

-export([merge/1]).

merge(Options) -> simple_options:merge(Options, ?OPTIONS_DEFINITION).
