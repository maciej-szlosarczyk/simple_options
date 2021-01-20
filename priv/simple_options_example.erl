-module(simple_options_example).

-define(OPTIONS, [
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

-export([
    substitute_default/0,
    validate_required/0,
    validate_value/0,
    validate_name/0,
    validate_options/0,
    test_matching/1
]).

test_matching(Input) ->
    X = 1,
    case Input of
        Y when Y =:= X -> ok;
        Y -> {error, Y}
    end.

validate_options() ->
    UserOpts = application:get_all_env(simple_options),
    simple_options:merge(UserOpts, ?OPTIONS).

substitute_default() ->
    UserOpts = [{pool_size, 1}],
    simple_options:merge(UserOpts, ?OPTIONS).

validate_required() ->
    UserOpts = [],
    simple_options:merge(UserOpts, ?OPTIONS).

validate_name() ->
    UserOpts = [{pool_size, 10}, {name, <<"local">>}],
    simple_options:merge(UserOpts, ?OPTIONS).

validate_value() ->
    UserOpts = [{pool_size, <<"1">>}, {name, 1}],
    simple_options:merge(UserOpts, ?OPTIONS).
