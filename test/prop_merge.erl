-module(prop_merge).

-include_lib("proper/include/proper.hrl").

prop_documentation_is_really_required() ->
    ?FORALL(
        [{Name, [_, _Default, _Error]}] = Options,
        [
            {
                proper_types:atom(),
                [
                    proper_types:exactly(required),
                    {proper_types:exactly(default), proper_types:term()},
                    {proper_types:exactly(error), proper_types:term()}
                ]
            }
        ],
        merge(Options) =:= {error, {Name, must_have_documentation}}
    ).

prop_defaults_are_substituted() ->
    ?FORALL(
        [{Name, [{_, Default}, _Error, _Documentation]}] = Options,
        [
            {
                proper_types:atom(),
                [
                    {proper_types:exactly(default), proper_types:term()},
                    {proper_types:exactly(error), proper_types:term()},
                    {proper_types:exactly(documentation), proper_types:term()}
                ]
            }
        ],
        merge(Options) =:= [{Name, Default}]
    ).

prop_validation_success() ->
    ?FORALL(A, proper_types:atom(), validate_is_atom([{name, A}]) == [{name, A}]).

prop_validation_failure() ->
    ?FORALL(
        A,
        proper_types:binary(10),
        validate_is_atom([{name, A}]) == {error, {name, is_invalid}}
    ).

prop_raise_custom_value() ->
    ?FORALL(
        A,
        proper_types:binary(10),
        validate_is_atom_custom([{name, A}]) == {error, {name, expected_atom}}
    ).

prop_validation_must_return_bool_or_atom() ->
    ?FORALL(
        A,
        proper_types:binary(10),
        failed_validate([{name, A}]) ==
            {error, {name, validation_return, {expected_type, atom}, {got, 1}}}
    ).

failed_validate(UserOpts) ->
    DefaultOpts = [
        {name, [{documentation, <<"name of the process">>}, {validation, fun(_) -> 1 end}]}
    ],
    try simple_options:merge(UserOpts, DefaultOpts) of
        X -> X
    catch
        error:Y -> Y
    end.

validate_is_atom(UserOpts) ->
    DefaultOpts = [
        {name, [{documentation, <<"name of the process">>}, {validation, fun is_atom/1}]}
    ],
    try simple_options:merge(UserOpts, DefaultOpts) of
        X -> X
    catch
        error:Y -> Y
    end.

validate_is_atom_custom(UserOpts) ->
    Definitions = [
        {name, [
            {documentation, <<"name of the process">>},
            {validation, fun(X) ->
                case is_atom(X) of
                    false -> expected_atom;
                    true -> true
                end
            end}
        ]}
    ],
    try simple_options:merge(UserOpts, Definitions) of
        X -> X
    catch
        error:Y -> Y
    end.

merge(Options) ->
    try simple_options:merge([], Options) of
        X -> X
    catch
        error:Y -> Y
    end.
