-module(prop_simple_options).

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

prop_default_substitution() ->
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

prop_simple_validation_success() ->
    ?FORALL(A, proper_types:atom(), validate_is_atom([{name, A}]) == [{name, A}]).

prop_simple_validation_failure() ->
    ?FORALL(
        A,
        proper_types:binary(10),
        validate_is_atom([{name, A}]) == {error, {name, is_invalid}}
    ).

validate_is_atom(UserOpts) ->
    DefaultOpts = [
        {name, [{documentation, <<"name of the process">>}, {validation, fun is_atom/1}]}
    ],
    try simple_options:merge(UserOpts, DefaultOpts) of
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

validate_all(UserOpts) ->
    DefaultOpts = [
        {name, [required, {documentation, <<"name of the process">>}, {validation, fun is_atom/1}]},
        {description, [
            {documentation, <<"name of the process">>},
            {default, 100},
            {validation, fun(X) -> X >= 100 end}
        ]}
    ],
    try simple_options:merge(UserOpts, DefaultOpts) of
        X -> X
    catch
        error:Y -> Y
    end.
