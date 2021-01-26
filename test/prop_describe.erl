-module(prop_describe).

-include_lib("proper/include/proper.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% Here, I need to generate a fixed list so that proper has no
%%% problems with running out of data. Originally,
%%% I compared the length of both lists and only used if X and Y are
%%% equal in length. It caused generator failures.
%%% @end
%%%-------------------------------------------------------------------
prop_describe_returns_list_of_documentation_string() ->
    ?FORALL(
        {X, Y},
        {proper_types:fixed_list([atom(), atom(), atom(), atom(), atom()]),
            proper_types:fixed_list([binary(), binary(), binary(), binary(), binary()])},
        zip_lists(X, Y)
    ).

zip_lists(Atoms, Strings) ->
    Definitions = lists:zipwith(
        fun(Atom, String) ->
            {Atom, [{documentation, String}]}
        end,
        Atoms,
        Strings
    ),
    ExpectedResult = lists:zipwith(
        fun(Atom, String) ->
            {Atom, String}
        end,
        Atoms,
        Strings
    ),
    simple_options:describe(Definitions) =:= ExpectedResult.
