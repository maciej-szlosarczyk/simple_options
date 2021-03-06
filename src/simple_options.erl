%%%-------------------------------------------------------------------
%%% @doc
%%% Simple options definition for Erlang. Enforces single
%%% important rule when it comes to configuration options: they are
%%% required to be documentated.
%%%
%%% The library does not allow you to define types of attributes,
%%% instead relying on validation functions that you can provide
%%% yourself to validate the input data.
%%%
%%% Each value is defined a property list that can have the following
%%% keys:
%%% required - boolean, signifies if the option is required from the
%%%            caller.
%%% validation - A function that called on input. When not provided, it is
%%%              assumed that the option is always valid. The function can
%%%              also return any atom that will be part of the error.
%%% error_message - term that defines error to return from validation in case
%%%                 it is false.
%%% documentation - Documentation for the option in any form
%%%                 (list or binary.)
%%% default - Value that will be put in place if the caller did not
%%%           provide one.
%%% A simple example:
%%%
%%% ```
%%% {name, [required,
%%%         {default, erlang},
%%%         {validation, fun is_atom/1},
%%%         {error, must_be_atom},
%%%         {documentation, <<"name of the language">>}]}
%%%
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(simple_options).

-export([merge/2, describe/1]).

-define(DEFAULT_ERROR, is_invalid).
-define(ALWAYS_VALID, fun(_) -> true end).

-type proplist() :: [{atom(), term()} | atom()].

%%%-------------------------------------------------------------------
%%% @doc
%%% Merge caller-provided options with the defined schema. Performs
%%% the following steps in sequence:
%%% Ensure all options contain a "documentation" definition.
%%% For each defined option, errors out if it is required and user did
%%% not provide a value.
%%% Validate each user-provided option with provided validation
%%% function.
%%% @end
%%%-------------------------------------------------------------------
-spec merge(list(), proplist()) -> proplist().
merge(UserOpts, Definitions) when is_list(UserOpts) ->
    ok = have_documentation(Definitions),
    do_merge(Definitions, UserOpts, []);
merge(_, _) ->
    error({error, options_must_be_list}).

-spec do_merge(proplist(), proplist(), proplist()) -> proplist().
do_merge([{Key, Opts} | Rest], UserOpts, []) ->
    Default = proplists:get_value(default, Opts),
    Value = proplists:get_value(Key, UserOpts, Default),
    ok = ensure_required(Value, Key, Opts),
    true = validate(Value, Key, Opts),
    do_merge(Rest, UserOpts, [{Key, Value}]);
do_merge([{Key, Opts} | Rest], UserOpts, Collected) ->
    Default = proplists:get_value(default, Opts),
    Value = proplists:get_value(Key, UserOpts, Default),
    ok = ensure_required(Value, Key, Opts),
    true = validate(Value, Key, Opts),
    do_merge(Rest, UserOpts, [{Key, Value} | Collected]);
do_merge([], _UserOpts, Collected) ->
    Collected.

-spec have_documentation(proplist()) -> ok.
have_documentation([]) ->
    ok;
have_documentation([{Key, Opts} | Rest]) ->
    case proplists:get_value(documentation, Opts) of
        undefined ->
            error({error, {Key, must_have_documentation}});
        _ ->
            have_documentation(Rest)
    end.

-spec validate(term(), atom(), proplist()) -> true.
validate(Value, Key, Specification) ->
    ValidationFunction = proplists:get_value(validation, Specification, ?ALWAYS_VALID),
    ValidationResult =
        case ValidationFunction(Value) of
            false ->
                Error = proplists:get_value(error_message, Specification, ?DEFAULT_ERROR),
                error({error, {Key, Error}});
            true ->
                true;
            %% Handle custom atom that will be returned into the error.
            X when is_atom(X) andalso X =/= true andalso X =/= false ->
                error({error, {Key, X}});
            %% Everything else should throw an error
            NotAcceptedValue ->
                error(
                    {error,
                        {Key, validation_return, {expected_type, atom}, {got, NotAcceptedValue}}}
                )
        end,
    ValidationResult.

-spec ensure_required(term(), atom(), proplist()) -> ok.
ensure_required(Value, Key, Specification) ->
    case proplists:get_value(required, Specification) of
        undefined ->
            ok;
        false ->
            ok;
        true ->
            case Value of
                undefined ->
                    error({error, {Key, is_required}});
                _ ->
                    ok
            end
    end.

%%%-------------------------------------------------------------------
%%% @doc
%%% Return all defined option keys with their documentation values.
%%% @end
%%%-------------------------------------------------------------------
-spec describe(proplist()) -> proplist().
describe(Definitions) ->
    lists:map(
        fun({Key, Properties}) ->
            Documentation = proplists:get_value(documentation, Properties),
            {Key, Documentation}
        end,
        Definitions
    ).
