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
%%%              also return any atom that will be part of the thrown error.
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

-export([merge/2]).

-define(DefaultError, is_invalid).
-define(AlwaysValid, fun(_) -> true end).

-type proplist() :: [{atom(), term()}].

%%%-------------------------------------------------------------------
%%% @doc
%%% Merge caller-provided options with the defined schema. Performs
%%% the following steps in sequence:
%%% Ensure all options contain a "documentation" definition.
%%% For each defined option, raise if it is required and user did
%%% not provide a value.
%%% Validate each user-provided option with provided validation
%%% function.
%%% @end
%%%-------------------------------------------------------------------
-spec merge(list(), proplist()) -> proplist().
merge(UserOpts, DefaultOpts) when is_list(UserOpts) ->
    ok = have_documentation(DefaultOpts),
    do_merge(DefaultOpts, UserOpts, []);
merge(_, _) ->
    throw({error, options_must_be_list}).

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
            throw({error, {Key, must_have_documentation}});
        _ ->
            have_documentation(Rest)
    end.

-spec validate(term(), atom(), proplist()) -> boolean().
validate(Value, Key, Specification) ->
    ValidationFunction = proplists:get_value(validation, Specification, ?AlwaysValid),
    ValidationResult =
        try ValidationFunction(Value) of
            false ->
                Error = proplists:get_value(error_message, Specification, ?DefaultError),
                throw({error, {Key, Error}});
            X when is_atom(X) andalso X =/= true andalso X =/= false ->
                throw({error, {Key, validation, X}});
            true ->
                true;
            _ ->
                throw({error, {Key, validation, expected_bool}})
        catch
            _ ->
                throw({error, {Key, validation, expected_bool}})
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
                    throw({error, {Key, is_required}});
                _ ->
                    ok
            end
    end.
