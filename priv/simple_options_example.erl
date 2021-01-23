-module(simple_options_example).

%%%-------------------------------------------------------------------
%%% @doc
%%% You can put your definitions into a macro and use to validate
%%% before you start your application.
%%% @end
%%%-------------------------------------------------------------------
-define(OPTIONS_DEFINITION,
        [{name,
          [{required, false},
           {default, local},
           {validation,
            fun (X) ->
                    is_atom(X) orelse is_binary(X)
            end},
           {documentation, <<"Name of the node - either an atom or a binary.">>},
           {error_message, <<"must be an atom or a binary">>}]},
         {pool_size,
          [required,
           {validation,
            fun (X) ->
                    case {is_integer(X), X > 0} of
                      {true, true} ->
                          true;
                      {false, _} ->
                          expected_integer;
                      {true, false} ->
                          expected_larger_than_zero
                    end
            end},
           {documentation, <<"Pool size as an integer. Must be bigger than 0">>}]}]).

-export([start/2, start_inline/2, stop/1]).

start(_Type, _Args) ->
    UserOpts = application:get_env(my_application),
    _Options = simple_options:merge(UserOpts, ?OPTIONS_DEFINITION),
    ok.

%%%-------------------------------------------------------------------
%%% @doc
%%% Or you can put them inline.
%%% @end
%%%-------------------------------------------------------------------
start_inline(_Type, _Args) ->
    Definitions = [{name,
                    [{required, false},
                     {default, local},
                     {validation,
                      fun (X) ->
                              is_atom(X) orelse is_binary(X)
                      end},
                     {documentation, <<"Name of the node - either an atom or a binary.">>},
                     {error_message, <<"must be an atom or a binary">>}]}],
    UserOpts = application:get_env(my_application),
    _Options = simple_options:merge(UserOpts, Definitions),
    ok.

stop(_State) ->
    ok.
