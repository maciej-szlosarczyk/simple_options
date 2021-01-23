simple_options
=====

A library that facilitates defining options. It allows you to validate the
options user passes into your library or inside your application, to ensure
it is properly configured before started.

This is still an experiment. It might be not tremendously useful to anyone.

```erl
Definitions = [
    {default_timeout, [
        required,
        {documentation,
         <<"Default timeout in miliseconds. Must be greater than 300.">>},
        {validation, fun
            (X) when is_integer(X) ->
                case X > 300 of
                    false ->
                        must_be_greater_than_300; %% Errors with this atom
                    true ->
                        true %%
                end;
            (_X) ->
                false %% Errors with value from error_message
        end},
        {error_message, <<"must be an integer greater than 300">>}
    ]},
    {name, [
        {default, my_app},
        {documentation, <<"Name of your application">>},
        {validation, fun is_atom/1}
    ]}
].
```

You can merge user provided options with your defintions:

```erl
UserOpts = [{default_timeout, 400}],
[{name,my_app},{default_timeout,400}] =
    simple_options:merge(UserOpts, Definitions).
```

When options provided by user are invalid or incomplete, you get an exception
error for the first defintion that does not pass validation:

```erl
UserOpts = [{default_timeout, 1}, {name, 1}],
simple_options:merge(UserOpts, Definitions).
** exception error: {error,{default_timeout,must_be_greater_than_300}}
```

## Examples

Examples of usage are in priv folder, there is one for Erlang, and one
for Elixir:

* [Erlang](priv/simple_options_example.erl)
* [Elixir](priv/simple_options_example.ex)

## Design

Simple_options validates only list of tuples, i.e application configuration
parameters.

Validation can be any function that takes the value the user provided and
returns true, false or any term to return in the runtime error.

The options you define must have a documentation key to encourage you to
provide users with information on what exactly are they modifying.

## Inspiration

This library is inspired by functionality provided by
[nimble_options](https://github.com/dashbitco/nimble_options) in Elixir,
but it makes different tradeoffs.

With simple_options, you can express more properties than type of a value,
but you are also exposed to making logical errors in your validation functions.

## License

Simple_options is released under MIT license.
