defmodule SimpleOptions do
  @opts [
    name: [
      :required,
      documentation: "Node name. Must be an atom.",
      validation: &is_atom/1
    ]
  ]

  def merge_default(opts) do
    :simple_options.merge(opts, @opts)
  end

  def merge(opts) when is_list(opts) do
    options = [
      name: [
        required: true,
        documentation: "Name of the node - either an atom or a binary",
        validation: &validate_name(&1),
        error_message: "must be an atom or a binary."
      ],
      pool_size: [
        required: false,
        default: 10,
        documentation: "Pool size as an integer. Must be bigger than 0",
        validation: fn x ->
          case {is_integer(x), x > 0} do
            {true, true} -> true
            {false, _} -> :expected_integer
            {true, false} -> :expected_larger_than_zero
          end
        end
      ]
    ]

    :simple_options.merge(opts, options)
  end

  def validate_name(name) do
    is_atom(name) || is_binary(name)
  end
end
