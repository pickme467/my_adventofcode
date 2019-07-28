defmodule Day8.MakeTree do
  @moduledoc false

  @doc """
  iex> Day8.MakeTree.make_tree([0, 1, 1])
  {%{meta: [1]}, []}

  iex> Day8.MakeTree.make_tree([0, 1, 13])
  {%{meta: [13]}, []}

  iex> Day8.MakeTree.make_tree([0, 2, 1, 3])
  {%{meta: [1, 3]}, []}

  iex> Day8.MakeTree.make_tree([0, 4, 1, 3, 4, 5])
  {%{meta: [1, 3, 4, 5]}, []}

  iex> Day8.MakeTree.make_tree([1, 1, 0, 1, 4, 2])
  {%{meta: [2], child: %{1 => %{meta: [4]}}}, []}

  iex> Day8.MakeTree.make_tree([1, 1, 1, 1, 0, 1, 2, 3, 4])
  {%{meta: [4], child: %{1 => %{meta: [3], child: %{1 => %{meta: [2]}}}}}, []}

  iex> Day8.MakeTree.make_tree([2, 1, 0, 1, 3, 0, 1, 4, 5])
  {%{meta: [5], child: %{1 => %{meta: [3]}, 2 => %{meta: [4]}}}, []}

  iex> Day8.MakeTree.make_tree([3, 1, 0, 1, 3, 0, 1, 4, 0, 1, 5, 6])
  {%{meta: [6], child: %{1 => %{meta: [3]}, 2 => %{meta: [4]}, 3 => %{meta: [5]}}}, []}
  """
  def make_tree([0, meta_length | data]) do
    {meta_data, data_rest} = Enum.split(data, meta_length)
    {%{meta: meta_data}, data_rest}
  end

  def make_tree([n, meta_length | data]) do
    {children, data_rest} =
      1..n
      |> Enum.reduce({%{}, data}, fn child, {children, input} ->
        {child_data, next_input} = make_tree(input)
        {Map.put(children, child, child_data), next_input}
      end)
    {my_meta, data_rest_rest} = Enum.split(data_rest, meta_length)
    {%{child: children, meta: my_meta}, data_rest_rest}
  end
end
