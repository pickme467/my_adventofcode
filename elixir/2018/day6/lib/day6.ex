defmodule Day6 do
  @moduledoc """
  Documentation for Day6.
  """

  @doc """
  iex> Day6.solution_1()
  4290
  """
  def solution_1 do
    input = Day6.Input.get_input()
    min_x =
      input
      |> Enum.sort_by(fn ({x, _y}) -> x end, &<=/2)
      |> hd()
      |> elem(0)
    max_x =
      input
      |> Enum.sort_by(fn ({x, _y}) -> x end, &>=/2)
      |> hd()
      |> elem(0)
    min_y =
      input
      |> Enum.sort_by(fn ({_x, y}) -> y end, &<=/2)
      |> hd()
      |> elem(1)
    max_y =
      input
      |> Enum.sort_by(fn ({_x, y}) -> y end, &>=/2)
      |> hd()
      |> elem(1)

    IO.puts "Minmax: #{inspect({{min_x, min_y}, {max_x, max_y}})}"

    tasks =
      for x <- min_x..max_x,
          y <- min_y..max_y do
          {x, y}
      end
      |> Enum.map(fn (coordinates) -> Task.async(fn ->
           find_shortest_path(coordinates, input)
           end)
         end)

      Task.yield_many(tasks, :infinity)
      |> Enum.reduce(%{}, fn
        ({_task, {:ok, point}}, acc) ->
          Map.put(acc, point, Map.get(acc, point, 0) + 1)
      end)
      |> Enum.sort_by(fn ({_k, v}) -> v end, &>=/2)
      |> Enum.filter(fn (x) -> is_finite_location(x, input) end)
      |> hd()
      |> elem(1)
  end

  defp find_shortest_path(point, list) do
    [{d1, p1}, {d2, _p2}] =
      list
      |> Enum.map(fn (coordinates) -> find_distance(point, coordinates) end)
      |> Enum.sort(&<=/2)
      |> Enum.take(2)
    case d1 == d2 do
      true -> :neutral
      false -> p1
    end
  end

  defp find_distance({x1, y1}, {x2, y2}) do
    {abs(x2 - x1) + abs(y2 - y1), {x2, y2}}
  end

  defp is_finite_location(_location, _all_locations) do
    true
  end
end

defmodule Day6.Input do
  def get_input do
    input()
    |> String.split("\n")
    |> Enum.map(fn (x) ->
      String.split(x, ", ")
      |> Enum.map(&String.to_integer/1)
      |> List.to_tuple() end)

  end
  defp input do
"315, 342
59, 106
44, 207
52, 81
139, 207
93, 135
152, 187
271, 47
223, 342
50, 255
332, 68
322, 64
250, 72
165, 209
129, 350
139, 118
282, 129
311, 264
216, 246
134, 42
66, 151
263, 199
222, 169
236, 212
320, 178
202, 288
273, 190
83, 153
88, 156
284, 305
131, 90
152, 88
358, 346
272, 248
317, 122
166, 179
301, 307
156, 128
261, 290
268, 312
89, 53
324, 173
353, 177
91, 69
303, 164
40, 221
146, 344
61, 314
319, 224
98, 143"
  end
end
