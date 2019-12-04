defmodule Day11 do
  @moduledoc """
  Documentation for Day11.
  """

  @doc """
  iex> Day11.part_1()
  {243, 34}
  """
  def part_1 do
    cell_powers = for x <- 1..300, y <- 1..300, into: %{}, do: {{x, y}, calculate_cell(x, y)}

    powers =
      for x <- 1..298, y <- 1..298, into: %{}, do: {calculate_power(x, y, cell_powers), {x, y}}

    key = Enum.max(Map.keys(powers))
    Map.get(powers, key)
  end

  @doc """
  iex> Day11.part_2()
  {90, 214, 15}
  """
  def part_2 do
    results =
      for y <- 1..300 do
        tasks =
          for x <- 1..300 do
            Task.async(fn ->
              find_max_power_grid(x, y)
            end)
          end

        tasks
        |> Task.yield_many(10_000_000)
        |> Enum.map(fn {_task, result} -> result end)
      end

    powers =
      results
      |> List.flatten()
      |> Enum.map(fn {:ok, value} -> value end)
      |> Map.new()

    value = Enum.max(Map.keys(powers))

    Map.get(powers, value)
  end

  @doc """
  iex> Day11.calculate_cell(3, 5, 10)
  4

  iex> Day11.calculate_cell(122, 79, 57)
  -5

  iex> Day11.calculate_cell(217, 196, 39)
  0

  iex> Day11.calculate_cell(101, 153, 71)
  4
  """
  def calculate_cell(x, y, serial \\ 1718) do
    rack_id = x + 10
    power_level = (rack_id * y + serial) * rack_id
    hundredth_digit(power_level) - 5
  end

  defp hundredth_digit(x) do
    rem(Integer.floor_div(x, 100), 10)
  end

  defp calculate_power(x, y, cells) do
    values = for vx <- 0..2, vy <- 0..2, do: Map.get(cells, {x + vx, y + vy})
    Enum.sum(values)
  end

  defp find_max_power_grid(xs, ys) do
    range = Enum.min([300 - xs, 300 - ys])

    {_last, max, grid} =
      1..range
      |> Enum.reduce({calculate_cell(xs, ys), 0, 1}, fn i, {last, max, grid} ->
        current = last + calculate_row(xs, ys, i) + calculate_column(xs, ys, i)

        case current > max do
          true -> {current, current, i + 1}
          false -> {current, max, grid}
        end
      end)

    {max, {xs, ys, grid}}
  end

  defp calculate_row(x, y, shift) do
    row = for r <- x..(x + shift), do: calculate_cell(r, y + shift)

    Enum.sum(row)
  end

  defp calculate_column(x, y, shift) do
    column = for c <- y..(y + shift - 1), do: calculate_cell(x + shift, c)

    Enum.sum(column)
  end
end
