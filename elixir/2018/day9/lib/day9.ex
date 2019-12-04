defmodule Day9 do
  @moduledoc """
  Documentation for Day9.
  """

  @doc """
  iex> Day9.solution_1
  409832
  """
  def solution_1 do
    {_marbles, scores} =
      next_marble(
        1,
        %{current: 0, circle: %{0 => %{n: 0, p: 0}}},
        %{},
        1,
        Day9.Input.input()
      )

     scores
     |> Map.values()
     |> Enum.map(fn list -> Enum.sum(list) end)
     |> Enum.max()
  end

  @doc """
  iex> Day9.solution_2
  3469562780
  """
  def solution_2 do
    {_marbles, scores} =
      next_marble(
        1,
        %{current: 0, circle: %{0 => %{n: 0, p: 0}}},
        %{},
        1,
        Day9.Input.input2()
      )

     scores
     |> Map.values()
     |> Enum.map(fn list -> Enum.sum(list) end)
     |> Enum.max()
  end

  defp next_marble(points, marbles, scores, _player, %{points: max}) when points > max do
    {marbles, scores}
  end

  defp next_marble(x, marbles, scores, player, input) do
    next_player =
      case player == input.players do
        true -> 1
        false -> player + 1
      end

    {new_marbles, new_scores} = score(x, marbles, scores, player)
    next_marble(x + 1, new_marbles, new_scores, next_player, input)
  end

  defp score(x, marbles, scores, player) when rem(x, 23) == 0 do
    score = Map.get(scores, player, [])
    {marble_point, new_marbles} = get_sevenths(marbles)
    {new_marbles, Map.put(scores, player, [x, marble_point | score])}
  end

  defp score(x, %{current: current, circle: circle}, scores, _player) do
    left = circle[current].n
    right = circle[left].n
    new_current = %{p: left, n: right}
    new_left = %{circle[left] | n: x}

    new_circle =
      circle
      |> Map.put(x, new_current)
      |> Map.put(left, new_left)

    new_right = %{new_circle[right] | p: x}

    new_circle =
      new_circle
      |> Map.put(right, new_right)

    {%{current: x, circle: new_circle}, scores}
  end

  defp get_sevenths(%{current: current, circle: circle}) do
    seventh =
      1..7
      |> Enum.reduce(current, fn _x, curr ->
        circle[curr].p
      end)

    next = circle[seventh].n
    prev = circle[seventh].p

    new_circle =
      circle
      |> Map.delete(seventh)
      |> Map.put(next, %{circle[next] | p: prev})
      |> Map.put(prev, %{circle[prev] | n: next})

    {seventh, %{current: next, circle: new_circle}}
  end
end

defmodule Day9.Input do
  def input do
    %{players: 428, points: 72061}
  end

  def input2 do
    %{players: 428, points: 7206100}
  end
end
