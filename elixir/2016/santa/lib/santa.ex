defmodule Santa.Day13 do
  @moduledoc """

  Day 13 is tricky. We know where we can go based on our current
  location and three new paths from that location. Three, because one
  is just our origin where we came from. So to make move we direct
  three locations to assess themselves and that's it.

  We need stopping conditions. Here they are:
  - current location is a wall
  - we reached longest allowed path (arbitrary set at start)
  - we found solution
  - we are at already visited spot

  The thing is that when a solution is found it might not be shortest,
  so every found solution shall be taken and used as longest path available

  What we need:
  - simple one for one supervisor
  - main task gathering data and giving info about longest path
  - worker task that check exit conditions and:
    - exit without notification
    - notifies about found solution
    - creates children tasks for next three spots and finishes

  """
  @doc """
  Day 13


      iex> Santa.Day13.go
      {1364, 31, 39}

  """
  def go do
    day_13_input()
  end

  defp day_13_input do
    {1364, 31, 39}
  end

  def is_wall number, {x, y} do
    require Integer
    value = number + (x * x + 3 * x + 2 * x * y + y + y * y)
    sum_all_ones = fn (bit, sum) -> sum + bit end
    all_ones = Integer.digits(value, 2)
    |> Enum.reduce(0, sum_all_ones)
    Integer.is_even(all_ones)
  end

end

defmodule Santa.Day13.Pathfinder do
  use Task
  @doc false

  def start_li
end
