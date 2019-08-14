defmodule Day10Test do
  use ExUnit.Case
  doctest Day10

  setup_all do
    {:ok, result: Day10.part_1_and_2()}
  end

  test "the height of a message is 9 characters", context do
    {map, 9, _iteration} = context[:result]
    Day10.print(map)
  end

  test "the iteration number is 10595", context do
    {map, _height, 10595} = context[:result]
    Day10.print(map)
  end
end
