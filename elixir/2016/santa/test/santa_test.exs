defmodule SantaTest do
  use ExUnit.Case
  doctest Santa.Day13

  test "one one is not a wall" do
    assert Santa.Day13.is_wall(0, {1, 1}) == false
  end
end
