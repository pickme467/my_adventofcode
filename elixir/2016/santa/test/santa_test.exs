defmodule SantaTest do
  use ExUnit.Case
  doctest Santa.Day13

  test "one one is not a wall" do
    assert Santa.Day13.is_wall(0, {1, 1}) == false
  end

  test "Arbiter returns default shortest path" do
    {:ok, arbiter} = Santa.Day13.Arbiter.start_link()
    assert Santa.Day13.Arbiter.get_shortest(arbiter) == 100
  end

  test "Arbiter when set new shortest returns new shortest" do
    {:ok, arbiter} = Santa.Day13.Arbiter.start_link()
    new_shortest = [1, 2, 5]
    Santa.Day13.Arbiter.new_shortest(arbiter, new_shortest)
    assert Santa.Day13.Arbiter.get_shortest(arbiter) == length(new_shortest)
  end

  test "Arbiter when set new shortest and then longer returns new shortest" do
    {:ok, arbiter} = Santa.Day13.Arbiter.start_link()
    new_shortest = [1, 2, 5]
    Santa.Day13.Arbiter.new_shortest(arbiter, new_shortest)
    Santa.Day13.Arbiter.new_shortest(arbiter, new_shortest ++ [23])
    assert Santa.Day13.Arbiter.get_shortest(arbiter) == length(new_shortest)
  end

end
