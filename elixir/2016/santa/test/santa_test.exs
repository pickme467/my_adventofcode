defmodule SantaTest do
  use ExUnit.Case
  doctest Santa.Day13.Step
  doctest Santa.Day13

  test "one one is not a wall" do
    assert Santa.Day13.Step.is_wall(Santa.Day13.Step.get_my_number, {1, 1})
    == false
  end

  setup do
    {:ok, supervisor} = Supervisor.start_link([], strategy: :one_for_one)
    {:ok, arbiter} = Supervisor.start_child(supervisor,
      Santa.Day13.Arbiter.child_spec([]))
    %{arbiter: arbiter}
  end

  test "Arbiter returns default shortest path", %{arbiter: arbiter} do
    assert Santa.Day13.Arbiter.get_shortest(arbiter) == 100
  end

  test "Arbiter when set new shortest returns new shortest",
    %{arbiter: arbiter} do
    new_shortest = [1, 2, 5]
    Santa.Day13.Arbiter.new_shortest(arbiter, new_shortest)
    assert Santa.Day13.Arbiter.get_shortest(arbiter) == length(new_shortest)
  end

  test "Arbiter when set new shortest and then longer returns new shortest",
    %{arbiter: arbiter} do
    new_shortest = [1, 2, 5]
    Santa.Day13.Arbiter.new_shortest(arbiter, new_shortest)
    Santa.Day13.Arbiter.new_shortest(arbiter, new_shortest ++ [23])
    assert Santa.Day13.Arbiter.get_shortest(arbiter) == length(new_shortest)
  end

  test "If good position reached new current position is added
  to the wisited path" do
    path = [:path]
    current = {1, 1}
    {:continue, [^current | ^path], _next_locations} =
      Santa.Day13.Step.execute(10, {10, 10}, current, path)
  end

  test "Good position returns list of four neighbours" do
    current = {1, 1}
    {:continue, _path, locations} =
      Santa.Day13.Step.execute(10, {10, 10}, current, [])
    assert length(locations) == 4
    assert Enum.member?(locations, {1, 2})
    assert Enum.member?(locations, {1, 0})
    assert Enum.member?(locations, {0, 1})
    assert Enum.member?(locations, {2, 1})
  end
end
