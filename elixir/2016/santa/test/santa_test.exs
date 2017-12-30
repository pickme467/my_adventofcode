defmodule SantaTest do
  use ExUnit.Case, async: true
  doctest Santa.Day13.Step
  doctest Santa.Day13
  doctest Santa.Day15
  doctest Santa.Day16
  doctest Santa.Day17
  doctest Santa.Day18
  doctest Santa.Day19
  doctest Santa.Day20
  doctest Santa.Day21
  doctest Santa.Day22
  doctest Santa.Day23

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

  test "Arbiter counts coordinates", %{arbiter: arbiter} do
    one_list = [1, 2, 3]
    another_list = [1, 3, 7]
    map = MapSet.union(MapSet.new(one_list), MapSet.new(another_list))
    Santa.Day13.Arbiter.add_path(arbiter, one_list)
    assert Santa.Day13.Arbiter.get_all_coordinates(arbiter) == length(one_list)
    Santa.Day13.Arbiter.add_path(arbiter, another_list)
    assert Santa.Day13.Arbiter.get_all_coordinates(arbiter) == MapSet.size(map)
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

  test "If current path length is same as shortest one,
  that means we failed finding it" do
    {:not_found, _} = Santa.Day13.Step.execute(1, {2, 3}, {5, 4}, [{1, 1}])
  end

  test "If we are at wall, we fail" do
    {:not_found, _} = Santa.Day13.Step.execute(10, {2, 3}, {1, 2}, [])
  end

  test "If our current location is on path, we fail" do
    {:not_found, _} = Santa.Day13.Step.execute(
      10, {2, 3}, {1, 2}, [{7, 8}, {1, 2}, {2, 3}])
  end

  test "If our current location is invalid we fail" do
    {:not_found, _} = Santa.Day13.Step.execute(10, {2, 3}, {-1, 2}, [])
    {:not_found, _} = Santa.Day13.Step.execute(10, {2, 3}, {1, -2}, [])
  end
end

defmodule SantaLongTests do
  use ExUnit.Case, async: true
  @moduletag timeout: 900_000

  doctest Santa.Day14
end
