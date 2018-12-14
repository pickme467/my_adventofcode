defmodule Day7 do
  @moduledoc """
  Documentation for Day7.
  """

  @doc """
  iex> Day7.solution_1
  "BFGKNRTWXIHPUMLQVZOYJACDSE"
  """
  def solution_1 do
    {successors, predcessors} =
      Day7.Input.get_input()
      |> Enum.reduce({%{}, %{}}, fn
        {before_task, after_task}, {successors, predcessors} ->
          all_after = Map.get(successors, before_task, [])
          all_before = Map.get(predcessors, after_task, [])

          {
            Map.put(successors, before_task, Enum.sort([after_task | all_after])),
            Map.put(predcessors, after_task, Enum.sort([before_task | all_before]))
          }
      end)

    all_successors =
      successors
      |> Map.values()
      |> List.flatten()
      |> Enum.uniq()

    first =
      successors
      |> Map.keys()
      |> Enum.filter(fn x -> not (x in all_successors) end)
      |> Enum.sort()

    traverse(first, successors, predcessors, [])
  end

  defp traverse([], _, _, solution) do
    solution
    |> Enum.reverse()
    |> List.to_string()
  end

  defp traverse([x | rest], successors, predcessors, solution) do
    new_solution = [x | solution]

    to_be_added = Map.get(successors, x, [])

    can_be_added =
      to_be_added
      |> Enum.filter(fn s ->
        predcessors
        |> Map.get(s, [])
        |> Enum.filter(fn x -> not (x in new_solution) end)
        |> length() == 0
      end)

    new_successors =
      case to_be_added -- can_be_added do
        [] -> Map.delete(successors, x)
        new_list -> Map.put(successors, x, Enum.sort(new_list))
      end

    traverse(Enum.sort(can_be_added ++ rest), new_successors, predcessors, new_solution)
  end
end

defmodule Day7.Input do
  def get_input() do
    input()
    |> String.split("\n")
    |> Enum.map(fn
      x ->
        [_Step, a, _must, _be, _finished, _before, _step, b | _rest] = String.split(x)
        {a, b}
    end)
  end

  defp input do
    "Step B must be finished before step K can begin.
Step F must be finished before step I can begin.
Step T must be finished before step U can begin.
Step R must be finished before step Z can begin.
Step N must be finished before step S can begin.
Step X must be finished before step Y can begin.
Step I must be finished before step Y can begin.
Step K must be finished before step L can begin.
Step U must be finished before step J can begin.
Step G must be finished before step L can begin.
Step W must be finished before step A can begin.
Step H must be finished before step Q can begin.
Step M must be finished before step L can begin.
Step P must be finished before step L can begin.
Step L must be finished before step A can begin.
Step V must be finished before step Y can begin.
Step Q must be finished before step Y can begin.
Step Z must be finished before step J can begin.
Step O must be finished before step D can begin.
Step Y must be finished before step A can begin.
Step J must be finished before step E can begin.
Step A must be finished before step E can begin.
Step C must be finished before step E can begin.
Step D must be finished before step E can begin.
Step S must be finished before step E can begin.
Step B must be finished before step R can begin.
Step U must be finished before step O can begin.
Step X must be finished before step I can begin.
Step C must be finished before step S can begin.
Step O must be finished before step S can begin.
Step J must be finished before step D can begin.
Step O must be finished before step E can begin.
Step Z must be finished before step O can begin.
Step J must be finished before step C can begin.
Step P must be finished before step Y can begin.
Step X must be finished before step S can begin.
Step O must be finished before step Y can begin.
Step J must be finished before step A can begin.
Step H must be finished before step C can begin.
Step P must be finished before step D can begin.
Step Z must be finished before step S can begin.
Step T must be finished before step Z can begin.
Step Y must be finished before step C can begin.
Step X must be finished before step H can begin.
Step R must be finished before step Y can begin.
Step T must be finished before step W can begin.
Step L must be finished before step O can begin.
Step G must be finished before step Z can begin.
Step H must be finished before step P can begin.
Step I must be finished before step U can begin.
Step H must be finished before step V can begin.
Step N must be finished before step Y can begin.
Step Q must be finished before step E can begin.
Step H must be finished before step D can begin.
Step P must be finished before step O can begin.
Step T must be finished before step I can begin.
Step W must be finished before step V can begin.
Step K must be finished before step M can begin.
Step R must be finished before step W can begin.
Step B must be finished before step T can begin.
Step U must be finished before step A can begin.
Step N must be finished before step H can begin.
Step F must be finished before step U can begin.
Step Q must be finished before step O can begin.
Step Y must be finished before step S can begin.
Step V must be finished before step O can begin.
Step W must be finished before step C can begin.
Step Y must be finished before step J can begin.
Step T must be finished before step V can begin.
Step N must be finished before step D can begin.
Step U must be finished before step Q can begin.
Step A must be finished before step C can begin.
Step U must be finished before step M can begin.
Step Q must be finished before step S can begin.
Step P must be finished before step V can begin.
Step B must be finished before step Z can begin.
Step W must be finished before step Q can begin.
Step L must be finished before step S can begin.
Step I must be finished before step P can begin.
Step G must be finished before step P can begin.
Step L must be finished before step C can begin.
Step K must be finished before step A can begin.
Step D must be finished before step S can begin.
Step I must be finished before step H can begin.
Step R must be finished before step M can begin.
Step Q must be finished before step D can begin.
Step K must be finished before step O can begin.
Step I must be finished before step C can begin.
Step N must be finished before step O can begin.
Step R must be finished before step X can begin.
Step P must be finished before step C can begin.
Step B must be finished before step Y can begin.
Step G must be finished before step E can begin.
Step L must be finished before step V can begin.
Step W must be finished before step Y can begin.
Step C must be finished before step D can begin.
Step M must be finished before step J can begin.
Step F must be finished before step N can begin.
Step T must be finished before step Q can begin.
Step I must be finished before step E can begin.
Step A must be finished before step D can begin."
  end
end
