defmodule Day12 do
  @moduledoc """
  Documentation for Day12.
  """

  @doc """
    iex> Day12.part_1()
    3337
  """
  def part_1 do
    {sum, _flowers} = find_generation(20)
    sum
  end

  @doc """
    iex> Day12.part_2()
    4300000000349
  """
  def part_2 do
    try do
      find_generation(50_000_000_000)
    catch
      {iteration, {sum, flowers}} ->
        flowers * (50_000_000_000 - iteration) + sum
      end
  end

  defp find_generation(number) do
    {pots, matchers} = Day12.Input.input()

    final_pots =
      1..number
      |> Enum.reduce(pots, fn i, last ->

        next = ".." <> last <> ".."
        next = match(next, matchers)

        if String.equivalent?(String.trim(last, "."), String.trim(next, ".")) do
          throw {i, sum_identifiers(next, i)}
        end

        next
      end)

    sum_identifiers(final_pots, number)
  end

  defp match(pots, matchers) do
    (0..(String.length(pots) - 5)
     |> Enum.reduce("..", fn index, output ->
       to_match = String.slice(pots, index..(index + 4))
       pot = Map.get(matchers, to_match, ".")
       output <> pot
     end)) <> ".."
  end

  defp sum_identifiers(pots, number) do
    first_index = -2 * number

    {sum, _index, flowers} =
      pots
      |> String.splitter("", trim: true)
      |> Enum.reduce({0, first_index, 0}, fn
        "#", {sum, index, flowers} -> {sum + index, index + 1, flowers + 1}
        ".", {sum, index, flowers} -> {sum, index + 1, flowers}
      end)

    {sum, flowers}
  end
end

defmodule Day12.Input do
  def input do
    {"#...#####.#..##...##...#.##.#.##.###..##.##.#.#..#...###..####.#.....#..##..#.##......#####..####...",
     %{
       "#.#.#" => "#",
       "..###" => ".",
       "#..#." => "#",
       ".#..." => "#",
       "..##." => "#",
       "##.#." => "#",
       "##..#" => "#",
       "####." => "#",
       "...#." => "#",
       "..#.#" => "#",
       ".####" => "#",
       "#.###" => ".",
       "...##" => ".",
       "..#.." => ".",
       "#...#" => ".",
       ".###." => "#",
       ".#.##" => ".",
       ".##.." => "#",
       "....#" => ".",
       "#..##" => ".",
       "##.##" => "#",
       "#.##." => ".",
       "#...." => ".",
       "##..." => "#",
       ".#.#." => ".",
       "###.#" => "#",
       "#####" => "#",
       "#.#.." => ".",
       "....." => ".",
       ".##.#" => ".",
       "###.." => ".",
       ".#..#" => "."
     }}
  end

  def input_sample do
    {"#..#.#..##......###...###",
     %{
       "...##" => "#",
       "..#.." => "#",
       ".#..." => "#",
       ".#.#." => "#",
       ".#.##" => "#",
       ".##.." => "#",
       ".####" => "#",
       "#.#.#" => "#",
       "#.###" => "#",
       "##.#." => "#",
       "##.##" => "#",
       "###.." => "#",
       "###.#" => "#",
       "####." => "#"
     }}
  end
end
