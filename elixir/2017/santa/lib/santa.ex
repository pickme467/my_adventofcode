defmodule Santa.Day1 do
  @doc """
  Acceptance test
  iex> Santa.Day1.part_one
  1049
  """
  def part_one do
    [first | rest] = Santa.Day1.Input.input()
    find_sum(0, [first] ++ rest ++ [first])
  end

  @doc """
  Acceptance test
  iex> Santa.Day1.part_two
  1508
  """
  def part_two do
    {l1, l2} = Enum.split(Santa.Day1.Input.input(),
      round(length(Santa.Day1.Input.input())/2))
    find_distant_sum(0, Santa.Day1.Input.input(), l2 ++ l1)
  end

  defp find_sum(accumulator, []) do
    accumulator
  end

  defp find_sum(accumulator, [a, a | rest]) do
    find_sum(accumulator + a - ?0, [a | rest])
  end

  defp find_sum(accumulator, [_ | rest]) do
    find_sum(accumulator, rest)
  end

  defp find_distant_sum(accumulator, [], _) do
    accumulator
  end

  defp find_distant_sum(accumulator, [a | resta], [a | restb]) do
    find_distant_sum(accumulator + a - ?0, resta, restb)
  end

  defp find_distant_sum(accumulator, [_ | resta], [_ | restb]) do
    find_distant_sum(accumulator, resta, restb)
  end
end

defmodule Santa.Day2 do

  @doc """
  Acceptance test
  iex> Santa.Day2.part_one
  47623
  """
  def part_one() do
    Enum.sum(Enum.map(Santa.Day2.Input.input(),
          fn (list) -> Enum.max(list) - Enum.min(list) end))
  end

  @doc """
  Acceptance test
  iex> Santa.Day2.part_two
  312
  """
  def part_two() do
    Enum.sum(Enum.map(Santa.Day2.Input.input(), fn (list) ->
          find_division(list) end))
  end

  defp find_division(list) do
    [{one, two}] = for x <- list, y <- list, rem(x, y) == 0, x != y do
      {x, y}
    end
    round(Enum.max([one, two]) / Enum.min([one, two]))
  end
end

defmodule Santa.Day3 do
  require Integer

  @doc """
  Acceptance test
  iex> Santa.Day3.part_one
  480
  """
  def part_one() do
    find_distance(347991)
  end
  @doc """
  iex> Santa.Day3.find_smallest_square_for(8)
  9

  iex> Santa.Day3.find_smallest_square_for(10)
  25
  """
  def find_smallest_square_for(number) do
    smallest_square = round(Float.ceil(:math.sqrt(number)))
    smallest_odd_square = case Integer.is_odd(smallest_square) do
                            true  -> smallest_square
                            false -> smallest_square + 1
                          end
    round(:math.pow(smallest_odd_square, 2))
  end

  @doc """
  iex> Santa.Day3.find_distance(10)
  3

  iex> Santa.Day3.find_distance(23)
  2

  iex> Santa.Day3.find_distance(12)
  3

  iex> Santa.Day3.find_distance(1024)
  31
  """
  def find_distance(number) do
    bottom_corner = find_smallest_square_for(number)
    side_size = round(:math.sqrt(bottom_corner))
    distance_to_corner = bottom_corner - number
    full_sides = distance_to_corner / (side_size - 1)
    |> Float.floor()
    |> round()
    nearest_corner_distance =
      bottom_corner - full_sides * (side_size - 1) - number
    round((side_size - 1)/2 +
      abs(nearest_corner_distance - (side_size - 1)/2))
  end

  @doc """
  Acceptance test
  iex> Santa.Day3.part_two()
  349975
  """
  def part_two() do
    crawl(347991)
  end

  @doc """
  Function initializes crawler with first element that falls under
  generic case. Special cases are excluded that way.

  iex> Santa.Day3.crawl(24)
  25

  iex> Santa.Day3.crawl(55)
  57

  iex> Santa.Day3.crawl(352)
  362
  """
  def crawl(expected_sum) do
    crawl(7, 7, 6, [3, 2],
      %{1 => 1, 2 => 1, 3 => 2, 4 => 4, 5 => 5, 6 => 10},
      expected_sum)
  end

  def crawl(index, sum_squares, last_4_squares,
    next_squares, dictionary, expected_sum) do
    new_sum =
      case point_type(index, sum_squares, next_squares) do
        :at_corner     -> count_at(index, last_4_squares, dictionary)
        :before_corner -> count_before(index, last_4_squares, dictionary)
        :after_corner  -> count_after(index, last_4_squares, dictionary)
        :rest          -> count_rest(index, last_4_squares, dictionary)
      end
    case new_sum >= expected_sum do
      true -> new_sum
      false ->
        new_dictionary = put(index, new_sum, dictionary)
        {new_index, new_sum_squares,
         new_last_4, new_next} =
          update_data(index, sum_squares, last_4_squares, next_squares)
        crawl(new_index, new_sum_squares, new_last_4,
          new_next, new_dictionary, expected_sum)
    end
  end

  def update_data(index, sum, last_4, next)  do
    case point_type(index, sum, next) do
      :at_corner ->
        {index + 1, get_new_sum(sum, next), last_4 + 2, get_new_next(next)}
      _   ->
        {index + 1, sum, last_4, next}
    end
  end

  def get_new_sum(sum, [a, _]) do
    sum + a
  end

  def get_new_next([a, a]) do
    [a + 1, a]
  end

  def get_new_next([a, _]) do
    [a, a]
  end

  def point_type(index, index, _next) do
    :at_corner
  end

  def point_type(index, sum_squares, [_, _])
  when index + 1 == sum_squares do
    :before_corner
  end

  def point_type(index, sum_squares, [_, b])
  when index + b - 1 == sum_squares do
    :after_corner
  end

  def point_type(_, _, _) do
    :rest
  end

  def count_at(index, last_4_squares, dictionary) do
    get_at(index - 1, dictionary) +
    get_at(index - last_4_squares, dictionary)
  end

  def count_after(index, last_4_squares, dictionary) do
    get_at(index - 1, dictionary) +
    get_at(index - 2, dictionary) +
    get_at(index - last_4_squares + 1, dictionary) +
    get_at(index - last_4_squares + 2, dictionary)
  end

  def count_before(index, last_4_squares, dictionary) do
    get_at(index - 1, dictionary) +
    get_at(index - last_4_squares, dictionary) +
    get_at(index - last_4_squares + 1, dictionary)
  end

  def count_rest(index, last_4_squares, dictionary) do
    get_at(index - 1, dictionary) +
    get_at(index - last_4_squares, dictionary) +
    get_at(index - last_4_squares + 1, dictionary) +
    get_at(index - last_4_squares + 2, dictionary)
  end

  def get_at(index, dictionary) do
    Map.get(dictionary, index, 0)
  end

  def put(index, value, dictionary) do
    Map.put(dictionary, index, value)
  end
end

defmodule Santa.Day4 do

  @doc """
  iex> Santa.Day4.part_one()
  325
  """
  def part_one() do
    lines = String.split(Santa.Day4.Input.input(), "\n")
    Enum.reduce(for line <- lines do
                    String.split(line, " ")
                    |> has_unique_words()
                end, 0, fn (true, acc) -> acc + 1
      (_, acc) -> acc end)
  end

  defp has_unique_words(words) do
    length(Enum.uniq(words)) == length(words)
  end

  @doc """
  iex> Santa.Day4.part_two()
  119
  """
  def part_two() do
    lines = String.split(Santa.Day4.Input.input(), "\n")
    Enum.reduce(for line <- lines do
                    words = sort_words_in_line(line)
                    length(Enum.uniq(words)) == length(words)
                end, 0, fn (true, acc) -> acc + 1
      (_, acc) -> acc end)
  end

  defp sort_words_in_line(line) do
    for word <- String.split(line, " ") do
      word
      |> String.to_charlist()
      |> Enum.sort() end
  end
end

defmodule Santa.Day5 do

  @doc """
  iex> Santa.Day5.part_one
  396086
  """
  def part_one() do
    Santa.Day5.Input.input()
    |> String.split( "\n")
    |> make_map(0, %{})
    |> walk(0, 0, fn (value) -> value + 1 end)
  end

  @doc """
  iex> Santa.Day5.part_two
  28675390
  """
  def part_two() do
    Santa.Day5.Input.input()
    |> String.split( "\n")
    |> make_map(0, %{})
    |> walk(0, 0, fn (value) when value >= 3 -> value - 1
                     (value) -> value + 1 end)
  end

  defp make_map([], _, map) do
    map
  end

  defp make_map([word | rest], index, map) do
    make_map(rest, index + 1, Map.put(map, index, String.to_integer(word)))
  end

  defp walk(map, start, step, next_evaluator) do
    case Map.get(map, start, :not_found) do
      :not_found -> step
      element ->
        map
        |> Map.replace!(start, next_evaluator.(element))
        |> walk(start + element, step + 1, next_evaluator)
    end
  end
end

defmodule Santa.Day6 do
  @doc """
  iex> Santa.Day6.part_one
  12841
  """
  def part_one() do
    {iteration, _} = iterate(Santa.Day6.Input.input(), [], 0)
    iteration
  end

  @doc """
  iex> Santa.Day6.part_two
  8038
  """
  def part_two() do
    {_, cycle_size} = iterate(Santa.Day6.Input.input(), [], 0)
    cycle_size
  end

  @doc """
  iex> Santa.Day6.iterate(%{ 1 => 0, 2 => 2, 3 => 7, 4 => 0}, [], 0)
  {5, 4}
  """
  def iterate(current, cache, iteration) do
    case current in cache do
      true -> {iteration, iteration - length(find_in(cache, current))}
      false -> iterate(make_next(current), [current] ++ cache, iteration + 1)
    end
  end

  def make_next(current) do
    index = find_max(current)
    value = current[index]
    distribute(next_index(index, current), value, Map.put(current, index, 0))
  end

  @doc """
  iex> Santa.Day6.find_max(%{1 => 1, 2 => 0})
  1

  iex> Santa.Day6.find_max(%{1 => 1, 2 => 0, 3 => 1})
  1

  iex> Santa.Day6.find_max(%{1 => 2, 2 => 4, 3 => 1})
  2
  """
  def find_max(data) do
    Map.to_list(data)
    |> Enum.sort()
    |> Enum.reduce(1, fn ({index, value}, acc) ->
      case data[acc] < value do
        true  -> index
        false -> acc
      end
    end)
  end

  def distribute(_, 0, data) do
    data
  end

  def distribute(index, value, data) do
    distribute(next_index(index, data), value - 1,
      Map.put(data, index, data[index] + 1))
  end

  def next_index(index, data) do
    case index == length(Map.keys(data)) do
      true -> 1
      false -> index + 1
    end
  end

  def find_in([element | rest], element) do
    rest
  end

  def find_in([_ | rest], element) do
    find_in(rest, element)
  end
end

defmodule Santa.Day7 do
  @doc """
  iex> Santa.Day7.part_one()
  "xegshds"
  """
  def part_one() do
    {all_keys, all_values} = Santa.Day7.Input.input()
    |> String.split("\n")
    |> Enum.filter(fn (line) -> String.contains?(line, "->") end)
    |> Enum.map(fn (line) -> [key, values] = String.split(line, " -> ")
      [right_key, _] = String.split(key, " ")
      right_values = String.split(values, ", ")
      {right_key, right_values} end)
    |> Enum.reduce({[], []}, fn ({key, values}, {all_keys, all_values}) ->
      {[key] ++ all_keys, values ++ all_values} end)
    Enum.find(all_keys, fn (key) -> not( key in all_values) end)
  end

  @doc """
  iex> Santa.Day7.part_two()
  299
  """
  def part_two() do
    Santa.Day7.Input.input()
    |> String.split("\n")
    |> split_leaves_from_branches({[], []})
    |> check_and_reduce()
  end

  defp split_leaves_from_branches([], output) do
    output
  end

  defp split_leaves_from_branches([line | rest], {leaves, branches}) do
    split_leaves_from_branches(rest,
      case String.contains?(line, "->") do
        true ->
            [key, values] = String.split(line, " -> ")
            {keyword, weight} = extract_keyword_weight(key)
            value_list = String.split(values, ", ")
            {leaves, [{keyword, weight, value_list} | branches]}
        false ->
            {[extract_keyword_weight(line) | leaves], branches}
      end)
  end

  defp extract_keyword_weight(key) do
    [keyword, "(" <> weight] = String.split(key, " ")
    {keyword, {String.to_integer(String.trim_trailing(weight, ")")), 0}}
  end

  defp incorporate_leaves_to_branches({leaves, branches}) do
    {leaves, branches}
    Keyword.keys(leaves)
    |> Enum.reduce(branches, fn (key, dictionary) ->
         Enum.map(dictionary, fn ({branch, weight, list}) ->
           {branch, weight, Enum.map(list,
               fn (^key)  -> List.keyfind(leaves, key, 0)
                  (other) -> other end)} end) end)
  end

  defp check_and_reduce(elements) do
    case incorporate_leaves_to_branches(elements)
    |> check_and_reduce([], []) do
      {:found, list} -> get_answer(list)
      elements -> check_and_reduce(elements)
    end
  end

  defp check_and_reduce([], leaves, branches) do
    {leaves, branches}
  end

  defp check_and_reduce([{name, weight, list} = element | rest], leaves,
    branches) do
    case check(list) do
      :all_same -> check_and_reduce(rest,
                     [reduce(name, weight, list) | leaves],
                     branches)
      :invalid  -> {:found, list}
      :not_done -> check_and_reduce(rest, leaves, [element | branches])
    end
  end

  defp check(list) do
    case Enum.all?(list, fn ({_, _}) -> true
                            (_)      -> false end) do
      true  -> check_same_or_invalid(list)
      false -> :not_done
    end
  end

  def check_same_or_invalid(list) do
    {_, {a, b}} = hd(list)
    case Enum.all?(list, fn({_, {x, y}}) -> x + y == a + b end) do
      true  -> :all_same
      false -> :invalid
    end
  end

  defp reduce(name, {x, y}, list) do
    {_, {a, b}} = hd(list)
    {name, {x, y + length(list) * (a + b)}}
  end

  defp get_answer(list) do
    map = Enum.reduce(list, %{}, fn ({_, {x, y}}, dict) ->
      Map.put(dict, x + y, [{x, y} | Map.get(dict, x + y, [])]) end)
    [one, two] = Map.keys(map)
    {wrong, good} = case length(Map.get(map, one)) do
                      1 -> {one, two}
                      _ -> {two, one}
                    end
    [{wrong_weight, wrong_rest}] = Map.get(map, wrong)
    wrong_weight - (wrong_weight + wrong_rest - good)
  end
end

defmodule Santa.Day8 do
  @doc """
  iex> Santa.Day8.part_one()
  4902
  """
  def part_one() do
    String.split(Santa.Day8.Input.input(), "\n")
    |> Enum.reduce(%{}, fn (line, dictionary) ->
      interpret_command(line, dictionary) end)
      |> Map.drop(["maxmax"])
      |> Map.values()
      |> Enum.sort()
      |> Enum.reverse()
      |> hd()
  end

  @doc """
  iex> Santa.Day8.part_two()
  7037
  """
  def part_two() do
    Map.get(String.split(Santa.Day8.Input.input(), "\n")
    |> Enum.reduce(%{}, fn (line, dictionary) ->
      interpret_command(line, dictionary) end), "maxmax")
  end


  defp interpret_command(line, dictionary) do
    [reg1, command, value1, "if", reg2, equation, value2] =
      String.split(line, " ")
    update_dictionary(dictionary, reg1, command,
      String.to_integer(value1), reg2, equation,
      String.to_integer(value2))
  end

  defp update_dictionary(dictionary, reg1, command, value1,
    reg2, equation, value2) do
    case shall_proceed(dictionary, reg2, equation, value2) do
      false -> dictionary
      true -> calculate(dictionary, reg1, command, value1)
    end
  end

  defp shall_proceed(dictionary, reg, equation, value) do
    reg_value = Map.get(dictionary, reg, 0)
    evaluate(reg_value, equation, value)
  end

  defp evaluate(x, ">", y) do
    x > y
  end

  defp evaluate(x, ">=", y) do
    x >= y
  end

  defp evaluate(x, "==", y) do
    x == y
  end

  defp evaluate(x, "<", y) do
    x < y
  end

  defp evaluate(x, "<=", y) do
    x <= y
  end

  defp evaluate(x, "!=", y) do
    x != y
  end

  defp calculate(dictionary, reg, equation, value) do
    old_value = Map.get(dictionary, reg, 0)
    max = Map.get(dictionary, "maxmax", 0)
    new_value = case equation do
                  "inc" -> old_value + value
                  "dec" -> old_value - value
                end
    dictionary = Map.put(dictionary, reg, new_value)
    case new_value > max do
      true -> Map.put(dictionary, "maxmax",new_value)
      false -> dictionary
    end
  end
end

defmodule Santa.Day9 do
  @doc """
  iex> Santa.Day9.part_one()
  14190
  """
  def part_one() do
    traverse(Santa.Day9.Input.input())
  end

  @doc """
  iex> Santa.Day9.part_two()
  7053
  """
  def part_two() do
    traverse_for_waste(Santa.Day9.Input.input())
  end

  @doc false
  @doc """
  iex> Santa.Day9.traverse('')
  0

  iex> Santa.Day9.traverse('<>')
  0

  iex> Santa.Day9.traverse('<<>')
  0

  iex> Santa.Day9.traverse('<!>>')
  0

  iex> Santa.Day9.traverse('<{o"i!a,<{i<a>')
  0

  iex> Santa.Day9.traverse('{}')
  1

  iex> Santa.Day9.traverse('{{{}}}')
  6

  iex> Santa.Day9.traverse('{{},{}}')
  5

  iex> Santa.Day9.traverse('{{<a!>},{<a!>},{<a!>},{<ab>}}')
  3

  iex> Santa.Day9.traverse('{{<!!>},{<!!>},{<!!>},{<!!>}}')
  9

  iex> Santa.Day9.traverse('{{{},{},{{}}}}')
  16

  iex> Santa.Day9.traverse('{{{},{},{{}}}},{{{},{},{{}}}}')
  32

  iex> Santa.Day9.traverse('{}<!!!>>{}')
  2
  """
  def traverse(list) do
    {depths, _} = traverse(list, 0, false, {0, 0})
    depths
  end

  @doc false
  @doc """
  iex> Santa.Day9.traverse_for_waste('<>')
  0

  iex> Santa.Day9.traverse_for_waste('<random characters>')
  17

  iex> Santa.Day9.traverse_for_waste('<<<<>')
  3

  iex> Santa.Day9.traverse_for_waste('<{!>}>')
  2

  iex> Santa.Day9.traverse_for_waste('<!!>')
  0

  iex> Santa.Day9.traverse_for_waste('<!!!>>')
  0

  iex> Santa.Day9.traverse_for_waste('<{o"i!a,<{i<a>')
  10
  """
  def traverse_for_waste(list) do
    {_, waste} = traverse(list, 0, false, {0, 0})
    waste
  end

  defp traverse([], _, _, sum) do
    sum
  end

  defp traverse([?!, _ | rest], depth, true, sum) do
    traverse(rest, depth, true, sum)
  end

  defp traverse([?< | rest], depth, false, sum) do
    traverse(rest, depth, true, sum)
  end

  defp traverse([?< | rest], depth, in_waste, {sum, waste}) do
    traverse(rest, depth, in_waste, {sum, waste + 1})
  end

  defp traverse([?> | rest], depth, true, sum) do
    traverse(rest, depth, false, sum)
  end

  defp traverse([_ | rest], depth, true, {sum, waste}) do
    traverse(rest, depth, true, {sum, waste + 1})
  end

  defp traverse([?{ | rest], depth, false, sum) do
    traverse(rest, depth + 1, false, sum)
  end

  defp traverse([?} | rest], depth, false, {sum, waste}) do
    traverse(rest, depth - 1, false, {sum + depth, waste})
  end

  defp traverse([?, | rest], depth, in_waste, sum) do
    traverse(rest, depth, in_waste, sum)
  end
end

defmodule Santa.Day10 do
  require Bitwise
  @doc """
  iex> Santa.Day10.part_one()
  23715
  """
  def part_one() do
    list = for a <- 0..255 do a end
    {[one, two | _], _, _} = String.split(Santa.Day10.Input.input(), ",")
    |> Enum.map(fn (x) -> String.to_integer(x) end)
    |> update(list, 0, 0)
    one * two
  end

  @doc """
  iex> Santa.Day10.part_two()
  "541dc3180fd4b72881e39cf925a50253"
  """
  def part_two() do
    compute_hash(Santa.Day10.Input.input())
  end

  @doc false
  @doc """
  iex> Santa.Day10.compute_hash("")
  "a2582a3a0e66e6e86e3812dcb672a272"

  iex> Santa.Day10.compute_hash("AoC 2017")
  "33efeb34ea91902bb2f59c9920caa6cd"

  iex> Santa.Day10.compute_hash("1,2,3")
  "3efbe78a8d82f29979031a4aa0b16a9d"

  iex> Santa.Day10.compute_hash("1,2,4")
  "63960835bcdc130f0b66d7ff4f6a5a8e"
  """
  def compute_hash(input) do
    list = for a <- 0..255 do a end
    {output, _, _} = String.to_charlist(input) ++ [17, 31, 73, 47, 23]
    |> update_x_times(list, 0, 0, 64)

    Enum.chunk_every(output, 16)
    |> Enum.map(fn (list) -> Enum.reduce(list, 0,
       fn (x, a) -> Bitwise.bxor(x, a) end) end)
    |> Enum.reduce("", fn(number, value) ->
      {_, hex} =  String.split_at("0" <> Integer.to_string(number, 16), -2)
      value <> hex end)
    |> String.downcase()
  end

  @doc false
  @doc """
  iex> Santa.Day10.updatetest([3], [0, 1, 2, 3, 4], 0, 0)
  2

  iex> Santa.Day10.updatetest([4], [2, 1, 0, 3, 4], 1, 3)
  12

  iex> Santa.Day10.updatetest([3, 4], [0, 1, 2, 3, 4], 0, 0)
  12

  iex> Santa.Day10.updatetest([1], [4, 3, 0, 1, 2], 2, 3)
  12

  iex> Santa.Day10.updatetest([3, 4, 1, 5], [0, 1, 2, 3, 4], 0, 0)
  12
  """
  def updatetest(sequence, list, step, index) do
    {[one, two | _], _, _} = update(sequence, list, step, index)
    one * two
  end

  defp update([], list, step, index) do
    {list, step, index}
  end

  defp update([current | sequence], list, step, index) do
    {t, f} = Enum.split(list, index)
    {sublist, rest} = Enum.split(f ++ t, current)
    reversed = Enum.reverse(sublist) ++ rest
    {front, tail} = Enum.split(reversed, length(list) - index)
    update(sequence, tail ++ front, step + 1,
      Integer.mod(index + current + step, length(list)))
  end

  defp update_x_times(_sequence, list, seq, index, 0) do
    {list, seq, index}
  end

  defp update_x_times(sequence, list, seq, index, n) do
    {new_list, new_seq, new_index} = update(sequence, list, seq, index)
    update_x_times(sequence, new_list, new_seq, new_index, n - 1)
  end
end

defmodule Santa.Day11 do
  @doc """
  iex> Santa.Day11.part_one()
  685
  """
  def part_one() do
    {result, _} = go_along_route()
    result
  end

  @doc """
  iex> Santa.Day11.part_two()
  1457
  """
  def part_two() do
    {_, result} = go_along_route()
    result
  end

  @doc false
  @doc """
  iex> Santa.Day11.walk_test([:nw], %{})
  1

  iex> Santa.Day11.walk_test([:nw, :nw], %{})
  2

  iex> Santa.Day11.walk_test([:ne, :ne, :sw, :sw], %{})
  0

  iex> Santa.Day11.walk_test([:ne, :ne, :s, :s], %{})
  2
  """
  def walk_test(route, map) do
    elem(walk(route, map), 0)
  end

  defp go_along_route() do
    String.split(Santa.Day11.Input.input(), ",")
    |> Enum.map(fn ("s") -> :s
      ("sw") -> :sw
      ("se") -> :se
      ("n") -> :n
      ("ne") -> :ne
      ("nw") -> :nw end)
      |> walk(%{})
  end

  defp walk(route, distance, furthest \\ 0)
  defp walk([], map, furthest) do
    {reduce(map), furthest}
  end

  defp walk([where | rest], distance, furthest) do
    walk(rest, update(where, distance), update_furthest(distance, furthest))
  end

  defp update_furthest(map, last_max) do
    new_max = reduce(map)
    Enum.max([last_max, new_max])
  end

  defp update(direction, map) do
    Map.put(map, direction, Map.get(map, direction, 0) + 1)
  end

  defp reduce([]) do
    0
  end

  defp reduce([x]) do
    abs(x)
  end

  defp reduce(list) when is_list(list) do
    [a, _ | rest] = list
    |> Enum.map(fn (x) -> abs(x) end)
    |> Enum.sort()
    |> Enum.reverse()
    case rest do
      [] -> a
      [c] -> c + a
    end
  end

  defp reduce(map) do
    {positive, negative} = []
    |> put(map, :ne)
    |> put(map, :nw)
    |> put(map, :s)
    |> Enum.split_with(fn(x) -> x == abs(x) end)
  reduce(positive) + reduce(negative)
  end

  defp put(output, map, direction) do
    value = Map.get(map, direction, 0) - Map.get(map, complementary(direction), 0)
    [value | output]
  end

  defp complementary(:ne) do :sw end
  defp complementary(:nw) do :se end
  defp complementary(:s)  do :n  end
end

defmodule Santa.Day12 do
  @doc """
  iex> Santa.Day12.part_one()
  169
  """
  def part_one() do
    create_map()
    |> find_all_connected(MapSet.new(["0"]), 1)
    |> MapSet.size()
  end

  @doc """
  iex> Santa.Day12.part_two()
  179
  """
  def part_two() do
    create_map()
    |> find_and_remove([])
    |> length()
  end

  defp create_map() do
    String.split(Santa.Day12.Input.input(), "\n")
    |> Enum.reduce(%{}, fn (line, map) ->
      [key, values] = String.split(line, " <-> ")
      Map.put(map, key, String.split(values, ", ")) end)
  end

  defp find_and_remove(map, sets) do
    element = hd(Map.keys(map))
    another_set = find_all_connected(map, MapSet.new([element]), 1)
    new_map = Enum.reduce(MapSet.to_list(another_set), map,
      fn(element, current_map) -> Map.delete(current_map, element) end)
    case length(Map.keys(new_map)) > 0 do
      true  -> find_and_remove(new_map, [another_set | sets])
      false -> [another_set | sets]
    end
  end

  defp find_all_connected(map, all_connected, set_size) do
    new_set = MapSet.new(
      Enum.reduce(MapSet.to_list(all_connected), [],
        fn (key, output) ->
          Map.get(map, key) ++ output end))
    new_all_connected = MapSet.union(all_connected, new_set)
    new_size = MapSet.size(new_all_connected)
    case  new_size > set_size do
      true ->
        find_all_connected(map, new_all_connected, new_size)
      false -> new_all_connected
    end
  end
end

defmodule Santa.Day13 do
  @doc """
  iex> Santa.Day13.part_one()
  788
  """
  def part_one() do
    make_map()
    |> find_catching()
    |> Enum.reduce(0, fn ({a, b}, sum) -> a * b + sum end)

  end

  @doc """
  iex> Santa.Day13.part_two()
  3905748
  """
  def part_two() do
    make_map()
    |> find_free_run()
  end

  @doc false
  @doc """
  iex> Santa.Day13.find_catching(%{1 => 1})
  [{1, 1}]

  iex> Santa.Day13.find_catching(%{1 => 1, 2 => 1})
  [{1, 1}, {2, 1}]

  iex>Santa.Day13.find_catching(%{0 => 3, 1 => 2, 4 => 4, 6 => 4})
  [{0, 3}, {6, 4}]
  """
  def find_catching(map) do
    Map.keys(map)
    |> Enum.sort()
    |> Enum.reduce([], fn (key, output) ->
      value = Map.get(map, key)
        case is_caught(key, value) do
          true -> [{key, value}] ++ output
          false -> output
        end
    end)
    |> Enum.sort()
  end

  defp is_caught(key, value) do
    value == 1
    or key == 0
    or (key >= value and rem(key, 2 * value  - 2) == 0)
  end

  defp find_free_run(map) do
    find_free_run(map, 0)
  end

  defp find_free_run(map, index) do
    found = Map.keys(map)
    |> Enum.all?(fn (key) -> not is_caught(index + key, Map.get(map, key)) end)
    case found do
      true -> index
      false -> find_free_run(map, index + 1)
    end
  end

  defp make_map() do
    Santa.Day13.Input.input()
    |> String.split("\n")
    |> Enum.reduce(%{}, fn(line, map) ->
      [index, depth] = String.split(line, ": ")
      Map.put(map, String.to_integer(index), String.to_integer(depth)) end)
  end
end

defmodule Santa.Day14 do
  require Bitwise

  @doc """
  iex> Santa.Day14.part_one()
  8190
  """
  def part_one() do
    create_hash_grid()
    |> Enum.reduce(0, fn (hex_string, sum) ->
      calculate_hot_bits(hex_string) + sum end)
  end

  @doc """
  iex> Santa.Day14.part_two()
  1134
  """
  def part_two() do
    create_hash_grid()
    |> Enum.map(&make_sets/1)
    |> Enum.map(&(String.to_integer(&1, 2)))
    |> calculate_groups()
    |> count_groups(0)
  end

  defp create_hash_grid() do
    for i <-0..127 do
      make_row(i)
    end
    |> Enum.map(fn (x) -> Santa.Day10.compute_hash(x) end)
  end

  defp input() do
    "ffayrhll"
  end

  defp make_row(index) do
    input() <> "-" <> Integer.to_string(index)
  end

  defp calculate_hot_bits(string) do
    String.graphemes(string)
    |> Enum.reduce(0, fn (hex, sum) ->
      partial = String.to_integer(hex, 16)
      |> Integer.to_string(2)
      |> String.to_charlist()
      |> Enum.reduce(0, fn (?1, sum) -> sum + 1
        (_,  sum) -> sum end)
      partial + sum end)
  end

  defp make_sets(string) do
    String.graphemes(string)
    |> Enum.reduce("", fn (hex, list) ->
      partial = String.to_integer(hex, 16)
      |> Integer.to_string(2)
      |> String.pad_leading(4, "0")
      list <> partial end)
  end

  defp calculate_groups(lines) do
    Enum.reduce(lines, {%{}, 0}, fn (line, {map_set, index}) ->
      {Map.put(map_set, index, get_sets(line)), index + 1} end)
    |> elem(0)
  end

  defp get_sets(number)do
    Integer.to_string(number, 2)
    |> String.pad_leading(128, "0")
    |> String.graphemes()
    |> Enum.map(fn ("0") -> 0
      ("1") -> 1 end)
      |> Enum.reduce({[], 0},
  fn (1, {list, index}) -> {[index | list], index + 1}
    (_, {list, index}) -> {list, index + 1} end)
    |> elem(0)
    |> MapSet.new()
  end

  defp count_groups(map, index) do
    case Map.size(map) do
      0 -> index
      _ ->
        position = get_first(map)
        traverse(map, position)
        |> count_groups(index + 1)
    end
  end

  defp traverse(map, {x, y}) when
  x == -1 or y == -1 or x == 128 or y == 128
  do
    map
  end

  defp traverse(map, {x, y}) do
    set = Map.get(map, y, MapSet.new())
    case MapSet.member?(set, x) do
      false -> map
      true ->
        remove_from_map(map, {x, y})
        |> traverse({x + 1, y})
        |> traverse({x - 1, y})
        |> traverse({x, y + 1})
        |> traverse({x, y - 1})
    end
  end

  defp get_first(map) do
    y = Map.keys(map)
    |> hd()
    x = Map.get(map, y)
    |> MapSet.to_list()
    |> hd()
    {x, y}
  end

  defp remove_from_map(map, {x, y}) do
    row = Map.get(map, y)
    set = MapSet.delete(row, x)
    case MapSet.size(set) do
      0 -> Map.delete(map, y)
      _ -> Map.put(map, y, set)
    end
  end
end

defmodule Santa.Day15 do
  require Bitwise

  @doc """
  iex> Santa.Day15.part_one()
  592
  """
  def part_one() do
    {gen_a, gen_b} = Santa.Day15.Input.input()
    Enum.reduce(1..40_000_000, {gen_a, gen_b, 0},
      fn(_, {a, b, count}) ->
        next_a = get_next(a, 16807)
        next_b = get_next(b, 48271)
        {next_a, next_b, judge(next_a, next_b) + count} end)
    |> elem(2)
  end

  @doc """
  iex> Santa.Day15.part_two()
  320
  """
  def part_two() do
    {gen_a, gen_b} = Santa.Day15.Input.input()
    Enum.reduce(1..5_000_000, {gen_a, gen_b, 0},
      fn(_, {a, b, count}) ->
        next_a = get_next_divisible(a, 16807, 4)
        next_b = get_next_divisible(b, 48271, 8)
        {next_a, next_b, judge(next_a, next_b) + count} end)
    |> elem(2)
  end

  @doc """
  iex> Santa.Day15.get_next(65, 16807)
  1092455

  iex> Santa.Day15.get_next(1092455, 16807)
  1181022009
  """
  def get_next(value, factor) do
    Integer.mod(value * factor, 2147483647)
  end

  @doc """
  iex> Santa.Day15.judge(245556042, 1431495498)
  1
  """
  def judge(gen_a, gen_b) do
    case Bitwise.band(gen_a, 0xFFFF) == Bitwise.band(gen_b, 0xFFFF)
    do
      true  -> 1
      false -> 0
    end
  end

  defp get_next_divisible(value, factor, divisor) do
    next = get_next(value, factor)
    case Bitwise.band(next, (divisor - 1)) do
      0 -> next
      _ -> get_next_divisible(next, factor, divisor)
    end
  end
end

defmodule Santa.Day16 do
  @doc """
  iex> Santa.Day16.part_one
  'pkgnhomelfdibjac'
  """
  def part_one() do
    Santa.Day16.Input.input()
    |> String.split(",")
    |> do_dance(make_programs())
    |> Enum.sort()
    |> Enum.map(fn ({_, letter}) -> letter end)
  end

  @doc """
  iex> Santa.Day16.part_two
  'pogbjfihclkemadn'
  """
  def part_two() do
    dance = Santa.Day16.Input.input()
    |> String.split(",")
    programs = make_programs()
    iterate(1, {%{programs => [0]}, programs, dance})
  end

  def iterate(1_000_000_000, {_, programs, _}) do
    programs
    |> Enum.sort()
    |> Enum.map(fn({_, letter}) -> letter end)
  end

  def iterate(iteration, {cache, programs, dance}) do
    next_programs = do_dance(dance, programs)
    |> Enum.sort()
    found_list = Map.get(cache, next_programs, [])
    next_iteration =
      case found_list do
        [0] -> 1_000_000_000 - Integer.mod(1_000_000_000, iteration)
        _   -> iteration + 1
      end
    iterate(next_iteration,
      {Map.put(cache, next_programs, [iteration] ++ found_list),
       next_programs, dance})
  end

  defp make_programs() do
    for pos <-0..15 do
      {pos, ?a + pos}
    end
  end

  defp do_dance(dance, programs) do
    Enum.reduce(dance, programs,
      fn ("s" <> number, sequence) ->
        make_spin(number, sequence)
      ("x" <> pair, sequence) ->
        swap_positions(String.split(pair, "/"), sequence)
      ("p" <> pair, sequence) ->
        swap_programs(String.split(pair, "/"), sequence) end)
  end

  defp make_spin(number, sequence) do
    n = String.to_integer(number)
    {a, b} = Enum.sort(sequence)
    |> Enum.split(-n)
    b ++ a
    |> Enum.reduce({0, []}, fn ({_, letter}, {index, output}) ->
      {index + 1, [{index, letter} | output]} end)
    |> elem(1)
  end

  defp swap_positions([index_a, index_b], sequence) do
    index_a = String.to_integer(index_a)
    index_b = String.to_integer(index_b)
    {^index_a, program_a} = List.keyfind(sequence, index_a, 0)
    {^index_b, program_b} = List.keyfind(sequence, index_b, 0)
    List.keyreplace(sequence, index_a, 0, {index_a, program_b})
    |> List.keyreplace(index_b, 0, {index_b, program_a})
  end

  defp swap_programs([program_a, program_b], sequence) do
    [program_a] = String.to_charlist(program_a)
    [program_b] = String.to_charlist(program_b)
    {index_a, ^program_a}  = List.keyfind(sequence, program_a, 1)
    {index_b, ^program_b} = List.keyfind(sequence, program_b, 1)
    List.keyreplace(sequence, index_a, 0, {index_a, program_b})
    |> List.keyreplace(index_b, 0, {index_b, program_a})
  end
end

defmodule Santa.Day17 do
  @doc """
  iex> Santa.Day17.part_one
  596
  """
  def part_one() do
    1..2017
    |> Enum.reduce([0], fn(n, list) -> iterate(list, 377, n) end)
    |> tl()
    |> hd()
  end

  @doc """
  iex> Santa.Day17.part_two
  39051595
  """
  def part_two() do
    1..50_000_000
    |> Enum.reduce({0, 0}, fn(n, {current, last}) ->
      next = next_insertion_index(n, current, 377)
    case next do
      1 -> {next, n}
      _ -> {next, last}
    end end)
    |> elem(1)
  end

  @doc false
  @doc """
  iex> Santa.Day17.next_insertion_index(1, 0, 3)
  1

  iex> Santa.Day17.next_insertion_index(2, 1, 3)
  1

  iex> Santa.Day17.next_insertion_index(3, 1, 3)
  2

  iex> Santa.Day17.next_insertion_index(4, 2, 3)
  2

  iex> Santa.Day17.next_insertion_index(9, 6, 3)
  1
  """
  def next_insertion_index(length, current, modulo) do
    move = Integer.mod(modulo, length)
    Integer.mod(move + current, length) + 1
  end

  @doc false
  @doc """
  iex> Santa.Day17.iterate([0], 3, 1)
  [1, 0]

  iex> Santa.Day17.iterate([1, 0], 3, 2)
  [2, 1, 0]

  iex> Santa.Day17.iterate([2, 1, 0], 3, 3)
  [3, 1, 0, 2]

  iex> Santa.Day17.iterate([3, 1, 0, 2], 3, 4)
  [4, 3, 1, 0, 2]

  iex> Santa.Day17.iterate([4, 3, 1, 0, 2], 3, 5)
  [5, 2, 4, 3, 1, 0]

  iex> Santa.Day17.iterate([8, 6, 1, 0, 5, 7, 2, 4, 3], 3, 9)
  [9, 5, 7, 2, 4, 3, 8, 6, 1, 0]
  """
  def iterate(list, step, number) do
    {a, b} = Enum.split(list, Integer.mod(step + 1, length(list)))
    [number] ++ b ++ a
  end
end

defmodule Santa.Day18 do
  @doc """
  iex> Santa.Day18.part_one
  4601
  """
  def part_one() do
    get_program()
    |> execute(0, %{"snd_fn" => &sound_fun/3, "rcv_fn" => &recover_fun/3})
  end

  @doc """
  iex> Santa.Day18.part_two
  6858
  """
  def part_two() do
    registers = %{"snd_fn" => &send_fun/3, "rcv_fn" => &receive_fun/3}
    program = get_program()
    p0 = Task.async(fn() ->
      receive do
        pid ->
          execute(program, 0, Map.put(registers, "p", 0)
          |> Map.put(:other, pid))
      end
    end)
    p1 = Task.async(fn() ->
      receive do
        pid ->
          execute(program, 0, Map.put(registers, "p", 1)
          |> Map.put(:other, pid))
      end
    end)
    send(p0.pid, p1.pid)
    send(p1.pid, p0.pid)
    [{^p0, {:ok, _count_p0}}, {^p1, {:ok, count_p1}}] =
      Task.yield_many([p0, p1])
    count_p1
  end

  defp get_program() do
    Santa.Day18.Input.input()
    |> String.split("\n")
    |> Enum.reduce({0, %{}}, fn (line, {index, commands}) ->
      {index + 1, Map.put(commands, index, line)} end)
    |> elem(1)
  end

  defp execute(program, line, registers) do
    case execute_command(program[line], line, registers) do
      {:deadlock, result} -> result
      {new_line, new_registers} -> execute(program, new_line, new_registers)
    end
  end

  defp execute_command("set " <> details, line, registers) do
    [register, value] = String.split(details, " ")
    {line + 1, Map.put(registers, register, get_value(registers, value))}
  end

  defp execute_command("add " <> details, line, registers) do
    [register, value] = String.split(details, " ")
    {line + 1, Map.put(registers, register, get_value(registers, register) +
       get_value(registers, value))}
  end

  defp execute_command("mul " <> details, line, registers) do
    [register, value] = String.split(details, " ")
    {line + 1, Map.put(registers, register, get_value(registers, register) *
       get_value(registers, value))}
  end

  defp execute_command("mod " <> details, line, registers) do
    [register, value] = String.split(details, " ")
    {line + 1, Map.put(registers, register,
       Integer.mod(get_value(registers, register),
         get_value(registers, value)))}
  end

  defp execute_command("jgz " <> details, line, registers) do
    [register, value] = String.split(details, " ")
    guard =  get_value(registers, register)
    jump =  get_value(registers, value)
    case guard > 0 do
      false -> {line + 1, registers}
      true  -> {line + jump, registers}
    end
  end

  defp execute_command("snd " <> details, line, registers) do
    snd_fn = Map.get(registers, "snd_fn")
    snd_fn.(details, line, registers)
  end

  defp execute_command("rcv " <> details, line, registers) do
    rcv_fn = Map.get(registers, "rcv_fn")
    rcv_fn.(details, line, registers)
  end

  defp sound_fun(details, line, registers) do
    {line + 1,
     Map.put(registers, "sound", get_value(registers, details))}
  end

  defp recover_fun(details, line, registers) do
    reg = get_value(registers, details)
    case reg > 0 do
      true  -> {:deadlock, Map.get(registers, "sound")}
      false -> {line + 1, registers}
    end
  end

  defp get_value(registers, key) do
    Map.get(registers, key, string_to_integer(key))
  end

  defp string_to_integer(value) do
    case String.starts_with?(value,
          ["-", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]) do
      true  -> String.to_integer(value)
      false -> 0
    end
  end

  defp send_fun(value, line, registers) do
    other_pid = Map.get(registers, :other)
    send(other_pid, get_value(registers, value))
    {line + 1,
     Map.put(registers, :counter, Map.get(registers, :counter, 0) + 1)}
  end

  defp receive_fun(register, line, registers) do
    receive do
      value -> {line + 1, Map.put(registers, register, value)}
    after
      500 -> {:deadlock, Map.get(registers, :counter, 0)}
    end
  end
end
