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
    {result, _} = String.split(Santa.Day11.Input.input(), ",")
    |> Enum.map(fn ("s") -> :s
      ("sw") -> :sw
      ("se") -> :se
      ("n") -> :n
      ("ne") -> :ne
      ("nw") -> :nw end)
      |> walk(%{})
    result
  end

  @doc """
  iex> Santa.Day11.part_two()
  1457
  """
  def part_two() do
    {_, result} = String.split(Santa.Day11.Input.input(), ",")
    |> Enum.map(fn ("s") -> :s
      ("sw") -> :sw
      ("se") -> :se
      ("n") -> :n
      ("ne") -> :ne
      ("nw") -> :nw end)
      |> walk(%{})
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

  def walk(route, distance, furthest \\ 0)
  def walk([], map, furthest) do
    {reduce(map), furthest}
  end

  def walk([where | rest], distance, furthest) do
    walk(rest, update(where, distance), update_furthest(distance, furthest))
  end

  def update_furthest(map, last_max) do
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

defmodule Santa.Day1.Input do
  @doc false
  def input() do
    '5255443714755555317777152441826784321918285999594221531636242944998363716119294845838579943562543247239969555791772392681567883449837982119239536325341263524415397123824358467891963762948723327774545715851542429832119179139914471523515332247317441719184556891362179267368325486642376685657759623876854958721636574219871249645773738597751429959437466876166273755524873351452951411628479352522367714269718514838933283861425982562854845471512652555633922878128558926123935941858532446378815929573452775348599693982834699757734714187831337546474515678577158721751921562145591166634279699299418269158557557996583881642468274618196335267342897498486869925262896125146867124596587989531495891646681528259624674792728146526849711139146268799436334618974547539561587581268886449291817335232859391493839167111246376493191985145848531829344198536568987996894226585837348372958959535969651573516542581144462536574953764413723147957237298324458181291167587791714172674717898567269547766636143732438694473231473258452166457194797819423528139157452148236943283374193561963393846385622218535952591588353565319432285579711881559343544515461962846879685879431767963975654347569385354482226341261768547328749947163864645168428953445396361398873536434931823635522467754782422557998262858297563862492652464526366171218276176258582444923497181776129436396397333976215976731542182878979389362297155819461685361676414725597335759976285597713332688275241271664658286868697167515329811831234324698345159949135474463624749624626518247831448143876183133814263977611564339865466321244399177464822649611969896344874381978986453566979762911155931362394192663943526834148596342268321563885255765614418141828934971927998994739769141789185165461976425151855846739959338649499379657223196885539386154935586794548365861759354865453211721551776997576289811595654171672259129335243531518228282393326395241242185795828261319215164262237957743232558971289145639852148197184265766291885259847236646615935963759631145338159257538114359781854685695429348428884248972177278361353814766653996675994784195827214295462389532422825696456457332417366426619555'
  end
end

defmodule Santa.Day2.Input do
  @doc false
  def input() do
    [[5806, 6444, 1281, 38, 267, 1835, 223, 4912, 5995, 230, 4395,
      2986, 6048, 4719, 216, 1201],
     [74, 127, 226, 84, 174, 280, 94, 159, 198, 305, 124, 106, 205,
      99, 177, 294],
     [1332, 52, 54, 655, 56, 170, 843, 707, 1273, 1163, 89, 23, 43,
      1300, 1383, 1229],
     [5653, 236, 1944, 3807, 5356, 246, 222, 1999, 4872, 206, 5265,
      5397, 5220, 5538, 286, 917],
     [3512, 3132, 2826, 3664, 2814, 549, 3408, 3384, 142, 120, 160,
      114, 1395, 2074, 1816, 2357],
     [100, 2000, 112, 103, 2122, 113, 92, 522, 1650, 929, 1281, 2286,
      2259, 1068, 1089, 651],
     [646, 490, 297, 60, 424, 234, 48, 491, 245, 523, 229, 189, 174,
      627, 441, 598],
     [2321, 555, 2413, 2378, 157, 27, 194, 2512, 117, 140, 2287, 277,
      2635, 1374, 1496, 1698],
     [101, 1177, 104, 89, 542, 2033, 1724, 1197, 474, 1041, 1803,
      770, 87, 1869, 1183, 553],
     [1393, 92, 105, 1395, 1000, 85, 391, 1360, 1529, 1367, 1063,
      688, 642, 102, 999, 638],
     [4627, 223, 188, 5529, 2406, 4980, 2384, 2024, 4610, 279, 249,
      2331, 4660, 4350, 3264, 242],
     [769, 779, 502, 75, 1105, 53, 55, 931, 1056, 1195, 65, 292, 1234,
      1164, 678, 1032],
     [2554, 75, 4406, 484, 2285, 226, 5666, 245, 4972, 3739, 5185,
      1543, 230, 236, 3621, 5387],
     [826, 4028, 4274, 163, 5303, 4610, 145, 5779, 157, 4994, 5053,
      186, 5060, 3082, 2186, 4882],
     [588, 345, 67, 286, 743, 54, 802, 776, 29, 44, 107, 63, 303, 372,
      41, 810],
     [128, 2088, 3422, 111, 3312, 740, 3024, 1946, 920, 131, 112, 477,
      3386, 2392, 1108, 2741]]
  end
end

defmodule Santa.Day4.Input do
  @doc false
  def input() do
    "nyot babgr babgr kqtu kqtu kzshonp ylyk psqk
iix ewj rojvbkk phrij iix zuajnk tadv givslju ewj bda
isjur jppvano vctnpjp ngwzdq pxqfrk mnxxes zqwgnd giqh
ojufqke gpd olzirc jfao cjfh rcivvw pqqpudp
ilgomox extiffg ylbd nqxhk lsi isl nrho yom
feauv scstmie qgbod enpltx jrhlxet qps lejrtxh
wlrxtdo tlwdxor ezg ztp uze xtmw neuga aojrixu zpt
wchrl pzibt nvcae wceb
rdwytj kxuyet bqnzlv nyntjan dyrpsn zhi kbxlj ivo
dab mwiz bapjpz jbzppa
hbcudl tsfvtc zlqgpuk xoxbuh whmo atsxt pzkivuo wsa gjoevr hbcudl
gxhqamx dradmqo gxhqamx gxhqamx
yvwykx uhto ten wkvxyy wdbw
kzc ndzatgb rlxrk hfgorm qwgdky ndzatgb rhvyene qaa wxibe qwmku nmxkjqo
qwx ubca dxudny oxagv wqrv lhzsl qmsgv dxs awbquc akelgma
rrdlfpk ohoszz qiznasf awchv qnvse
ggsyj czcrdn oolj sibjzp ibzjps asp
vbcs ypgzae xcvpsr ptvb leoxqlq zmpt fhawu yfi tvbp
ejkr qlmag nsz jwpurli nhsml asksnug mes
kkgkjml kklmgjk kjgpx iquytbj eccceb mfv iuyqjbt ovv
uoklkco zzey sdfhiyv ytdeg
azr mjv raz arz rdfb
pir dafgsah dafgsah kndjbml estcz yjeoijp kkcws ebq puwno
iqymwc tac vlqc bmnkz xustm leqi
gwdjed cfha axz xjuq
abfjsg pahat qlj zan qsfn iozfys jnvu bis jakggq
afwuejn zrbu zurb hrn lwvjb jnwixla aufejnw
vkqn cuzf humhriz webnf uzfc zfuc
eznxd kgbfy jqyc net vzfege tprzyc
mqnapzn vrgw ilzp vgw
aie zkkih fhpwu bbn fhpwu wvxxgmd
ksoasrn yll mvdjxdo wydymx dmodvjx drnjlm tcjpjhj xzakb wrsbuwl vaygdwf rsasonk
qahbh tfhkl apdqqpm tfhkl nsox
xkelwve mvdmesj xrto tgku gkb bpe
nni nyylpu cyusxe zydeyok yokzdye xiscesy
itwsfr eqwrx igqkvif whklwdb
lpa hwci suwqfln xis sfht lzek ajecd
svpf eulute eya gvmsd app claria tjtk zjt agdyemi bixewo
gmzglxi zlgouy bejg kte xlf giquj mjeq ivjkw ktbhaga hoffyrt
wwjy dtf ftd agei yde xhbfo fyridy
gexcy hetkz ufflrfi frifluf plb kqre etxo elg henqy fspm
khaemn buec ichau wxctsxg
cgmv ujyvcuu jta yux ccx skrafkn cmyc yidqhv ltb ycnajry zitq
ybsahqn pio veeze vdztjz iedou pio sue ijbz gvqncl vpa ijbz
hkfi xzrsyke hikf mxolx xlxmo ungfc tst xjzd
tpx ioprco qixlv ipocro
oahmwrv homvraw vws ntmbdvx
fxlg wnuz ogt bxgtul vmfh nwuz glfx tgxdq bxfv kajuh
vrhqn nrqvh tgogb vya ragbro ulrz uava kexoi yav vkfe
bxxy tyxgxd oabsud bauosd jlch bdmrqq wqhjwb ayblb hclj
sfzsgsc sfzsgsc jbrvh sfzsgsc bdhy
wixleal vhnqbfw qwfnhbv woco oowc
exkkwz wekxzk krxbua nshxqgh
gkn blxgui nkg gnk
otsa isqn otsa isqn
ude xedl ude xedl amkktp
teroe yuvbd inf mpytuvz xiq xqi ovqetn
zyq ybeifwx fvoqp vhoduy bcq wbxl
zymiid vafcqv vjbmekf lgxkma bjti qfavcv iqp fnbu lakmgx
rkaqvd vylkh jfdxh imxxg bbrt imxxg rkaqvd
yajg qnhhs bzmb eyk hijcg tkij iwr jvwp dipzd jvwp
btzhw zttheyo ravsbz bmbba majoe ykrs tbxqf tai cgsvpu srbavz
vsyczfs ymg vsyczfs wxlwaqb oouio owek wxlwaqb azvbqiq
ghrapd ghrapd wisq wisq
znmleu aztnkbs wxc gycxd vqenhh geqyo rpjg
kxbom gzz zzg zgz
dfsesc okwb dfsesc okwb
egpwqbe djlk xpkxa hoo eepbqwg
nxdfror yfhkhn zgea fkspva rjgg bnmq ddsf rjgg gkinm
vdrxfom wbdwu dhkt xtvzc zjobo aqvgrt
svddsgz mhcrbcp wmpd mhcrbcp klim ddlnxv wrdftrc ddow wrdftrc
obxr wscs new brxo wen epns cvjvxts ilnc
rwezl vmbut kgblt xfg vnhlebq nzqdzxm ynh wokrezy zub nzzqxdm
vephajp bzupele mltzglh sbgn vephajp lhve mltzglh
slajp kyaf vlnvy srfietn ayfk inaufex fanuexi
vazwg kjg qanzso ptuu vvlwq uupt kohhql jkg
xmmmpky rbqimi slvxsf tlcwm pbf pks iucx rbmiqi
irkup jvu tkeioz avdu suxamf
tmgih ldca jswka dblzzt rap rgqyy gyrqsk nnnn pok
pdbjhrl gsvxbqr nqfkhtc ngn okbgzd pdbjhrl oostjtm okbgzd
mzqfdat dujh aeplzqh acbguic vlzdt amyszu amyszu jqecky bhl hjqnimq xoy
dszafr bqampg epozj sfrlpe dszafr wfurku sotjpg wagtnxy
jbmo jbmo plbfkvw bkc jbmo
ehelldu vrid unnf vrid xqiu tbibjyi bmbpsmq tpqyefx xqiu
rpgm zzbj cjgfdyb bdjfgcy rzqecd miyvfbu aqlkagf hsuxwgl
gvywzp phvnd ypwypbm yfelxx egbr lcfyz hecdhkj xxfley
tsmebin tbsnmie mkijj ijjmk
cghxrqs vzxle wrfghv skighgt zviteab plcrgv
ezdirp rxkw horcek qcgny inx nikb tigzp
eidk sactjci sre vkapava owvf eyds eyds
vvjdm uye tjixj azklizl pnb
tcrimv xye twii xye twii tad
mtxcg lwjxdj zjudqu ekoujd ysf ajtfta dkj lwjxdj
aowhmvv kkic kjize fnohl ukx remfmii usbp
wkossu limxmhp xnoeocb wkossu lnrlqf kjozfg xeulstx sjncsw ekaimuv xnoeocb sxjegcg
lsfe zpewzlc yhjyiay lou ukhi lpwezzc slef zvtidgg kdeseq enka tfvgudr
ovfsa vuv tbtorv tbtorv gmxn opspw lli mfzvkv zlyhr oznalr
kugrpw sduq rdc ciaxwir ylnzwec kugrpw sduq
obevuau thu jpyfvof rpawwo obevuau gsvoutr quiaei
xpgua pbxa pxgau kdan
ohyzqk abxgg xozgai nib axozig bni fucgykm jpkswt
jrgu dmozts jrug ufpho
qojzue uzeojq txuhj eqjzou
wcvj qwlravl niyxf oiaptlk wlxnnzj jgdzap jgdzap lfgn bdt sfga adrypo
ylah eedu rvwdpmq eedu ylah
quages kmla yjqua dzxcfam srjag wujmcv qujya ssaol uzdwi
gdsppz yqxlish yfkjbbf ecnzu ejvtv cdjwre
slsls pcmrq zax btrc kliv ntho gymkk kkq pcrmq mvnw sjfegpx
ryz jfw eki wvibww qdzylg whbagp ffrfjg wdhnqpm hcrz
tcjqfh tmvzp mpztv vpmzt
xood xutgof teqov fqyyub oakm rzaheiq
axagoq jawbz sexucp sexucp atenr edekcwn edekcwn agl ecj gbje gipivfq
poqv qopv bos flhghs gshlfh
rxd dzphnb bwmna vxd rxd sbk kuor
kqeelq jqbyh xczqzqe jbkmx kelqeq xqcfqn
jdfy qzjyz xvqyo jdfy xvqyo
vyoqyd pwayqag eygmdt smakwc veftikz fzeikvt
aozgkne mpd mktgoew eepp zlwycr eepp hswbxcx nmi ddnfr eepp
dgpfp cfhhqdx vjrb uyimbm byx hfdhxqc
fxq jcouwfy uhuao zsab xjao
noudveu egxyuqw hmnnv vovt wmqkx syatiac whkd
gxyzk opgb kjxp delavq hsnvk kfn irkcfq lvc aadcwy opgb
exuiupk ddiiyvm nsggpj ddiiyvm nsggpj
hhjby rfejzp akxzs nconlt rynivtq voloj qwhhll ubvx yxuacz miuwxh ppe
uspqvx supvxq cekv niddfuw
optzcag sra ajs ozacptg yxkludq jjpvldz mxo mox
dko qyec iuxbrbj dlz jxribub
ywlyz vipfh amsfr fwiozi tahjov tap rsea zwlyy oqdyfbo
xeqml jwy eguc bvzvh
crp mxsihvo wwtg gsypx mxsihvo qpfw looca gewvy zjqki tdkuxo crp
mqlnzm yihsvrl hhtwcv kigymqu yswki hksk vbiujq xeqz irzcq cpnz
zxhfsw uuyhwid nzabem mmfk wszfhx shxzwf hqnrvsq
hfjajdl qwmk hjdalfj mwkq gqbku dsszk
fbiy pujq htgaqqq yro ztpe yiufb fnpi ggchdgz
sixq jsboan eoie msdrwzw sixq njsrc sixq yimqoi
pbxgv kqmi hjuk bbtrlta bqwm bgehofj ainqhm qoypsil manhiq ogebhfj lvmuo
wnax aen fthpcke tcz yadjmva mydavaj rcfkc krfcc
lkatiw zxliii usdj oopxl yylv bkjfy gtlyjv usdj muqazdb
yqonaxv wqnvoo hfpll oyxnlfs fgajc khhtzr hfpll gsvvipz wbjxsnp dcdikt hqw
vvuv kspmnz zvmryqd egvuz eazkhz kspmnz
xgq dziwiym gsl nbzmsta ccbzn yalof dbbugq aar ywmbvk yofla dcwb
qrtyhhw xeyo vlym ulzzbl hrxyb qeyu jqdkewk oxye evaxz kybc bssyt
eqrf cfyy kwhohw ozg jsc egz jsc
vct cet ixxvmz ibhvndq eks dpi jzfwdqv saeh jqzdfwv vwfdqjz
vus vus kitvvgq wpi alfncf gzj oxcy fith oxcy ecbsr
uacculk guwhwdp cankcv yswy bmby sve dvonm nran
ydftm wszgaex rgbw otd dbet lhsxndd jqfyx
vhawg hwagv uagy fveik nrsew zujw hawvg dzfmt agzgw
uqdj talb uqdj aizyuqm
pbbejee szdtohv tycfow xwne qzlqy dxcwejz pqdqrc wfyotc gdqt uxaeug wtldm
hmzmd oyp pyo opy
qwdh kwpll kwpll zsbez uxg klr uxg
myqr zza kqpcos adsql eumunrv qlaeumx
acyye xvdewe nwkhuz bzcmx asw ostiwk mfzu nwkhuz
memq uqadd kfj dses lrxb hxygp dsse bxbr hgpxy uavrar
mjmk lsdttuz qjkg yfthmkn pram
pctfq aly usim shihap uims xkfgp ksrbn ifvsyl
cdma nnnu hdm dhm
kpt upgsm ohvrrqf qwps wjcbve ohvrrqf
wowphgb nteme otizypb eumgvb puoctli opicult wbohwpg
fppz ftkql sbut lkqtf svif viqhlnn buts lljhbd
oqk uinby rqy vbjhf oul hzfo coca glpy brjy yglp qnvhvei
sbbwr dnyrux gpikv nsx aawyeq uhtucwq rhxzy jgx bdgdrl dnyrux lgfgi
agn mljz hgmglem popu jtapub agn
ehfpgr bnobvg bnobvg bnobvg
ozgzedn godezzn art atr
urz rzu xzyc rjhwi kgiodi doiigk layr dwbxu
rkcbav pnp bpsmm ifivfe csouqpw fyswzbd csouqpw bnjt rnnoxed
hpjgtcc ctcpgjh cchjtgp lxn
cinokbx uyaz uyaz uyaz
bphfwad bphfwad bphfwad yml izlhlb dvjvo jeropar
ocgftcl wshjk zbinw fcotlgc xdj nwibz
zbze hllno rmq invd gupoxr gwumc vnzj fcvvhjo dnn sfsxw
oqlhkz hgf yxiahks vhzvl ayshkxi irmwkmq
apeqic ahwu abxjrd tuwrd pynnil eohmlgo lafx ybpofe wbznxv swuafas
cpg jpsfo jposf rer ixeydpz
rhqrwvn wrhqnrv xptms jhc rnqvhwr
zfpl tukxzda lifkqtd ynfuno cttx ctxt tlqdkfi ueswv wbipfbe
eblw bwbjg fuu qqm qtv qtv isbl denfcb
ick yqwcffk pvcchd apkjyc ouu uyfe nplid ick caqscs sddkx
rtzh idn snnw xmlou idn kdhenl rtzh ujwttl pkynkhe
dnwha fpv dnwha iqi xggepo dnwha
yjvk saay enxqhw wigoah dzasyr nnt artl iqwia jpp xmfr hwigao
ryt heenuai ytr gqew hyb byh wdurx kmd adgjz
ypdqeji sfkkfhn stms cdmyh nqllx utiphia gxbx zflhtgo yurztx eni
pwlhlt lhlwpt rfkvlgr tucajej ckujc ntcyae xestygt eshmggk
gtfb codwc vjtli ffmjwx ruoekt cylrm ktroue dfaxzvs kkgejzi ewucgu jyatrum
ersbag cod xssha aqzbe kxu bzghhqk pbs bizvqk bhbbd bps
vhci ypxf bxzor unngl xilpzpk civh nykora vchi
cyb cceu negnld nbcfs pxsjgh xah nbcfs nbcfs jabpxg wtanv qhztvr
cljgzkn lrdeina hrjoz kdgpn vqkmpal nivk scvnu vzuausp
nif fin uxjbip xxztsn yyo opueh zxs qnso paedey hsd fttvqdn
gbnkmpr afo aof ryyudy gbmpnrk
uaa npb dkit npb buadan esv npb hwrj
hws dfgq fcyty qszhu chyxxl ytmpb azxl jrsn boqrx
hkzlnkd fkilvog xbubu fbgbp
fgi inmay uliytc vgkcw qsoe uliytc isjhix oyir ocaq
qrzkpm dpzetbr zommsxo cixg nwjyvp bet wyjpvn cgxi tsncd
uvlf lufv ulfv cigl
uwwf thr kdq fhjmty bvxue vcwwmk kdq nzajq bxkf
qcwduju idxaja qcwduju idxaja
fnarz pstzfne nco qzf kcevoo qwx csvsxga pstzfne
twug xrwy uoctfl bkh yxrw
unpdnbe apf cvm bpullu fyels tjpri jyw unpdnbe xfyekay vhk zpyb
rbv psirdv psirdv mnjrp qpwc vicismd qpwc
zjj zjj kesyhow eqcfqy vqy
zazpd gmea aobl dcs mage hqjdpwc bvxr srw
rhcdb nzsa jgcgc rhcdb wxs vsvvptn zvckqo wxs
unyet prchn fiwter wvgknes dvzbxfs ufet neuyt fczlrx bpocdci vdsfzbx
znfev fwrdarx knqkv ojiv ojiv fwrdarx
tbtlo hdashg kyspxm ypmkxs nmrk
fzr zqxaszt frz xzrre
shueb iraetk uhsv duvah uhsv zstysc nrfllbc emrknka
vzkrmp mgtkjnw njr bwjgp jdwyyhv yudha wbvmx ewu urhiioq
yjq xxr swvm aipdj apjid tfsq gfqg izrvhev
iljuqt fpo fxadit iljuqt iljuqt
zrj poewso vsje bsprbmc vsje yfwf ybu dmkqib ybu hlrpdi ymh
apxaeq bgdm mqty whyay mnuzfgk awm bgdm mwwi ekw bgdm
dpdbfkm crrg mkph kphm grcr ukbk
ilqm wroz mqil qlim
pnlx nwadw uabelu rueamxr wjer uwge jwer ywagrx
akuil nkh oitq werli werli
fkmhcr ieoj xfsa xfacoeb tcg poomcme vck zmpc djcqgkf kft
csyk qni hqfrye zyyxz ggynzad pjpokmu bigqa qie
lkpenw zyllii qtbvdq zqnu ichftg xazped agl irhlbiy snlwfe twliar
acsrba dzz ivylbl rfcdd rfcdd qcg
zbui fomvpx zjhmgl sivtffu xuhswzt fzeid tgj mzok mozk afbhuje tzswxuh
nupjiat fdxkbn tuatp jhdfnub yitdk yghqw nupjiat ibi edfv tuixw auwjm
focht mnprh tljj ayp
pjdnl uaoworh iqm gic dqlu spn heuymio
kvg ferrvhp unvzsd qdcpd rji zpch
nhvay chuzg pyhdd hnmrnq zeidhf pyhdd ohy hnmrnq
boa sau gxh grx
gwo utwpd zcsrx gow bnm
xoqniyd hmithl xoqniyd hmithl
yqqsbzo stca zcsjnqf skbueeu tlbkef mvqbg igzr wujuz yqqsbzo kkfe
wgzuepu wge fkrxuag csenx tkngoz wge azueyxs
get xiryxs xiryxs xiryxs
wammvx edy hahetl xmvawm dye
lscxxgi anmax quo cqprwn imocarq gnbfhe rcnqpw
znpmid yaluvzn ydm ckh uhso rrk wbby lwxsu
atppk byf dzz uift nqejgm njgeqm
dtqmy iog ahub habu
hkthdwt pfxlwsu hkthdwt hkthdwt
tsuiue tsuiue yais tsuiue
swooqmp rqrcs ngr vujrq inuu rqrcs
dhu zxdfiyv xuz xuz mgaty mgaty
kiiiz zco qdv vfgkj rders zco
trszp havbm redpeqk gktp ifvzvwl yfoxnm tzg avzd otiouso eks lqlutwb
cfiru lpdy kpeas mdc lxnjjqz nqyyb xkjsug rcifu dln
jga ijgkjo qhbnupb ofzqn iokjjg gaj lrh pkynrcr jgatk
bexwc tat tat otsngaa
feh mjxbs ehf cyfhlv vvdgdu hef
njlvq ojwaes awsejo ktyvxd qeyeze bpoaj ulgngn zyeqee kqc bsdzzvq
hbfp vnhs vnhs pko pxnxgm
bmy bzpn bzpn bcfep
cju nqjy yjqn bbrj esgzw swgl bjrb
cxvrshm rbglkyv kqwzcyd azqr ckwbbew fhgqv nfk lactzh ssqpwbr wbewbck
ptcb gqkb apcc okl jbbgk qni bqu slydyo qhh dqd osv
zbisefn bmxcljk bmxcljk arkamus vpq uxuwvb
ksik xbzk lahh ctfur sxh rduokr xqou zwbgqsp skik
hwhmfk hwhmfk bjpxzg qqftmu ijyv igvayf bjpxzg
askxqew tibx pqaczy fhzyec echzfy cezfhy
omzyy mbzfvsn kkoff qgqn crnnkn krx oqp jhn anb qte qxt
jypnwn vjbnbsl axf pldxbq pdoy rmxcvig cpad yhah rzqewkg nmzkkr erjo
visidzp bujlfn xuomjj mjnqn wgflg skb
oer oer lfi zyqnem lfi guljz
fannhwu wafma gcje cvcia qwyh ugtbpa geufqg
kwtjib pqwai tdmjj kuxr euzl rxuk
ovi splc hflutgw hflutgw
gvel gelv aeiygth elvg twwr kivxrrj jkmqa
bas ylxbdgn yliv pytkhq haujsyf fggrnbc wsgree rfnppcx key gvdzgfy evdtrrz
oblab wpgm bpyy xuroy qhb adqko
hneb law uzms fhhk yjymdx wjla ixfh yblh
qlvsd bxsq hjaq fuwspzu hyshq idbabc rqcih ilixp wft rglf lmqm
qdskj two ckd qdt hzjvd woo fmmuw
kumc zywzq srafcbb ihfu kfvav
qlkkrq qlkkrq qlkkrq qsc
hob bpecik zqtrfz iqizeu plrer epm zqtrfz xrekeql xrekeql
warszd sxyyorh sxyyorh eztjf warszd kszp
hjbrax liumjue liumjue liumjue
rfnqd folmiu dlicln pdyk uqd rfnqd
mjdu lytfvya xomdujn leaqiyc lgemz lihfnhv zgeml koukz luqda
yqsz zedjmwn aep qwbhd yqsz
etg rmovps abizj yqr kib
yznxec sfqkd ofkzep njr hmeym nsh xdq
ryoyq heoo zuo udvfev ehoo axcnbpu oeho mfenmd shrebzy
uaeh jwllsjp frkhqsy uaeh
giofw hwceb euikqp ldmb kqpkxwv namazcg hqyyzgs cglsqux
qledbd qledbd kbwo wgfmgp
olbsca muxw nxs locsba
gbxxgj xlzm gws pkpwy ofkxb sykhdo nbhrv
najr bfk tbqkm hxabe nvr mdi dmuujr bfil nyripr zcydzy
kiczhcn dfgylw yzkwk nytijj pceu yukj ekaol xpb uep
acyyxn rwczsud acyyxn payiek inusyb rwczsud
mzssokx bshs bshs ocrvlug nzsgvch riejkrd jkj mpmdgsp kvixdfq msmmx
uaxy wpvhf uaaq ranp vfhwp iik kii nvh
shecxef nqpx jly dzm qvmpu kxg hdg
xembm yzevult ljrllc yrlskyk zas wstnz yrlskyk vasra
yoaxppi kzax hvxfezf mek teo cbtlrfa ncxac yee
dzfpbi cynov dje vxypba wcwww cwnu cqtp cnuw wwwcw rkzas
xzwdt jcwv anb xzwdt
fodgjem fmmrsfl eovsneo etzutda paw fmmrsfl jcqql
yfztt alcw nwdmd afgknu njxkj zykz cvv jbnl han iatmruu trqls
yas hpulrmf dzts sltg qsbw fjj rjymnnx dkkv
hwjtgd abmb cfw xoumxn xnoumx cxo xnxmuo alb
hnl zgdiip lrddhl fyw mporhtp waedf dltdfmc lyipoth ubmg hnl
wxard wxard cibp nzquvb muuslvw igvewfh mika wxard
cjqjhva rrhzy qpdc nqnyd enbdee ewrhp cqdp xekgjai
axtmxb axtmxb phl urdqaar urdqaar
umce jult bkart dgdvdwc kqzlzn nqkzlz umlxx cmue xvehqag wxifal
lwsuc ski ubo ksi sik qwcudv
husdv tssr gfp bfzbrp jtmk svvdpb uvshd zbnpdmj svpdvb
nnbvf xbb dobqk xwloqca uxvqti blcwxpu kubwu nognin goywn
xhe dhddftc ggltd dhddftc wspf
jodq cgvnk lpl wkwwlqd prfby bpyfr tbgyqm
bdebxj cuvow jdwdxw kuzh dvxmsyb dyvcxo psf kjnoe odfwgfa
xpfb knzgfsi thmsnbi ymjxn bevohy xpfb
hphcu fjodpdt mfsp jkvvp jvypar nlud lfv uftupcr nul dunl
olz ihyhw qntr lwcbohv qntr wzralwl
kfz pkjhidy msnmwz exox xexo uakipj mmznws zbbji ozispqb
gfi kwdhx qqo kdxwh fig
ehh rfozwr caoisw qntlk pkv zulc kpv hrqz
exmlrj aacc rzb qie rzb
mxyqe cuqz feyd meqyx gdvpu rqyjtvw dmoo vugdp emem
advj xmnad uvh ufnbi xmnad xmnad zzwjksx chbrjas hrbp ruvyg
nasrghk pmol ryko ofgakhd korf vpy nakrsgh
mylyqg aeizp rnk krlwchk aaqg
edxursp sosyv zesgnpx zlo sly alurdc ypmez qib aqtt lmxd
ihm hwzhd jhiw raocjk nlxce yzuzu nhudri tvygl tmclg mdkz
psubdis qrmxebg kdac xvl raxwfx vlx sxme
tci tphdy tggam vqqiyjz sgfvdri sxhztz fhsmxx yaj ncxcxq tic
xkljs cuhrm fdjqwd fuzyzh dzuzgjd lzpye lzpey
jriwl ypkcxd fxrg eit okzzzsc yaykarm qzuv jurgek dzfbbfl
workf rrw absfl gxluw qprdsz absfl qwqbmi amepvz oiqmy workf
dxyyb brnerbx lykd oqmz ursl zqom
cqtuzva aih uhaswd auhwds ktyvc hufogcg
jre fhlgrse svedc prfspaj ghm qcjzfc nsd
fow xyo vlvg sgg jgzvff rjxh eovre xtupnz
pekj pgiecc igxd zbiqoob ovv
xofxmz rdzdiq yruoqkh arfunx yruoqkh ucm bxov
ctogwj lpv ivtoxkf faj ctogwj xfzluad ctogwj vvw
rmc vjxj strgo tykifpp
ulivozu bczond ywnmt shakc yknr psr
bfx alwedh jfomlf pzj tely alwedh vccsoer rgwftcl vccsoer
frkwbv uudwt qsfg onuhiml jrd usu
bgdx deybefo gdj dgbx luu cbuwawd wqqtq dqmwy gin mhtfgy
ohjp ykemg nrs leayrh brtipx jhop phoj
utaep ywsy utaep ywsy
qow dxagjwb qbki bqik
larkpq bdgw mly vvwgv
juar zaerof qekpe hhgd eygru epekq dhgh
xpblz xksc lzue xksc yid nnve trlndn gjczngs cifqoaf
fpv ekz eknldf uqjgeu awwnwxu eknldf eknldf txhxv
mzvk wqtbda ovdbh vnes uiuuc uicuu bpwwtm aaat cygej nio gnl
rkdkzp bjaxqif xuwx bjaxqif hgtz slkqw rkdkzp ztp xfvgk ycvg
zpwr wvxzfcd opgcrfc ytxeboe rcqa ehrga lmgm
brsdnk nqgkjab nbjkaqg gho zqe
szbysu oqrtbp wjpuv oqrtbp oqrtbp gjmqq
uoyi ctscw uoyi ggn ija
fop lxa cgwpw lyvrxbe tit fop fop kfigqnu
ldqmk rxo ajhrbc ahrcjb xqdk kdxq
ith vdrl kvaxktm grkzmon ith ywbz kmnoiz
zdoo omjo fbz dveiipw fbz
ivj mcnu tkijlq xkq lrkyit cumn sfkrk numc ezxeeoi
lcwzdi sbsdgdy olvc olvc bimubzf bimubzf
cdjd umhwh djdc cddj oxheq veazlm
gxszn zsgxn azy yaz
byvmj mjybv jvxkuy akas uxyjvk
whmkttq whgzm gwmzh pkvtljw zgmhw jasudeq
yyjri fxsj xffmna vbal ftff rwq uszym bznil rfuctp ejndv wqr
gnwzjbw dezfvq gzkhzkl ivrdvxx wfah xvivrxd qzdvfe
xnfo zqzn iaod zlcclsd onxf lpskrfk nzqz kqzr kffpwak eky
muc tafbzp nra gvzc xiu gvzc
gfnbnyj nyjbfgn eoosw yjzf
qwwls sqwwl mxph swwql
twor uzjftq twro orwt
qomjuob bqaim zvfqww cvqzm wwipc zsywb bsqkp aoj fus
nlyd gtbgox tajlzgs bgtgxo pqt
pjtmgz ulblj ussh gngagba hhtexq bjbj obe xctciay osriw obe shxri
agc ejjdtak jgq moj agc iua syhxih znavmrc iih qubj
zxwzwhm lipkqhz bbv birxsj gzg iefrjh mprsfs ofpltbl gbo srpmsf hirm
rbpgqoe kymrf uzsut gkbtd xctpg qul hirtfl
wfvg pnqhuv jayjm ftqt mbrotl aydmoc lfwlxk vpvcsi svbn bnsv
jxjxza ysl kls vmt fvgunx hketl oshgie
dfeyxv akx qagwayp qrs lnulrle rqs gbvd bvdg
aac ndptml oke edwrg aac xechxz
mpx yrb oervzb ydvkw avlt oervzb bxdqbo hzwls
dsynfk dsynfk epexzjd epexzjd zofb
vhe zxfolqk lkh fxt flzkxqo lztwkmo khl
izlthi wtokkuz ousbpxp pvr uuxueq lvbeff mfk syjq fwgnfmg yytqesm gdd
kjcg slt khz atzw twpspdx kgyk wgq hjat ntf xvhxol msvdjs
ymm arrggw mmmbvrs ist arrggw nbvvc cwyacp
kuzglex iemp iemp jsko iemp oqs dheqypr
tzztq dsxqbow qgaeo kqn dsxqbow qqzpv
ysr fctpiyn psgb gatavv zsfxoxq nynfoh qaimoj zotjk nxug syr
xvm qvr hdxyhpf cbo xmv lfv wltyjlx
hjq pohc xgqit tducggu zdqmnc xqgit tqxgi srfyzu vdikqx
msiqte ewvp bzrv cmuy gse qqayvb bzrv qehy
watdvu ametrc etlduhh vcc luehdth udavtw
jktj mkq jktj mkq
uekth ufjkmdi qzhge wzwcwk nvrodcc vrcdocn bhcvd
xumywk zwofh kuxmyw acgzsjj hfowz njnz bnklyi
hmm fexu fexu hmm
zeuoarc yoa ggff jazzd mjein yhj qwo qwo
rolkwf fcyat lwm wqqm juwkt wqqm udj tex xgps nyy pdbkkhb
gld ksl gld bnsuhqc gld rwmybj
tvyxk xgmk cri pef epf unsl yktxv
muiql ejq taetjkf ejq xzmo wmv qbtmrh hkfbch taetjkf sut
pqg icvv gpq tufd iixd duft
zekx ybbb gzml vrbwcl opfb fkrv tto cbipr
moh stkkf ynrtdf jlgb kstfk ksktf
nvysvf mdtdoq bqqvr bqqvr
dqyz mzoqtp gzhdgd symsq iwh bpwox
pkqi jgzsrah yfjxx kdp xjaf lbj gkpixnj tyvzzso qmjbo skg nlchzbk
culxfx jarwu eiwriu vwvg gvwv sgnasz
kyfsn dwc sbnoe xwpgjh nbmvec dwc qjdh mpw gefimue fvqjwt kkor
hcdcgxs fof flc hfpjy lii fihcao pxg xywei jwsq yxr
oxrcv pda oxrcv gdvojsz kmlga mixlmp hdcabsn qvoa fwt
poe joylchz humrjy cyxbqfm lyk ybrfmp qmtpqyk vtpr lyk vtpr
ffswqs yxbuj tfzkmc yxbuj giog ckubbfy rtigw rtigw rpitxd
kcvrn eejyftw ejytfew rnckv
lvk lkv cooumh vlk
loypv ukowl loypv nyoyfl vehnm uff
tst sei zovy itdwibj mcbtst wcf rzp xvbtax ffzp xieenuy aegkj
zkhi hvsbgza xbwtdns wypfngy lvabd pybhcd crczm buikdpo vqgon pynfwyg phbcdy
ihy irxrj entmc yxfhbta xsdv xsdv
ezrcv kfgm pjneez puccy gzpxdlf gkfm yucpc mli xezfug
umjppkq idkiri wmnbhi unl nteyw wmnbhi zyv idkiri shhcrau
dzj zveqwae ljnikvb baavr dhsohp zveqwae goq zveqwae
xhc xch bmttdr snd jakd
jmgnvda bdpzfw dfwpzb pimpv blqtbyo lzdzo bgrlfy anmjvdg
lwvu ksg gqbtibd ksg lwvu ohfzlt foajo apyrcwj uaro
vel qksrwp zei ipnvd hdua rkspqw bujf
iozkiu upa knmcug zidypn yswb zswkvx naqsu
tjktoe dqpt pbqi dqpt
lcl tui uoizm xrdpmwi fbsuuqq tgeac hpajm tegac nczlic
ntmm mskzb arem ntmm jayzfe wyurgsh eqwcqt edhska asxhjv jayzfe
jyq juifidx fokzxh cgo xofhzk nhro xyccuq ioa nwk nqaxpfw
cvag bpk cuo ocu buehhq tartafi ifs qwh cveurg
bwut xpfni qzg cmp cid jftawv twiszmo
zgxc sui kypkd vpam ymxicrw jcfbutd fgx jcfbutd
tkxn rjqzljh tkxn mdwcho
qbv zneocv zneocv zneocv
tywf soncr lyepx qzj xdsr pdqv swt
ulu rdk iomqu dgouoba icax
ddsc oxilqpd ddsc atbekg ouzmxf oxilqpd kwtzz yhmyd otvi
vtj llnfrpc vfighju urosrsz vurtse llnfrpc qeuo vfighju nnn smsnp tfom
updfjmz ngtgi zaitq rqqhcyn ladzx zaitq fbaphyz hipe
rii fpos atl tal qhubqdv lat
whxzwdj yznkngr eefbmub wnxitd tnwxid zja ziewilm xylwn ihhsha lrptuyf
fhmzaxv mdn udl gyv pqw qlrz flm rqtji
bgn clnm cnml qyh hhf qqnur sgvigvm
qjtbysc ycbqjts gbgvlz vgzlgb dgxks qbvp grji dcc
wmduuq qayymzo zvh ylbipw sin ybwpli ilypwb
qsvzktt qsvzktt dasmg knh gcgep qai
jxukj qlgr cjssj aavqv
xpxa glsdfxq ngxwon ytuue pizqu
fxl vegoed tct luwm ulwm eeovdg
ntmpe auasx vkwgi cryuiix dmiufo fcb ldl jauncf gyouym asjcryc
lgwdcs eoxm hcrpnuf pcfnhru vlye fpurcnh uquukv vjc
lfns riwpdh phwxvew hhu jfptvv ofxd hkotgfq
qvuwnq wnpvs xdivrfz yaenqr fipwgl
vhcexfd bishqsc gsbruxm yzccyot yjloa aptg vbr gsbruxm ihqhyz yzccyot
knfst zhihi swhhq zhihi
qfto abhjx abhjx bpnijn ogmqxn rclqag dmeb rdogx emfriui hyvp ogmqxn
ivaemm wlsc dvjv aivemm xvf shfonv
vowhosr vptlu ucrut rdynh ttqvhg rdynh abtja pnvdy puxfmf dyhd
uvrenol ycuhvy ygm fjsxiwo oftstid ygm
fix qrqeg dfgvlun fix iraxgtt lhgqdo eqkgshd jwmrm qrsbzba
mxdj icjqzqw fvew gtvlhm mxdj
cyjtkm crb pmg jwo iluc brc ttnd
dasmgp ool ool opc
ubi pmz mtkh ibu hlx ipcvjki sydw zpm eewfdeu oga
avex yjaoghv yjaoghv lwwx
kwkdst iuokd nmpw onayet zlavwnd wwvbr jtrkyku wfxx dumydgh gnd zgi
ahyjnc rjakp bhabq tsmfi ahyjnc tsmfi yitqgi uwnywil shnkbn
krr sbbfjtm yvunas hwppsjf ntuuzw ngyvdmt ynk nfq mfrb pyw hngr
eeecesf phoo ijmx sjp kgmtg sjp wyz
qwixmou oximqwu ixu lsmf
dyrzq lbstdjv ldvowml qjf fqj zpabc dwmvoll jnq
pdtlu hgcfvz mnwjyq ymi cvcp kmx mkx ooffp uiwg opoff uevqt
hflomt fhlmto gutdbyp xyi zpggxc wqe
jpsr wwex yjgdj fqah wrmmw nyrnw hcomcgv teajmu emw zrraid
tvgsca bzgzkga ypsxsk dqz exmu tvgsca dqz qnd
arzn hojpi bznw ejuupe bznw hojpi
rids dule qaefaon sspit mtzgdls cfujw xldhimi igdoy dule
nefsys plea obksngc zxqee avsi obksngc vnsxdrl gspadob avsi owmzpeh tcj
oweq fkr krf rfk ztwjdry shzcmew jhna
hdjizhg dfclic usds luz mcwyj luz qvomls mren otax
pmzzfj pmzzfj wfxyq mqv hyp lhf
dxeaw ckkey ccvawo keaf izlh oacvcw lgcpgeh kdiky
xkwe xekw kwex tzfyx
dmmyt mtdnqw pdw vdav ofrtsk
klz zlk snxnihg snhigxn zkynpd
ijzce xobf uojezxi xiuojez
ztepv zvpet nije aditjlg natkkk dtitg jprgia
fesuh wadrhc bayf kktfaf nxvhq smbdaop gqx ioez fkjufb abyf
hej sta pztkcd pesabzz szp iada iada cdae hej sqst luf
xlnuhn oljaf fljao ascxez fojal
dprclb fzn wgauz rxewtp cjrlgz zfn
fidwoa mvoqy afian ntzokap mkplgy jfukgjv cyfsz
hbvqnnt giinuzq uezugy qooxjc zsxr rnihg ipbels
qroi wtltjq suj tqit bxtc jidzhpe nizp wtltjq nadcdm wwyhjrg
qtr fkbl bpptu baen awjpwsg vvqbxz animt uqbk zvbxvq
nznq fdiul jbv umyrf yufrm hrl duilf
bkvlfuw onkqzeo iwrg rifqzhj mgroul rnor qqqc sbfi hny zosfp kopxb
nvifbx jbowbj fnyskt jbowbj xvun xvun
piyl haajm stwzpp xvjg amjah
gye efwwwiv kyv zmtcgmi ifwvwew
dflx gdtb jyoj jyoj dflx aqhycgi xffnn
inc mpys mzqmcwx vryz ibqrzc pmsy fat rojpxwy rcbqzi gjef"
  end
end

defmodule Santa.Day5.Input do
  @doc false
  def input() do
    "2
0
-1
0
-2
-2
0
-5
-6
1
-4
-3
1
-2
-4
-9
-4
-5
-10
-10
-17
-8
-17
-17
-10
-18
-12
-21
-1
-22
-4
-24
-2
-16
-8
-6
-33
1
-2
-5
-35
-20
-26
-9
-15
-14
-33
-24
-8
2
-13
-25
-45
-44
-32
-50
-27
-56
-56
-16
-26
-8
-13
-41
-48
-59
-59
-34
-7
-56
-8
-49
-11
-16
-25
-11
-4
-31
-22
-12
-68
-7
-13
0
-12
-8
-59
-73
-68
-78
-46
-37
-87
-63
-61
-81
-83
-72
-1
-36
-75
-33
-31
-76
-25
-15
-15
-74
-74
-37
-46
-82
-110
-66
-60
-104
-61
-55
0
-109
-16
-63
-118
-109
-25
-96
-124
-7
-20
-36
-68
-38
-71
-106
-7
-14
-96
-117
-109
-107
-112
-46
-116
-128
-18
-87
-20
-121
-121
0
-115
-89
0
-12
-124
-24
-18
-124
-44
-99
-10
-63
-103
-45
-54
-99
-101
-96
0
-50
-143
-18
-55
-78
-31
-40
-62
-176
-137
-5
-135
-36
-52
-97
-29
-16
-12
-108
-64
-29
-65
-128
-113
-21
-135
-126
-163
-51
-61
-177
-52
-26
-151
-142
-98
-203
-178
-175
-81
-110
-201
-150
-104
-117
-165
-4
-96
-9
-118
-123
-156
-39
-122
-59
-223
-9
-14
-53
-191
-23
-171
-7
-131
-2
-225
-20
-66
-225
-203
-41
-122
-159
-222
-96
-134
-191
-105
-78
-139
-225
-164
-109
-178
-186
-24
-161
-125
-214
-199
-152
-172
-102
-217
-15
-60
-148
-173
-10
-258
-119
-9
-11
-6
-53
-220
-57
-123
-62
-172
-80
-200
-236
-255
-61
-199
-165
-277
-32
-169
-108
-86
-137
2
-96
-204
-269
-181
-96
-166
-271
-235
-75
-245
-6
-95
-130
-303
-55
-227
-266
-82
-298
-135
-272
-311
-311
-86
-198
-41
-147
-169
-91
-65
-122
-293
-149
-218
-325
-292
-136
-119
-100
-290
-48
-257
-282
-199
-67
-262
-75
-274
-136
-87
-123
-61
-258
-117
-93
-113
-250
-225
-250
-135
-342
-304
-153
-114
-335
-327
-276
-119
-35
-75
-239
-140
-199
-354
-94
-51
-176
-35
-121
2
-51
-80
-116
-69
-282
-180
-10
-103
-172
-332
-158
-95
-365
-358
-69
-263
-75
-74
-1
-236
-28
-311
-364
-275
-333
-52
-376
-156
-310
-276
-160
-218
-341
-78
-184
-14
-291
-279
-248
0
-199
-252
-221
-23
-371
-39
-355
0
-211
-151
-34
-209
-310
-215
-76
-398
-21
-206
-339
-147
-47
-70
-105
-340
-128
-321
-101
-225
-425
-248
-117
-437
-379
-196
-262
-236
-365
-217
-213
-70
-406
-414
-425
-332
-13
-120
-238
-238
-110
-224
-116
-414
-419
-309
-121
-104
-9
-406
-82
-72
-249
-432
-380
-14
-388
-403
-249
-349
-480
-118
-243
-285
1
-126
-33
-99
-241
-151
-329
-64
-63
-120
-75
-255
-118
-177
-138
-451
-306
-245
-33
-55
-90
-212
-301
-168
-363
-314
-417
-385
-52
-497
-131
-71
-134
-69
-138
-141
-347
-358
-505
-294
-288
-432
-234
-439
-112
-260
-31
-273
-86
-362
-237
-369
-442
-402
-453
-30
-171
-33
-170
-20
-539
-93
-456
-240
-10
-344
-150
-262
-417
-421
-44
-15
-51
-69
-268
-290
-344
-196
-556
-519
-242
-197
-546
-99
-377
-225
-113
-138
-120
-263
-170
-452
-405
-376
-551
-190
-9
-224
-110
-183
-454
-212
-55
-264
-348
-510
-152
-71
-356
-424
-233
-234
-10
-524
-76
-251
-456
-578
-348
-176
-406
-173
-160
-603
-129
-558
-106
-535
-465
-310
-569
-302
-504
0
-175
-59
-485
-586
-88
-136
-524
-164
-55
-150
-191
-40
-552
-386
-217
-119
-250
-448
-357
-485
-623
-412
-202
-266
-55
-256
-169
-327
-165
-101
-550
-434
-176
-152
-518
-624
-374
-305
-370
-304
-362
-109
-277
-53
-318
-574
-338
-276
-620
-306
-499
-77
-122
-329
-156
-654
-576
-79
-498
-9
-15
-445
-415
-491
-641
-104
-600
-126
-156
0
-70
-271
-162
0
-640
-505
-261
0
-476
-555
-130
-423
-360
-138
-457
-674
-303
-624
-513
-566
-405
-406
-632
-177
-66
-277
-445
-117
-628
-452
-156
-535
-254
-44
-466
-142
-414
-473
-201
-85
-336
-660
-607
-376
-367
-582
-619
-499
-89
-232
-708
-731
-215
-72
-159
-278
-53
-257
-104
-7
-745
-403
-534
-17
-520
-510
-353
-504
-578
-729
-139
-271
-181
-134
-475
-251
-490
-205
-675
-112
-753
-256
-485
-234
-654
-403
-450
-658
-172
-675
-54
-169
-230
-1
-436
-108
-660
-289
-367
-314
-68
-572
-586
-369
-145
-543
-756
-78
-490
-785
-180
2
-424
-159
-727
-235
-645
-28
-282
-489
-250
-71
-682
-711
-398
-55
-328
-182
-166
-743
-388
-334
-432
-729
-360
-471
-16
-602
-709
-69
-269
-791
-489
-322
-646
-301
-159
-268
-777
-456
-746
-356
-809
-728
-90
-302
-386
-728
-656
-29
-101
-728
-82
-406
-675
-62
-160
-617
-125
-698
-120
-655
-751
-284
-475
-16
-420
-574
-638
-38
-573
-623
-747
-115
-430
-591
-832
-422
-40
-212
-564
-71
-513
-623
-503
-189
-256
-728
-655
-580
-508
-382
-883
-885
-480
-117
-407
-253
-635
-244
-101
-847
-857
-205
-119
-756
-302
-169
-277
-521
-586
-472
-880
-580
-825
-428
-248
-846
-226
-621
-413
-302
-161
-177
-601
-830
-275
-700
-922
0
-897
-764
-151
-343
-10
-601
-772
-574
-478
-791
-244
-4
-927
-601
-686
-109
-786
-224
-409
-725
-411
-692
-886
-667
-767
-773
-623
-868
-319
-739
-36
-309
-634
-240
-790
-711
-426
-426
-860
-227
-677
-67
-567
-621
-714
-430
-914
-335
-601
-205
-475
-737
-60
-28
-19
-221
-660
-386
-67
-474
-49
-488
-207
-209
-178
-889
-370
-180
-232
-709
-861
-359
-372
-591
-80
-709
-340
-441
-116
-156
-525
-687
-604
-31
-303
-425
-650
-642
-606
-626
-332
-26
-126
-846
-672
-74
-201
-256
-660
-342
-184
-1008
-196
-757
-522
-293
-241
-662
-655
-617
-810
-943
-295
-506
-904
-1038
-966
-259
-1025
-448
-597
-210
-45
-533
-48
-943
-618
1
-697
-1049
-607
-668
-164
-751
-722
-940
-381
-194
-671
-336
-738
-712
-390
-352
-35
-470
-737
-715
-663
-549
-598
-707
-135
-277
-470
-225
-742
-1010
-10
-134
0
-724
-811
-758
-869
-1029
-567
-889"
  end
end

defmodule Santa.Day6.Input do
  @doc false
  def input() do
    %{1 => 4, 2 => 10, 3 => 4, 4 => 1, 5 => 8, 6 => 4, 7 => 9, 8 => 14,
     9 => 5, 10 => 1, 11 => 14, 12 => 15, 13 => 0, 14 => 15, 15 => 3, 16 => 5}
  end
end

defmodule Santa.Day7.Input do
  def input() do
    "uwzmqi (57)
emlzcpy (106) -> pwmoihf, sdwnkb
oevnzwt (90)
imjhra (23)
uuimn (299)
xzpov (55)
kynja (73)
kggcnt (43)
qdxpkx (67)
wubltb (276) -> udcok, pihpjbp, hiifqwb
anifo (10)
zxdpmwl (57)
tkdmopp (44)
kdsbbcf (193) -> huyizg, evofb
pxfpf (235)
mjftixu (131) -> kwzsj, rspilzk, amtxw
dbtoizo (73)
fdqmg (124) -> kggcnt, dzekyo
egvfxip (23)
mpijek (63)
kkoitqr (70)
jgwvyp (29) -> hoagrj, swefql
guipj (25) -> sauwet, graedp, jdvya, bmlwb, eidmza, kmnmx
ayuhvb (216) -> bemhni, gvpac
unmxfe (91)
miwfyq (96)
loofhh (31)
liczn (12)
urvunmn (63) -> ddsmt, oablvt
jxdqhu (60)
pvqfa (174) -> pftmymi, vmrdwe
xrabl (82) -> ivyarjy, lhxinfd
rfvtfs (93)
qkrirn (129) -> ceztn, tqjmaxi, lwroef
epnny (85) -> xltlyvw, ctkjip
hlzyncx (85)
fyxdfod (82)
bawgh (7)
sxpqvwv (358) -> oziwd, rreoxyc, qntqokn, acibw
nogbp (119) -> psvkj, djyks
mgdhu (15) -> cfcbhku, fltdrlc
nsvan (1749) -> ahracx, dvoul, ecxayrz
gefrwix (99) -> sovfoi, vdqpuon, ghtdnrx
hfrtlx (57)
kyhace (12)
gbcfg (277) -> pzbgmxx, lysysqu, urvunmn, yiohbo, hqjlgre
uhwsavi (57)
fyynsgp (6)
wqobawc (67)
agjdl (61)
tkureky (247)
czalivu (28)
wykyl (64)
wqtbny (49)
wnhkac (18)
fuhbamv (81)
wqudy (238) -> dpkdpj, jmmticn, kpotsk, qtntw
latvu (35)
inpjzmh (23)
orawnkw (195) -> bsqlkiy, fhhzvwj
kwzsj (40)
ymfwoto (50)
kckucu (152) -> mnoaa, infll
fwlme (6)
xlyxjyx (302) -> lewizr, duxfof, uqazvo, mzbwym
aksgboa (93)
rxxqpi (920) -> gwwphk, xaskpny
gqqgsz (49)
rlbdave (27)
fuunnks (42) -> cstnc, dyxmvo, kwtszff, ittpbzy
bhcwhrf (88)
ppigbow (84)
bbzhhk (70) -> bjrfinh, lissl
hsael (60)
tcpdvk (33)
yvbgs (286) -> byaaoab, kebefwy, wpxxz
utjtx (8) -> rzlkr, ntfvgoj, qmvpgee
kdcyull (48)
ldakuz (45)
lcfpcst (92)
ufnpup (102) -> cewmc, uwzmqi
eavfy (87)
lufgbo (60)
sncjicb (102) -> xslucv, jbpvtf, gfltww
lydjsu (65)
aclnw (210)
fqdtzfy (221) -> zwfly, bidasc, mjfhodb
wavbwew (80)
sqwfq (59)
yeicqcy (46)
chzrjqp (90)
fhabfp (59)
wbijkw (19) -> zzdbn, pqxef, ikzqhu, dyqgl
gbhlxb (75)
zpimcy (83)
zmjzmyq (35)
pagsex (45)
onsjs (197)
gqiczm (61) -> gwsceo, ucocdl, hdqow, fjqccm, oxvqjeh
hmdqubz (16) -> juzol, txote, kynja
lzikkl (330) -> cdrjh, lwbcn
haadpw (65)
eeluucp (62) -> pxsmani, qdmop
nnsogp (60)
nezhs (94)
fqrwf (88) -> hujzcku, tkdmopp, ckgmjsg
zoszls (70)
uzlgko (56) -> loyjp, wwnkoz
uqazvo (39)
sphktgz (12)
sikpe (48)
rsjmc (53)
htyvy (17)
ylluriq (129) -> pawmnl, uhmnltu
mrsxl (18)
gbkubnt (45)
xdvwjb (70)
jguhhsb (9)
mfyivv (84)
etsxvmj (896) -> enlfaa, ymnichj, tuefd, zhkmnxk, vcllnn
cougztw (27)
ewifyk (25) -> sxino, vgwxuan
vwtxaci (78) -> nnkpuhp, haadpw
vwqof (64)
cewmc (57)
haztq (48)
wxyhyh (43)
dyxmvo (107) -> tnham, jsdvowc
zhcmvvm (90)
zdrwxqt (89)
bluacvc (26)
yxfysz (418) -> cfmcrf, ixbsb, kafyh, zouqjoo
hiifqwb (14)
knsphtv (74)
xrwnzgz (21)
xtbqb (82) -> bydmdru, awfnh
zngystc (140) -> gszlkf, inkfmx
gtktonk (92)
ukkph (17)
fzowmer (22)
gumpsin (34)
rynlaj (292)
wjhgb (48) -> icoic, hwhgi
ndvns (24)
cawzw (81)
kyphnel (51)
uawyee (22)
mzftrlb (70)
xfxsxh (71)
wzlonwk (65)
inkfmx (30)
vkkds (207) -> pvhdt, fshoj, dsuonma, vuazmuj
wdwgd (80)
licby (79)
ytlpe (77)
usddqi (170) -> fgokr, mdznc, psynxr
ychaa (47)
ztuswk (7)
rtchnfd (162) -> eptve, lejuo
bymrwvc (67)
xqusi (22) -> jdbmary, flrdpaj, iwysxj, tooduon
gyivxga (26) -> cldaf, lqgvx
eptve (37)
aflaj (35)
ghnldf (35)
oldfs (34)
bkdxl (85)
qsiqu (114) -> efvvtgo, hcfgf, fgayln, xzkzoq, isuevmr, xqyyni, rjfqyz
abjrozg (189) -> dlcep, rhwfl
fjemwxf (69) -> ofxdo, jjkns, mwvihj, vrhke
zuapfco (40)
tqcwn (50)
ouycck (1843) -> lmliqa, txtnweq
hkelse (71)
wifvw (68) -> cpoxc, btlczl, dqzsblu
cxfvbz (807) -> vazst, evquf, pvcpmx
quxtc (142) -> tcpdvk, npwrqsm
fltdrlc (90)
fivaym (45)
jdvro (43) -> wkkqsy, wdwgd
qinwdk (178) -> liueudh, zgtsnwk
zautz (48)
flmeo (338) -> auocy, uqkhzxt
zziuivt (78)
iooqbyk (1558) -> fuhbamv, dqrsxx
vumxuur (99)
awfnh (60)
woeabie (18)
xtqnalt (19)
xedlv (67)
jatrdb (229) -> njnyoek, ctvhcv, qsgjk
axjkbq (188) -> hgoruv, iimyku
hmenje (183) -> npefyxi, fwoyu, acueoez
zxwmd (96)
zapbrn (421) -> szzhp, jsvwb
tmwvxgi (179) -> egvfxip, gohxocn, wgvlq
fkagr (32)
wppgjkw (187) -> hlmdgp, usglf
wajnp (54)
hbgeak (92)
rnbvott (157) -> kaekzo, hbozym
haurusk (170) -> kxgrtg, ythbw
ylpvre (48)
zmuit (131) -> vrzrsz, uawyee, jbxllu
mnoaa (59)
zttpfsd (20)
solmej (193) -> wnhkac, gkjafru, cmfwb
lkzdum (33)
vhpjrh (64)
sqwejs (47)
xxufma (17)
eshjt (10)
vrzrsz (22)
pvhdt (23)
dtqyhj (96)
htmrm (85)
tbsmz (70)
hezjirm (54)
cozhzk (62) -> fkagr, dqyaco
jcrmny (70)
zpadu (96)
lfszf (50)
vpnfkw (80) -> ewzvs, jbitbzb
rphgr (895) -> gkfymj, rtchnfd, gtyfx, ujdhkyw
bmlwb (481)
mzaxe (52) -> itzntrm, wqtbny, jyhddhe, zgatapc
qxzhq (180) -> ezjqb, kiceppo, zkiotrm, gqqgsz
bkfxvy (1563) -> bkvuz, dbtoizo
utaxgw (57) -> bupugpv, gnqxls, rnvenxi
vcuizl (119) -> maklk, bxyvuvc
ydbzo (57)
lhagpts (737) -> utjtx, bcyipmu, vcuizl
dkbycez (131) -> vagyecy, fvzfdr
kdyuwjh (92)
udcok (14)
xssia (80)
hrzvmel (34)
dupdsj (81) -> hgsiz, yeicqcy, fncsm
gseohkk (29)
dllffsl (54)
ixbsb (104) -> gjbikl, cogziqp, gdibw
pqxpguz (22) -> tnpzf, zaxds, jvocm, eeluucp, zenvqu, ytqwzv
abfkpur (651) -> ppeumu, yuvpdk, kzimilx, wbwyvyu, dkwbhuk
khbth (270)
vpvab (76) -> evgvano, ibfbn, xovrzf, qzbet
iwotig (64)
nomlh (74)
oicxl (27)
gqjhcw (5)
cmfwb (18)
assqm (23)
tlyncze (36)
pqkzpq (35) -> gjpcufw, ocmac, hohsf, qdtdbl
zfxzq (58)
vhlon (80)
yiohbo (49) -> kwaqrgu, nezhs
tymahpb (87) -> ladkvis, rlrcvv, fciadzu, xpigis, kckucu, kdrswra, khbth
vcmntc (99)
beigkn (86)
pxzoq (16) -> mdimlja, hrufxbf, ufzrg
wuwag (65)
jdvya (385) -> zautz, haztq
jbxllu (22)
ashtupo (6)
xgdyhva (893) -> tkureky, solmej, yucxy
tooduon (92)
decdq (43)
ukhyz (95) -> ilnlkpv, bhzuzbt, agevvkl, byrfacr
pgscbge (77)
vdsjf (22)
zqdoieb (53)
sgtarpx (33)
yhiby (100) -> tzljrvn, ckezp
wdxfr (55)
owxgj (37)
uhmnltu (91)
lsbzpu (6)
ciwxuch (96)
qskbft (200)
gwwphk (79)
kbarp (60)
ygzas (55) -> ztyqti, nxapqfk
shifzsw (54)
huytmw (37) -> tsdfcgo, uqfbsti
atwtz (58)
ivcqfao (117) -> zqdoieb, bnmnak
wifwhkq (80)
ghtdnrx (1120) -> jcyxjwa, ftuvvv, nogbp
ztsajt (128) -> grvqyf, yxsvp
odmlf (60)
hocfv (70)
xegshds (53) -> upqkbq, hsdkmdv, ggxgmci, owjvny, gqiczm
efulo (213) -> lzcikn, tihjcht
jjkns (93)
fedhzki (61)
qtntw (294)
airlbu (71)
rzlkr (97)
ogujo (49)
jylgvd (165)
immcgrm (47)
hohsf (54)
knzffuc (11)
pwamvw (830) -> nvyaeal, tptcl, bbloz
nifptxs (90) -> zapbrn, vhxgbd, hmenje, hlzape, ugaxscf, fjemwxf, arnbvk
psvkj (81)
acsqtk (128) -> ykcivtj, bhcwhrf
ehgpqe (27)
knxtlig (57) -> lhqhx, alkysnc
sxino (50)
hwhgi (86)
glpwie (88) -> tzqux, uhwsavi, hfrtlx
mrkknr (34)
juxcxo (96)
pxpeniz (41)
buztc (76)
ewhzc (60)
bqpycy (253) -> snjfup, assqm
oynlkiy (25)
inful (168) -> knzffuc, ldgldpx
sybsbk (27)
towdptl (54)
yujhec (30) -> qkzgb, nmwrqbx, dktkg, rnbvott, etingr
nidvi (193) -> wjwit, dbuxj, rhpfhuc, ehyygbi
ttkqf (85)
iumsv (51)
ijvrlpz (95)
qqjvdh (76)
mwznhh (17)
ybwcod (63)
yulcus (24)
bcswm (96)
lfjpkw (216)
mjfhodb (18)
dbuxj (20)
ehyygbi (20)
lgdbaig (4976) -> lplhe, sznyfy, tjrecaq
odtgv (92)
falrf (97) -> zttpfsd, zfaog, sdezdnz
wyjnxf (12) -> bxipuq, ibosof
icdptj (42) -> onlnhb, ibqenc
lysysqu (237)
vltkp (33)
fwjufh (62)
uqkhzxt (60)
nbdfits (57)
mtsju (42) -> tzevrtu, wflxu
uxeyxk (88) -> ctmob, miwfyq, dtqyhj
krrxx (27)
tzevrtu (74)
tjmeqt (50) -> sinrdj, bbrsdwl
itzntrm (49)
ehjpn (291) -> opbaea, kvxtl
zayqjx (95) -> henlezn, vhpjrh
cvzsvf (15)
mmlsnw (63)
agwpt (84)
crfmd (70)
jjpnqsk (29)
otrlqy (56) -> hzbcp, qtcwngh
smkcc (147) -> yalqeoe, uzjoy
ztylsq (32)
dvzoq (17)
fyuitn (46) -> iumsv, fzmwc
zkiotrm (49)
ebhzgav (13) -> afooq, vihjl, xxoqrj, ipblpmy
wgtmwjd (682) -> lsoto, abqknn, uzlgko
kniivab (9) -> bymrwvc, aylzrt
oxrpu (63)
rsvcn (84)
hgbvgnn (96)
udlyr (227)
ktaso (28)
lmliqa (67)
rcpscka (117) -> eduizfz, sholtl
lzvpq (40)
sixlpmr (310) -> ttrvo, onsjs, ppujydt, zmuit, dfzbuxo
acibw (8)
ftpwtol (98) -> sstby, wduscx
pcfotkv (23) -> cszltz, pqdoti
eknyvq (97)
svxpx (66)
nhyhkq (68)
ttrvo (123) -> evpfdj, icdyfw
hlbhkkr (80)
bpuiaqr (96)
azykbk (80)
mnslyf (6) -> ouagp, wyjnxf, jxtzkel, vwtxaci, quxtc, yhiby, mgwiq
yfngcvd (59)
vfbhl (92)
fhcrud (62)
ggxgmci (66063) -> nsvan, qcbfbfz, anygv, gefrwix, kkeafw, jepbpw
dlcep (74)
aylzrt (67)
ghxwug (84)
auqfj (8)
kdajj (40)
wjjzh (96)
njnyoek (21)
lejuo (37)
abeinmv (961) -> tlfklor, izqbvna, sxpqvwv, foqjjt, xqusi
bkvaj (119) -> ktaso, ufqlean, czalivu
npwrqsm (33)
ysvbz (192) -> ekirg, lzvpq
unhnq (5580) -> rvkalxi, kabbsvt, jwptb
hqjlgre (47) -> jxxgurr, wvysxc
kpotsk (294)
nwand (344) -> vjtbtv, epnny, zdagr
xbxqoh (40)
ispeb (30)
lznlhjd (33) -> qqjvdh, ribswz, ndebha, vsojq
zlrboun (55)
ofxdo (93)
oxezpz (34)
uqfbsti (73)
zjljate (228) -> fzowmer, pcmgq
kbxvke (195)
uaxomk (87)
udawli (6) -> eiliwle, ijvrlpz
fnefss (348) -> qrtruk, ztsajt, aclnw
hbampcd (10)
czgws (57)
anygv (3678) -> tghfe, ybzqi, fabacam
qfxwzis (47)
ullsjlc (99)
yjarolt (69)
cfefqhk (84)
wflxu (74)
icroy (52)
ngkjtk (47)
oqsdfwt (43)
gswcnfo (93) -> gwnou, xedlv
mdznc (19)
zrdjlz (90)
btlczl (205) -> sjoxehw, ucrqw
xxyttn (228) -> dayjxl, melthx
qrunwtc (208) -> rzyuyv, gjisc, zrqzlvd, sqwtiaf, kgnuc, ufnpup, lfjpkw
hocvke (105) -> xzpov, btkih, kgijqnz
luswe (7)
whqrugy (77)
ikkatj (76)
hvdafm (590) -> gglig, nuorc, ioyesxg
kyjjz (210)
aqtgibg (84)
byrfacr (93)
tooqbdo (130) -> uaxsa, mcdpv, ccqsq
zbthq (88)
sboslk (27)
mwvihj (93)
ythbw (61)
kgnuc (76) -> iuzfnx, kylauj
xdnrao (194) -> wptxs, pdbtp
nszphks (60)
huyizg (24)
ujapot (76)
uycyz (2625) -> zzjwnh, ncirziy, icdptj
xuzskid (54)
xcvbgr (63)
macpvga (239) -> fdqmg, hyqrffw, rfytg, idnhoqh, dchobfa, kyjjz, aqahah
afrpf (29) -> ybbuy, uifanza
klqmcsw (185) -> zvzdrp, luswe
euglphc (165) -> jkyljso, abharn
heiejhm (48) -> ndyceo, nvoys
iulkkfp (112) -> pkwzp, cdzlq
ucrqw (73)
ivzps (58)
lyhgmwz (12)
ehwko (41)
hasbi (124)
pawmnl (91)
kbtogma (96)
dwwabne (6)
wjwit (20)
dktkg (232) -> ztuswk, iadezq, lhlyb
iimyku (59)
nlzohm (74)
owgrt (6) -> zpimcy, xconx
nyfdcz (61) -> zrdjlz, oevnzwt
crihz (75)
vehan (188)
ucocdl (11321) -> sixlpmr, yujhec, uryldo, bgltopb, tpreug, gwwjmo, pwamvw
urhawb (43)
ztyqti (71)
akmyzla (5)
hdtmmh (61)
vckxof (56) -> qpjgrr, lwejw
ctvhcv (21)
jdltlbx (92)
yxsvp (41)
xltlyvw (87)
fatxilu (17)
jljurl (14)
wfold (83) -> affnbx, fhcrud, gcfqnlw, fwjufh
mlavfvc (304)
mhwqim (70)
nemomq (62) -> jatrdb, nzrhtgr, ngyxxm, wnnnvgn, emlzcpy, haurusk, rynlaj
jdbmary (92)
icrilh (47) -> yolrh, cglbn
ybzqi (395) -> pcfotkv, ewifyk, uredmot
qvcpu (5)
qzbet (255) -> miunk, horfum
ckvji (42)
jpcurr (9)
stjxh (59)
kucnmn (50) -> incbyp, ogujo
vikstpo (80)
sdezdnz (20)
uticq (47)
yynji (85)
svnqa (313) -> nthqzh, uekit, hopiqm
xmufp (1476) -> cozhzk, xvjkwcd, yzjmm, jzsmrz, zuzkw
emdehy (42)
usixdl (84)
kgqhmx (27)
wjnibfh (12)
bvwdckm (47)
icoic (86)
aicos (78)
kabbsvt (1459) -> rwtva, wykyl
vuhehz (589) -> kdsmyax, tooqbdo, zizsy
lwbcn (64)
owjvny (68748) -> lgdbaig, yskay, uvftw
moxxiw (480) -> apyhkb, uakqow, tutfkbo, zmiyzin, hgwupie, thtnmkb
miunk (78)
tjrecaq (5) -> dzxkea, fsckdb, dreryps, svnqa, qfegd
twnon (66) -> ciwxuch, zpadu
fciadzu (152) -> rmnfq, tnxxb
pwmoihf (93)
vsbbr (61)
apyhkb (163) -> ixvot, dllffsl
dkwbhuk (1778) -> wavbwew, hlbhkkr
gnqxls (36)
dlglddh (98)
tlfklor (190) -> bwnvvei, thwgxk, thcth, gvphwwi
pwyspt (240)
eynrs (4953) -> gtvuq, vidgimw, fnefss, clmkbr, yxfysz, latvopy
ntfvgoj (97)
msxnd (45)
atwsafs (245)
incbyp (49)
ktiuc (1175) -> mzaxe, xxyttn, tmwvxgi, azcuz, szkgky, beaubot, ffvahj
hikmxhb (6)
ctmob (96)
zmiyzin (94) -> ryrig, sqwfq, mbpgrbi
gjauauy (12)
asymmuf (33) -> bwsbj, tfiynwo, zideon, xvkcms, jezdog, twnon, raloti
aoscisa (12) -> mnyokhx, ppigbow, eubqww
jbitbzb (54)
wwnkoz (62)
picati (50)
kmnmx (481)
lmdsuz (34)
nqohnyo (99)
rcvol (76)
ccqsq (27)
bsxjp (70) -> xcvbgr, gdidwas, hrhcwn
uakqow (237) -> advexy, zkvrxb
eckgtt (234) -> sofrfdv, nxygf, dbcrmjt
azcuz (64) -> vetib, jirva
hegyu (282) -> mfmrv, zwqpx, fyynsgp, hikmxhb
ppbvdzh (47)
snjfup (23)
yjlzh (23)
qcako (11)
lplhe (1375) -> qrklawm, avytp, fqrwf
gtyfx (110) -> vgvfksg, mmlsnw
jsdvowc (76)
beaubot (118) -> rscnige, dzjwel
wgvlq (23)
gjisc (192) -> liczn, lyhgmwz
jdqmr (10)
fjouck (19)
xreip (84)
ooeypdy (134) -> kgqhmx, vrvzkc
beiih (2781) -> wuwag, xbhexk
nroory (31)
hswdm (45)
kexrse (40)
ahracx (1289) -> mhgdq, homcnz
aksev (153) -> oqctx, cvzsvf
hmzal (155) -> hsael, wltyw
cldaf (97)
iscnnvh (606) -> fzzyjt, xtbqb, vyzhqx, owtdq
ibosof (98)
lhkoebz (67) -> jnenlog, jblnqb
slnoqn (21)
ulhmjq (34)
noqbv (8)
xeppuz (7)
fxpic (304)
gktepby (64)
gjkbf (67)
qaugj (61)
bbloz (51) -> icroy, rshqkfx
pjiicj (67)
ucqpz (39) -> dlglddh, tvhui
rwgpcs (57)
uzxovd (33)
jqougaa (59)
vrhke (93)
kkeafw (1602) -> gbcfg, uodmc, mnslyf
dcpqyp (6) -> soxzrwm, zxbirr
ceztn (37)
zwfly (18)
yfriw (59) -> xdvwjb, crfmd
gefbk (139) -> ivzps, atwtz
dpkdpj (126) -> mfyivv, aqtgibg
xpgzcxc (71)
rymvvbq (118) -> qjtldq, bawgh
bkpxd (20)
tqjmaxi (37)
phqyeg (90)
xnzhkrn (54)
niydyxq (33)
upqkbq (26244) -> dfnak, szsny, ehkrs, eynrs, cbpsyx, bbixr, zsckv
zcrgoy (17)
zuphhfa (199) -> ospcnv, pkagrvq
snhlzkk (428) -> iriyw, qdlcvk, mhium
qtcwngh (67)
jirva (92)
nyqju (66) -> nszphks, kbarp, guivwse, nnsogp
zdjxp (716) -> hhjdf, woiqeq, wfold
schym (20) -> fgyndgv, hlqed
ngyxxm (96) -> viqhzr, yylvwtf
woiqeq (149) -> jslpd, pfjvzzw
sthyxh (235)
xbhexk (65)
wnnnvgn (152) -> suqxa, wmhjbm
xzixh (71) -> wdnjs, hlzyncx
hwqhsc (235) -> akmyzla, tlekuus
bidasc (18)
pzbgmxx (156) -> vsevm, dwztnh, sybsbk
ckjdqr (49)
qdtdbl (54)
dvoul (816) -> yfriw, klqmcsw, ueepet
jkyljso (51)
ugqjxlt (43)
uazvt (57)
qovrrvg (172) -> eboizv, rrqfy
wgvxebc (62)
iwysxj (92)
gmtzu (30)
rwtva (64)
uulig (24) -> cqifjd, abjrozg, lgurkym, lznlhjd, lskrih
nefive (168) -> opkjkav, kyphnel
qsgjk (21)
kgpymt (18)
avytp (154) -> mdbixwm, mshht
yskay (47) -> asymmuf, gnjkpwf, tlmnwl, iyekup, rphgr, mfohmwu
mmyiwt (61)
wfilxtb (50)
iibjdbp (76)
amtxw (40)
ndyceo (86)
vbqsm (250) -> jdqmr, hbampcd
ttstqws (38)
ybbuy (87)
bfrmpr (101) -> dsjirbh, latvu
vagyecy (52)
rwlchj (73)
vdqpuon (868) -> pykcpw, yioxr, lcgym, wbijkw, dupdsj
zegoosj (36)
emcstpe (71)
pykcpw (168) -> bdkmsdy, mwznhh, zcrgoy
ymnichj (909) -> jfhpd, xvedp, phswa, udawli, tjmeqt
wlibbwd (5) -> nogjxu, bapvb, azykbk
bncsnst (12)
uxbnnor (76)
zbodhfq (56)
qhezs (93)
drhmhn (18)
hcfgf (197) -> tpfcmlm, xxxauxt, plcxnz
cbomeum (35)
qpjgrr (66)
dsjirbh (35)
lkoagx (58)
szsny (5431) -> yvbgs, lcepfsw, exfeog, rxxqpi, fuunnks
ytlzs (251) -> xnzhkrn, hezjirm, xuzskid, mwdqdbu
dzxkea (322) -> cwevfy, mepeg, bvnfacp, tygpx
xikuc (10)
rmnfq (59)
qfaubqr (82)
sauwet (445) -> bsieig, mrsxl
pdzfg (134) -> qfxwzis, ppbvdzh, rzwkjvu, sqwejs
quryyds (78) -> tfshhr, wdxfr
jvmuid (62)
cpwfem (93)
wratzd (22)
fhdkp (232) -> sbhcx, kdcyull, sikpe
agevvkl (93)
xadsoxv (71) -> whqrugy, fmlyuv, yerfxa
gvpac (51)
ipsfv (211) -> bkpxd, xivvpur
zprquyv (97) -> vrnxiui, htmrm
akobg (1607) -> vcsdacf, udfmh, ambgac
phxtzgu (78) -> exfffna, ewhzc, lufgbo, odmlf
rreoxyc (8)
jmmticn (24) -> chzrjqp, zhcmvvm, phqyeg
woxpci (80)
cvchqoz (24)
qxnuj (70)
tpfcmlm (20)
hewjj (263) -> dwwabne, iwhazt
adxox (79)
bxyvuvc (90)
opbaea (26)
xcbliim (404) -> wonyy, drhmhn, kgpymt
kzimilx (328) -> qmttkg, hpobvlt, pdzfg, zkborz, sbanx
mtvoxu (54)
qfggzok (67)
vxtfjhq (23)
hixeb (80)
bkvuz (73)
npozpo (98)
uqhnekk (17)
rhpfhuc (20)
ribswz (76)
mnyokhx (84)
oleuqo (43)
usbjyly (907) -> mjftixu, vldursw, ipsfv, pqkzpq, fkdauwn
nkbtsr (19)
szzhp (10)
hgwbjzo (83) -> pnhxqna, zxwmd
bubhosg (45)
lxxkjb (45)
yolrh (14)
cdzlq (96)
mbpgrbi (59)
lqvrx (85)
mmaqp (159) -> fsdlnh, ypmvfz
ekirg (40)
mdimlja (36)
cxynfb (183) -> wcqsimj, kwxpb, jprkymy, yjlzh
kgijqnz (55)
bapvb (80)
oqctx (15)
bemhni (51)
lcgym (199) -> anifo, eshjt
hopiqm (31)
ujdhkyw (136) -> yccqn, wfilxtb
ygbphkw (2416) -> eujnekp, utaxgw, jylgvd
occtbf (11)
vhdke (7) -> wqtbb, buztc, trzjoi, brubv
hpobvlt (186) -> lmdsuz, gumpsin, xpzzkdb, oxezpz
bdprpzc (38)
xcuid (26)
txayxeg (146) -> ngkjtk, uticq
ftuvvv (89) -> juxcxo, onmghy
vgysxct (103) -> cygpuh, svxpx
nxapqfk (71)
kylauj (70)
amyymhg (52)
jjpco (92)
mgmoe (45)
sinrdj (73)
rzclv (91)
ahcjog (59) -> yhhob, rvbbn, cawzw
saqcb (64)
cfmcrf (126) -> phmqb, xeppuz
uluavyz (311)
rzyuyv (182) -> uqhnekk, yqpivvl
ckezp (54)
ljihfp (71)
plcxnz (20)
uguqde (18)
scdvmta (40)
sofrfdv (28)
sahfob (87)
lzcikn (30)
gdidwas (63)
xqyyni (227) -> sjiaux, dwkkssy
yoibok (50)
wjgdz (54)
uifanza (87)
ufzrg (36)
kwxpb (23)
vmrdwe (13)
yucxy (65) -> mpedpht, jhzoyme
xuzvwcg (23) -> bluacvc, xcuid
wrbtgkb (45)
abqknn (142) -> kialkdr, xtqnalt
dklrle (46)
lhqbgck (632) -> ykcxtea, zjljate, ysvbz, qovrrvg
smzpa (114) -> jugwirf, ttborca, tmuuyka
fwoyu (86)
dgkhq (64)
tvhui (98)
clyht (10)
sspem (80)
pgpvnb (39)
zkborz (322)
liueudh (63)
myrlvj (63)
cszltz (51)
ambgac (34)
pwznn (120) -> cougztw, krrxx
iwhazt (6)
hgoruv (59)
gglig (156) -> saqcb, mubenp, vwqof
bwnvvei (50)
ezjgtbp (71)
xzkzoq (73) -> gtktonk, odtgv
qkzgb (219) -> ukkph, zrsya
cphgovc (269) -> nnqghfo, ohjvf
bdkmsdy (17)
hzbcp (67)
btkih (55)
ejyho (96)
ecagt (7)
vhxgbd (267) -> lkoagx, vrjfn, zfxzq
estka (47)
tihjcht (30)
tcaemi (171)
bnmnak (53)
jtendcr (85)
uekit (31)
jbpvbc (168) -> caukzx, pjiicj
pdbtp (91)
gjpcufw (54)
bvnfacp (21)
dqzsblu (91) -> wzlonwk, ywxkr, iggqobg, lydjsu
fjekad (48)
juzol (73)
dfzbuxo (17) -> fdrki, ufmlfmj, jxdqhu
dombfx (57) -> oxrpu, ybwcod
eidmza (481)
advexy (17)
ddleaoo (71)
bcyipmu (229) -> aflaj, cbomeum
rsttknu (97) -> bsxjp, bmzeddf, ndwvmg, glpwie, exbgy
tnham (76)
zideon (236) -> occtbf, hmltq
gfltww (51)
eduizfz (79)
fsdlnh (54)
jugwirf (51)
yylvwtf (98)
npefyxi (86)
gszlkf (30)
wptxs (91)
amntev (61)
hlqed (76)
rvbbn (81)
sygyji (6)
wcqsimj (23)
fjqccm (14455) -> tymahpb, ouycck, ehazo
gcfqnlw (62)
nnbokwt (194) -> mngea, hcajnu
bxipuq (98)
izqbvna (250) -> zoszls, kkoitqr
zxbirr (83)
nkipkdo (72)
yuvpdk (1257) -> gswcnfo, vceutc, kahwlj
eiliwle (95)
ohjvf (99)
cvkmmqc (54)
cdvscgo (44) -> tjmyc, phxtzgu, ayuhvb, eckgtt, wubltb
trzjoi (76)
zzdbn (50)
wonyy (18)
woxmeo (56)
enohjb (206) -> bkdxl, ttkqf
sznyfy (1243) -> xkfxnp, aoscisa, bbzhhk
phbapzv (50)
gkfymj (92) -> fbvhl, nkipkdo
hmltq (11)
kafyh (36) -> eincbt, amyymhg
pbkmeod (78) -> estka, ychaa
fgyndgv (76)
kunhp (19) -> rqpbivd, kdyuwjh
vruhp (227) -> woxpci, hixeb, sspem
jirnr (24)
kaekzo (48)
dsuonma (23)
xdxhzm (893) -> sncjicb, mmvhi, brchwcm, gefbk
bxpfs (92)
qdzbmhn (1640) -> xrabl, pwznn, dvzguq
aplke (19)
veuqzjh (25)
tpreug (377) -> axjkbq, hegyu, nyqju
fpmvb (17)
hlmdgp (78)
wbgwke (19)
dzjumb (43)
clmkbr (482) -> hasbi, pxzoq, ecfquya, coezio
affnbx (62)
ignblg (123) -> txeege, owxgj
ehqlqj (40)
vulynlf (18)
tghfe (89) -> usddqi, zuphhfa, rjzbhh
mdbixwm (33)
fdrki (60)
ggxhzhd (10) -> masck, amntev
obejsk (99)
eubqww (84)
auocy (60)
pqxef (50)
lhlyb (7)
tfiynwo (174) -> emdehy, zgtzox
jezdog (58) -> wvxvu, picati, tnisesh, yoibok
zizsy (89) -> hdtmmh, mmyiwt
ufmlfmj (60)
maklk (90)
oxvqjeh (17638) -> xscyb, wzjsbdm, lzikkl, xlyxjyx, flmeo, xcbliim
mmvhi (113) -> xpgzcxc, ddleaoo
yalqeoe (38)
ecfquya (32) -> vlfpib, dklrle
sagihe (940) -> bicpqwm, dyfptec, kbxvke, mgdhu
kiceppo (49)
tnpzf (114) -> ewrcce, jqougaa
vceutc (29) -> nqohnyo, vumxuur
uzdzkal (32)
csotuo (34)
xlufw (93)
hdqow (10132) -> uulig, macpvga, bkfxvy, zdjxp, akobg, vjboadc
rfqquyg (92)
sqwtiaf (30) -> aksgboa, qhezs
lamrqdl (59)
ittpbzy (101) -> jrkbskn, tqxfj
vjboadc (1160) -> dombfx, aksev, huytmw
iriyw (193) -> vxzgkqa, fjouck
hiftfo (88)
bsqlkiy (20)
hzkeiu (16)
arnbvk (329) -> zbodhfq, woxmeo
pxsmani (85)
jfapwfu (28) -> qhsptw, oqsdfwt, rsaitjm, decdq
jrkbskn (79)
zouqjoo (140)
wduscx (86)
vetib (92)
qtfroa (112) -> pxpeniz, ehwko, ujfmq
wgqnrc (30)
nvyaeal (29) -> mpijek, myrlvj
vsevm (27)
cdxwbv (23)
tzqux (57)
qkqgwsl (19)
ykcivtj (88)
tsdfcgo (73)
aqahah (68) -> airlbu, tgvqqn
jddzj (11)
zuzkw (16) -> sqfue, zlrboun
rdizbi (30)
lhxinfd (46)
sjiaux (15)
zfaog (20)
tgsdks (18)
dreryps (38) -> rfqquyg, jdltlbx, obqactw, hbgeak
xkfxnp (72) -> ejyho, hgbvgnn
qrtruk (63) -> ckjdqr, dtmhqm, tzdntld
mgwiq (16) -> wjjzh, kbtogma
sjoxehw (73)
jbpvtf (51)
gjtpl (48)
hrhcwn (63)
igkso (40) -> tqfzh, gbhlxb
cygpuh (66)
fckfbce (105) -> jtendcr, fcutr
yerfxa (77)
vjtbtv (77) -> unmxfe, rzclv
ppujydt (23) -> pujhjf, lvwwxt
qmvpgee (97)
kdrswra (102) -> xreip, ghxwug
oablvt (87)
latvopy (38) -> smmnc, quryyds, vyptki, hgchigs, vckxof
uaxsa (27)
lissl (97)
xovrzf (331) -> ehqlqj, scdvmta
homcnz (62)
crsjegg (21) -> iooqbyk, qrunwtc, bumibye, lhqbgck, sagihe, vpvab
pkagrvq (14)
thcth (50)
nogjxu (80)
hhjdf (207) -> wgvxebc, jvmuid
ipblpmy (46)
qmttkg (94) -> ohohls, zxdpmwl, czgws, ydbzo
qjtldq (7)
iggqobg (65)
hhfgih (152) -> wratzd, vdsjf, tbynt, hlzzxi
xvjkwcd (40) -> oleuqo, urhawb
fabacam (305) -> dlcxjg, dlactl, falrf
xvrgg (77)
fshoj (23)
uredmot (55) -> zmjzmyq, ghnldf
psynxr (19)
dtiprc (61)
pevdihc (135) -> rbmoj, hiftfo
jxtzkel (132) -> bdprpzc, ptfeyky
uvftw (1277) -> hvdafm, xgdyhva, vvndbk, gwkfb, cdvscgo, lhagpts
jvocm (114) -> lamrqdl, stjxh
zgtzox (42)
fzmwc (51)
aopwj (77)
vgeln (89)
nvwknv (47)
vrvzkc (27)
sholtl (79)
guivwse (60)
gjbikl (12)
kdsmyax (51) -> vhlon, vikstpo
dyqgl (50)
uqjsosg (67)
kahwlj (79) -> nlzohm, nomlh
lqgvx (97)
sstby (86)
sgcmty (77)
hgchigs (34) -> djktoc, aopwj
ouagp (24) -> lcfpcst, bxpfs
tsahe (106) -> gjkbf, uqjsosg
psjbb (86)
vrjfn (58)
rjzbhh (91) -> sarppfb, nhyhkq
melthx (10)
nuorc (40) -> ytlpe, fvhfru, xvrgg, kbdbe
mubenp (64)
mqtxqmz (12) -> lqvrx, chczyg
ypmvfz (54)
txote (73)
vxzgkqa (19)
bbrsdwl (73)
xconx (83)
enlfaa (479) -> djquz, sthyxh, hkuay, hmdqubz, qxccjtw, vgysxct
wbwyvyu (13) -> hmzal, fckfbce, fqdtzfy, hgwbjzo, hewjj, rcpscka, cxynfb
cvrph (145) -> dbdrdzo, acdahz
izsgfu (64)
jcexcl (18)
oacoxb (1336) -> msazzkr, jbpvht
acdahz (83)
afooq (46)
fkdauwn (187) -> aynknet, uzdzkal
mpedpht (91)
lcepfsw (218) -> schym, pbkmeod, ntodfz, owgrt, dcpqyp
xscyb (458)
pujhjf (87)
jtankmh (190)
pxctb (71)
tqfzh (75)
jzsmrz (126)
nzrhtgr (150) -> ezjgtbp, xfxsxh
tnxfqf (34)
djquz (211) -> fwlme, lsbzpu, ashtupo, gbauyfh
jyhddhe (49)
wdnjs (85)
byaaoab (186) -> ikhsv, pgpvnb
bumibye (997) -> xzixh, kdsbbcf, nyfdcz
frffchv (33)
cogziqp (12)
brchwcm (27) -> uxbnnor, iibjdbp, qupaz
fbvvzp (304)
tnisesh (50)
henlezn (64)
kbdbe (77)
vojpfo (43)
psxqu (61)
fvzfdr (52)
rvkalxi (77) -> vlzbp, jbpvbc, muvvvu, xadsoxv, ahcjog
abharn (51)
gegdmus (42)
hfdjtw (43)
zoqmn (54)
vgwxuan (50)
pjukxu (76) -> ikkatj, ujapot, rcvol
oziwd (8)
evgvano (411)
evquf (195)
gkjafru (18)
ygwxr (144) -> ylpvre, cabbtti
dniumoe (83) -> wajnp, mtvoxu, towdptl, kmiax
tlekuus (5)
mrchmw (43) -> xssia, wifwhkq
ueepet (113) -> ugoetwf, hfdjtw
znzsgk (98)
cabbtti (48)
cbpsyx (1290) -> nifptxs, oxzecjg, uycyz
ulqnjx (18)
ezjqb (49)
xvkcms (212) -> vxtfjhq, imjhra
kxgrtg (61)
hxtbym (50)
pqdoti (51)
lizkruo (15)
zzjwnh (130) -> ehgpqe, oicxl
mzbwym (39)
nkixmeh (32)
xxxauxt (20)
caukzx (67)
zenvqu (124) -> sboslk, fwudfax, rlbdave, onqnmbp
ivyarjy (46)
ldgldpx (11)
grvqyf (41)
hlzzxi (22)
jsvwb (10)
cglbn (14)
swefql (99)
zgtsnwk (63)
hlzape (373) -> fatxilu, htyvy, fpmvb, xxufma
lqylns (82) -> inful, igkso, mtsju, otrlqy, jtankmh, vorviq
iouzha (161) -> slnoqn, gmmkqp
kwtszff (119) -> bkfqmqn, tbsmz
rqpbivd (92)
bupugpv (36)
zaxds (66) -> ygywjq, pusxipi
onmghy (96)
jnenlog (78)
sovfoi (97) -> uluavyz, vhdke, kuivtq, ylluriq, cvrph, pevdihc
cfcbhku (90)
tgvqqn (71)
ncirziy (48) -> ulhmjq, zwoqof, hrzvmel, vrbss
obqactw (92)
udfmh (34)
hgsiz (46)
bicpqwm (177) -> sygyji, bjfou, qsche
jslpd (91)
pcmgq (22)
graedp (89) -> npozpo, znzsgk, jxvses, teikos
pnhxqna (96)
cwwdl (73)
msazzkr (28)
mngea (23)
gohxocn (23)
qfqnrvt (4602) -> ptwedjj, xdxhzm, qsiqu
rsaitjm (43)
lwroef (37)
jbpvht (28)
wjovhv (35) -> gokngb, vczph, phbapzv, tqcwn
nsabyg (99)
tvbswr (45)
fwbly (134) -> dzjumb, ugqjxlt
hrufxbf (36)
wltyw (60)
ptfeyky (38)
jwptb (1539) -> yulcus, ndvns
qxccjtw (178) -> wbgwke, arqqbb, qkqgwsl
ewrcce (59)
oxzecjg (2983) -> eknyvq, myypsh
dwkkssy (15)
gnxiznu (126) -> nroory, loofhh
bejncv (38)
qcbfbfz (2322) -> vuhehz, lqylns, wgtmwjd
pkwzp (96)
owtdq (103) -> frffchv, lkzdum, sgtarpx
mdauzot (240)
alkysnc (73)
gtvuq (898) -> zuapfco, kexrse
eewbbvo (70)
jfhpd (196)
mshht (33)
sbanx (204) -> fhabfp, yfngcvd
pusxipi (83)
mwbvrhn (182)
fbvhl (72)
ulvmck (1649) -> bfrmpr, tcaemi, gkvrrq
yproaz (18)
bgltopb (866) -> bpbik, kniivab, quyasfd
nnkpuhp (65)
bhzuzbt (93)
tcbrhdi (87)
guzycwt (34)
jdabb (61)
fsckdb (268) -> yjarolt, dewtepm
yfkyv (45)
hyqrffw (146) -> ztylsq, nkixmeh
ctkjip (87)
vularzu (93)
ygywjq (83)
xzjcf (1843) -> mmaqp, euglphc, smzpa, zprquyv
jblnqb (78)
dbcrmjt (28)
dewtepm (69)
zdagr (249) -> gqjhcw, qvcpu
jepbpw (383) -> tgwmbza, wifvw, nwand, tyjrdme, snhlzkk
duxfof (39)
tzljrvn (54)
smmnc (20) -> agwpt, cfefqhk
vgvfksg (63)
ugaxscf (289) -> vtneuhy, bfcab
tbynt (22)
hkuay (121) -> bejncv, fpqpmca, ttstqws
eujnekp (7) -> licby, adxox
exfeog (853) -> crihz, xuzvwcg, icrilh
gmmkqp (21)
qfegd (228) -> vgeln, zdrwxqt
lewizr (39)
teikos (98)
bwsbj (216) -> xrwnzgz, hydfd
frzwdkn (11)
fgokr (19)
cwevfy (21)
dzekyo (43)
tmuuyka (51)
njppj (23)
eboizv (50)
gvphwwi (50)
vidgimw (534) -> dsstjf, fyuitn, kucnmn
wmhjbm (70)
zkvrxb (17)
gwnou (67)
pihpjbp (14)
lwejw (66)
fmlyuv (77)
rtstdkd (47)
kmiax (54)
vyptki (98) -> mgmoe, msxnd
gwkfb (1238) -> ggxhzhd, rymvvbq, wvwiwua
wvwiwua (108) -> wjnibfh, exiwwbx
shilhu (67)
bpbik (97) -> cdxwbv, zchxtxj
hssgk (14)
hujzcku (44)
tjmyc (258) -> gmtzu, rdizbi
vhjfcw (191) -> yproaz, tgsdks
ewzvs (54)
qsche (6)
rxwtoee (30)
muvvvu (167) -> gbkubnt, lxxkjb, vjdrrnk
sbhcx (48)
vvndbk (953) -> jgwvyp, udlyr, vhjfcw
nnqghfo (99)
coezio (70) -> jcexcl, woeabie, uguqde
zchxtxj (23)
rfytg (142) -> oldfs, mrkknr
ndtsa (7)
qrklawm (78) -> emcstpe, pxctb
ladkvis (99) -> nbdfits, uazvt, rwgpcs
ewiugad (167) -> uzxovd, vltkp, niydyxq, yxknolf
yioxr (150) -> njppj, inpjzmh, nbfpp
exfffna (60)
rrqfy (50)
sdwnkb (93)
rshqkfx (52)
fvhfru (77)
tutfkbo (187) -> ckvji, gegdmus
vlfpib (46)
lsoto (148) -> fwjnm, hzkeiu
ecxayrz (63) -> wutzd, vbqsm, nefive, ftpwtol, hocvke
urexzjf (187) -> wxyhyh, vojpfo
nbfpp (23)
rspilzk (40)
zgatapc (49)
vcsdacf (34)
rnvenxi (36)
mcdpv (27)
oieuzv (25)
txtnweq (67)
sarppfb (68)
wpxxz (20) -> fedhzki, vsbbr, psxqu, jdabb
usglf (78)
ujfmq (41)
fpqpmca (38)
yxknolf (33)
ppeumu (517) -> mrchmw, bkvaj, iouzha, knxtlig, jdvro, afrpf, kunhp
acueoez (86)
foqjjt (294) -> fjekad, gjtpl
dyfptec (105) -> ispeb, wgqnrc, rxwtoee
idnhoqh (70) -> mzftrlb, qxnuj
vrnxiui (85)
tgwmbza (1064) -> aplke, tqnqxx, nkbtsr
mhgdq (62)
yccqn (50)
bbixr (4503) -> xmufp, moxxiw, nemomq
aynknet (32)
cdrjh (64)
hgwupie (115) -> aicos, zziuivt
fwjnm (16)
qhsptw (43)
ptwedjj (33) -> xdnrao, enohjb, uxeyxk, qxzhq, fhdkp
jxvses (98)
xvedp (50) -> cwwdl, rwlchj
jxxgurr (95)
etingr (217) -> ulqnjx, vulynlf
bsieig (18)
mhium (203) -> hssgk, jljurl
vlzbp (286) -> auqfj, noqbv
suqxa (70)
dlcxjg (17) -> mhwqim, jcrmny
xpigis (270)
ehkrs (11) -> ulvmck, dhujegl, fngfnuz, qdzbmhn, usbjyly
xxoqrj (46)
rscnige (65)
dhujegl (34) -> mlavfvc, fxpic, fbvvzp, qinwdk, acsqtk, pjukxu, iulkkfp
qgufw (74)
cznqutl (99)
evpfdj (37)
pfjvzzw (91)
ffvahj (47) -> qdxpkx, vmvjl, qfggzok
yfouoak (56) -> vfbhl, jjpco
qntqokn (8)
kebefwy (246) -> jguhhsb, jpcurr
hbozym (48)
infll (59)
wvysxc (95)
ospcnv (14)
nvoys (86)
efvvtgo (185) -> zegoosj, tlyncze
lhqhx (73)
yqpivvl (17)
hcajnu (23)
tgtjhg (25)
viqhzr (98)
yygyfi (155) -> jddzj, qcako, frzwdkn
yzjmm (36) -> ldakuz, fivaym
kialkdr (19)
gdibw (12)
rdngdrd (80)
lgurkym (337)
ddsmt (87)
rkdopb (82) -> todbu, sahfob, uaxomk
xslucv (51)
bfcab (76)
gnjkpwf (947) -> zayqjx, smkcc, lhkoebz, ivcqfao
iadezq (7)
ckgmjsg (44)
tqxfj (79)
nmwrqbx (55) -> obejsk, nsabyg
mepeg (21)
iuzfnx (70)
bfphqmo (99)
myypsh (97)
ykcxtea (86) -> cpwfem, vularzu
yhhob (81)
fcutr (85)
rjfqyz (71) -> rfvtfs, xlufw
ryrig (59)
fzzyjt (202)
tyjrdme (321) -> zngystc, jfapwfu, qskbft, pvqfa
ywxkr (65)
vjdrrnk (45)
tptcl (61) -> krxpma, rtstdkd
tnxxb (59)
zrsya (17)
jprkymy (23)
icdyfw (37)
iovan (1882) -> wppgjkw, rkdopb, ehjpn
qeijcl (12)
izfqx (47)
mwdqdbu (54)
txeege (37)
onqnmbp (27)
isuevmr (221) -> sphktgz, gjauauy, bncsnst
ohohls (57)
kwaqrgu (94)
mfmrv (6)
dzjwel (65)
ibfbn (75) -> hquuw, sreeeqf, usixdl, rsvcn
uzjoy (38)
jhzoyme (91)
loyjp (62)
vazst (45) -> ymfwoto, hxtbym, lfszf
jkhruz (15)
ioyesxg (132) -> wjgdz, cvkmmqc, shifzsw, zoqmn
veyvaoh (7)
cpoxc (217) -> wqobawc, shilhu
zvzdrp (7)
hydfd (21)
tygpx (21)
juvwvsn (53)
phswa (176) -> xikuc, clyht
fhhzvwj (20)
vldursw (145) -> rsjmc, juvwvsn
ocmac (54)
dchobfa (12) -> ullsjlc, cznqutl
onlnhb (71)
ftolbk (183) -> pagsex, wrbtgkb
flrdpaj (92)
vmvjl (67)
kqcog (48) -> bcswm, bpuiaqr
cstnc (79) -> bubhosg, tvbswr, hswdm, yfkyv
vuazmuj (23)
eincbt (52)
wzjsbdm (62) -> sytjgl, tfuuzgi, bfphqmo, vcmntc
dvyohbc (85)
zwoqof (34)
wqtbb (76)
gkvrrq (77) -> izfqx, bvwdckm
xivvpur (20)
jxzyc (141) -> nvwknv, immcgrm
ufqlean (28)
todbu (87)
qdmop (85)
thtnmkb (95) -> adjfke, zbthq
ikzqhu (50)
jcyxjwa (98) -> agjdl, dtiprc, qaugj
horfum (78)
dayjxl (10)
vczph (50)
alpas (17) -> dgkhq, izsgfu, gktepby, iwotig
dqrsxx (81)
ixvot (54)
hoagrj (99)
dwztnh (27)
uodmc (727) -> atwsafs, hwqhsc, wlibbwd
dqyaco (32)
bkfqmqn (70)
dlactl (127) -> jkhruz, lizkruo
kuivtq (311)
wkkqsy (80)
szkgky (76) -> psjbb, beigkn
wutzd (168) -> guzycwt, tnxfqf, csotuo
rlrcvv (128) -> hkelse, ljihfp
vtneuhy (76)
exbgy (159) -> veuqzjh, oynlkiy, oieuzv, tgtjhg
bjrfinh (97)
hsdkmdv (50286) -> unhnq, crsjegg, abfkpur, qfqnrvt, etsxvmj
masck (61)
vrbss (34)
ytqwzv (68) -> qfaubqr, fyxdfod
kvxtl (26)
ibqenc (71)
zsckv (6579) -> iscnnvh, pqxpguz, wqudy
splhdhe (80)
raloti (104) -> sgcmty, pgscbge
sreeeqf (84)
tqnqxx (19)
xaskpny (79)
gbauyfh (6)
exiwwbx (12)
phmqb (7)
bydmdru (60)
zwqpx (6)
ehazo (1097) -> heiejhm, fwbly, gyivxga, wjhgb
zudhbe (168) -> ndtsa, ecagt
fncsm (46)
zhkmnxk (21) -> ytlzs, ukhyz, cphgovc, vruhp
cqifjd (189) -> knsphtv, qgufw
vcllnn (244) -> dkbycez, jxzyc, qtfroa, ucqpz, wjovhv, pxfpf, orawnkw
arqqbb (19)
dsstjf (100) -> cvchqoz, jirnr
bjfou (6)
gokngb (50)
qupaz (76)
opkjkav (51)
fgayln (257)
jzdtwf (7)
dbdrdzo (83)
sytjgl (99)
nthqzh (31)
ndwvmg (235) -> qeijcl, kyhace
gwwjmo (575) -> mdauzot, yfouoak, qkrirn
evofb (24)
ugoetwf (43)
zrqzlvd (42) -> eavfy, tcbrhdi
djyks (81)
gwsceo (9) -> beiih, guipj, xzjcf, ktiuc, iovan, ygbphkw, abeinmv
qdlcvk (61) -> dvyohbc, yynji
lvwwxt (87)
fngfnuz (1222) -> vehan, yygyfi, vpnfkw, gnxiznu, ooeypdy
dfnak (6645) -> cxfvbz, oacoxb, rsttknu
pftmymi (13)
ikhsv (39)
quyasfd (63) -> xbxqoh, kdajj
ntodfz (172)
krxpma (47)
xpzzkdb (34)
dtmhqm (49)
hquuw (84)
tfuuzgi (99)
rbmoj (88)
tfshhr (55)
nxygf (28)
wvxvu (50)
chczyg (85)
djktoc (77)
ttborca (51)
pvcpmx (137) -> jjpnqsk, gseohkk
fwudfax (27)
vyzhqx (62) -> eewbbvo, hocfv
dvzguq (140) -> ulzcu, dvzoq
rhwfl (74)
mfohmwu (344) -> dniumoe, uuimn, ewiugad, vkkds, bqpycy
lskrih (177) -> splhdhe, rdngdrd
vihjl (46)
adjfke (88)
brubv (76)
rzwkjvu (47)
iyekup (1248) -> ignblg, ebhzgav, ygzas
uryldo (749) -> mqtxqmz, mwbvrhn, zudhbe
vorviq (176) -> jzdtwf, veyvaoh
vsojq (76)
ilnlkpv (93)
tzdntld (49)
tuefd (209) -> kqcog, hhfgih, nnbokwt, pwyspt, txayxeg, tsahe, ygwxr
bmzeddf (259)
sqfue (55)
ndebha (76)
ulzcu (17)
tlmnwl (474) -> nidvi, alpas, urexzjf, ftolbk, efulo
thwgxk (50)
soxzrwm (83)"
  end
end

defmodule Santa.Day8.Input do
  def input() do
    "ioe dec 890 if qk > -10
gif inc -533 if qt <= 7
itw dec 894 if t != 0
nwe inc 486 if hfh < -2
xly inc 616 if js >= -3
j inc 396 if b != -5
nwe dec -637 if uoc > 0
b inc 869 if yg >= -3
gif dec -221 if iyu < 0
tc dec -508 if gy >= -7
x dec 637 if gif < -526
nwe dec -185 if nwe != -8
x inc 638 if b != 869
ih dec -722 if itw > 9
xly inc -38 if ih >= 8
hm dec 910 if t == 0
uoc dec -585 if qt == 0
js dec -325 if hm == -910
yr dec -922 if cp != 0
qt dec 316 if itw != 2
bi dec -422 if iyu <= -1
uoc inc -862 if itw <= 3
itw dec -301 if x < -632
gif inc -492 if fi != 5
uoc inc -745 if x < -631
xly inc 21 if js > 331
hm inc 44 if js > 334
js dec 503 if tc > 503
t inc -216 if j == 396
yg inc 559 if nwe > 189
bhp dec -214 if x >= -646
hm dec 366 if fi == 0
t dec -658 if nwe == 185
hm inc -432 if qt <= -307
xly dec 695 if uoc >= -1031
cp inc -438 if x != -647
yg dec 211 if x >= -628
bi inc 829 if ih > -8
yg dec 540 if tc >= 503
hm dec -913 if qt > -310
qk inc -406 if itw < 309
uoc dec -716 if iyu >= -1
ih inc -655 if qt != -316
ih inc 6 if xly > -80
cp inc 795 if xly > -88
bhp dec 59 if yr < 1
yr dec 952 if x >= -628
xly dec -867 if j > 393
fi inc 720 if ioe >= -892
gif inc 454 if ioe > -886
j dec 547 if fi != 720
qk inc 665 if bi > 819
hm dec -174 if cp != 357
hm dec -795 if uoc <= -314
uoc inc 273 if itw <= 307
gy dec 212 if xly >= 783
tc inc 918 if ih != 9
tc inc -43 if js >= -186
gif inc -615 if b == 869
bhp inc -335 if fi > 724
ih inc 747 if hm >= -1711
ih inc -515 if ioe != -881
yg dec 967 if cp == 357
yr inc -23 if qt < -309
gif dec -16 if cp == 357
itw inc 353 if uoc <= -41
cp dec -788 if b <= 869
bi dec 510 if itw < 306
yg inc 321 if qk > 265
itw inc -194 if gif == -1624
yr dec 484 if b == 869
yr dec 828 if yg != -1515
cp dec -700 if gy != -212
ioe dec -238 if iyu >= -5
xly inc -334 if bi != 316
js dec 642 if uoc < -27
cp inc 131 if x >= -633
cp dec 693 if iyu >= -2
bi inc 671 if hm >= -1712
fi dec 781 if nwe >= 176
ioe inc 770 if qk > 253
nwe dec 381 if j < 406
qt inc -599 if hm != -1715
yg inc 277 if qk >= 268
hm dec -656 if ioe < 117
uoc dec -875 if ih < 243
js dec 297 if yg >= -1514
hfh dec 821 if iyu >= 8
ioe inc -133 if iyu > -9
x dec -623 if iyu != 0
gy inc 240 if cp >= 451
gy inc 937 if hfh != -10
tc dec 476 if tc != 1376
iyu dec -35 if hm == -1706
nwe dec 86 if yr >= -1344
cp dec 96 if qk != 259
x dec 864 if b >= 865
hm dec -965 if bhp != 161
bhp dec 402 if b == 859
b inc -19 if hm <= -746
xly dec 24 if uoc >= 840
ih inc 816 if js >= -1117
xly dec 511 if iyu != 5
t dec -214 if iyu != -8
yr inc -81 if tc <= 898
js inc -187 if yr < -1331
x inc -970 if cp < 462
hm dec 668 if tc > 904
uoc dec -488 if yg <= -1502
uoc inc -974 if j != 394
qk inc -58 if qt > -913
j dec -741 if js == -1304
j dec 58 if nwe != -276
fi dec 772 if qt != -910
yg dec -271 if bi == 990
uoc dec 983 if gy != 966
t inc -355 if uoc >= -636
hfh dec -486 if yr == -1335
itw inc -102 if tc < 905
gif inc 663 if b > 862
fi dec -546 if js < -1294
bhp dec -955 if qt > -914
qt dec -927 if ih == 1054
gy inc -272 if uoc <= -620
yg inc -30 if nwe != -291
nwe dec 393 if ih != 1048
ih dec 904 if tc != 915
nwe inc -211 if j != 1084
qt dec 249 if tc <= 913
ioe dec -703 if gy <= 695
hfh inc -836 if j <= 1079
tc inc -153 if qk <= 255
bhp dec -697 if uoc == -627
hfh dec -762 if qk <= 263
xly inc -369 if qk >= 269
xly inc -135 if xly > -80
j dec 773 if x <= -2468
iyu inc -541 if iyu < 9
itw inc 870 if xly < -77
x inc 646 if x >= -2474
qk dec 447 if fi >= -287
qk dec -37 if yr >= -1344
nwe inc 406 if gy >= 703
j inc 72 if j > 299
qt inc 946 if t < 306
hm dec -406 if cp == 451
ioe inc -906 if bhp < 849
xly inc -924 if b <= 871
xly inc -645 if qt == 709
b dec -578 if bi <= 999
js inc 237 if t <= 304
gif inc 425 if bi <= 989
uoc dec 318 if x < -1821
fi inc 455 if bhp < 859
fi inc -282 if iyu <= -537
bhp dec 980 if cp > 451
t dec 204 if gif > -967
ioe inc -273 if fi == -114
iyu inc 940 if yr != -1335
t inc -593 if uoc == -945
tc inc -34 if yr >= -1335
yg inc 204 if gif < -969
nwe dec -128 if iyu == -541
qk dec 949 if yr >= -1335
nwe dec -582 if js >= -1063
tc dec -550 if cp <= 452
js inc 427 if ioe <= 407
bhp inc -672 if gif == -961
b dec 890 if hm > -1419
qk dec -801 if iyu > -538
t inc 789 if tc != 1433
tc dec 134 if hm >= -1413
cp inc -287 if b > 552
b inc -131 if bhp == -800
iyu inc 991 if b <= 429
itw inc 3 if hm > -1411
ih inc 263 if b < 428
j dec 848 if t <= 294
xly inc 94 if ioe != 406
j inc 116 if qk < -1094
t inc -779 if yr >= -1338
nwe dec 910 if tc >= 1281
uoc inc -945 if itw <= 975
xly inc -675 if hm <= -1410
xly inc -58 if x == -1825
hfh inc -701 if bhp < -798
nwe inc -586 if uoc > -952
x inc -164 if b < 435
hfh inc -641 if j != -354
yg inc 827 if qt > 699
iyu dec -512 if t < -478
j inc 651 if itw < 987
yg dec 537 if hfh >= -289
ih inc -289 if qk <= -1094
itw dec 158 if nwe > -2260
xly dec 704 if hfh == -289
itw dec -564 if j <= 305
nwe inc -820 if hfh >= -290
bhp dec -252 if nwe < -3065
bhp dec 680 if itw <= 1385
cp inc 811 if t == -486
x inc 423 if b > 419
itw inc 131 if gy <= 693
qk inc 500 if nwe == -3065
gif dec 583 if yg <= -981
x dec -261 if fi > -115
itw dec 314 if cp >= 977
itw inc 197 if b < 429
ioe inc -666 if x <= -1314
hfh inc 345 if hm < -1402
b inc 653 if itw >= 1719
qk dec 298 if itw != 1701
t dec 943 if qt > 703
gif inc 430 if qt > 715
bi inc 60 if gy > 687
j inc -816 if gy > 685
xly inc 334 if js != -1075
qk inc -388 if xly != -2656
t inc 475 if fi <= -113
t dec -842 if nwe > -3078
fi inc 284 if cp == 976
xly inc 265 if ih >= 127
qt dec -481 if hfh < 58
xly dec -484 if ioe <= 415
ioe inc 525 if xly >= -2183
t inc 658 if tc >= 1281
nwe dec -680 if t != 555
qk inc 395 if tc <= 1293
bi dec 422 if gy > 683
itw inc 713 if t == 546
bhp dec 285 if iyu < 967
gif inc -186 if tc <= 1297
iyu inc -858 if iyu <= 954
gy inc -320 if nwe < -2384
yg inc 691 if qt < 1199
hm inc -19 if j <= -528
bhp dec 403 if xly == -2175
gy inc -886 if qk >= -1399
yg inc 404 if js < -1065
gy inc -255 if fi > 160
bhp inc 740 if bi <= 620
b inc -548 if nwe != -2398
t dec 255 if nwe >= -2397
t inc -678 if js >= -1064
js inc -497 if js >= -1071
iyu dec -463 if hfh >= 52
js inc -537 if b != -122
t inc -518 if yg != 129
ih dec 208 if yr >= -1341
qt dec -566 if yr > -1340
itw inc 113 if nwe == -2395
ioe dec -471 if itw != 2424
cp inc 222 if tc > 1283
ih inc 28 if bhp != -1915
yg inc -923 if xly <= -2169
bi inc 893 if yr == -1335
cp inc -459 if gy > -771
b inc 958 if bhp != -1922
yg dec 896 if tc < 1298
ih inc 385 if hfh != 48
j dec -663 if iyu <= 1419
fi dec 89 if x != -1297
tc inc -294 if gif != -1147
uoc inc -232 if qk < -1381
bi inc 743 if hfh >= 48
gif inc 317 if yr == -1335
hm dec 938 if xly <= -2172
iyu dec 961 if xly > -2173
qk dec -69 if qk == -1390
ioe dec -450 if hfh != 52
j inc 944 if qt != 1748
x dec 48 if gy <= -764
yr dec 505 if iyu <= 1422
x dec 316 if yg < -1695
bi inc -386 if nwe != -2392
xly inc 363 if fi < 87
uoc dec 629 if nwe < -2384
ioe dec 816 if bhp < -1909
uoc inc 196 if fi == 81
gif dec -196 if iyu > 1419
bi dec -652 if nwe != -2402
gy dec -416 if yg > -1701
bhp dec 78 if qk < -1384
bi inc -660 if ih != 328
ih dec 352 if fi >= 79
ioe inc 288 if itw == 2414
b dec -461 if xly < -1814
t inc -291 if js > -1557
tc dec 502 if j == 432
gif inc -800 if qk >= -1384
yr dec -59 if gif > -641
yg dec 360 if yr <= -1275
nwe dec 790 if gy >= -358
b dec 544 if yr == -1268
tc inc 743 if ih <= -22
bhp inc 314 if cp <= 744
x dec -708 if tc <= 2039
iyu inc 968 if yr != -1274
x dec 454 if hfh <= 61
qk dec -41 if bhp >= -1685
ih inc 302 if tc != 2032
cp inc -167 if yr == -1281
ih inc 349 if qt != 1760
gif inc -367 if uoc <= -1603
xly inc 229 if gy <= -344
gif dec 459 if fi >= 76
hm dec -297 if gy <= -347
j dec -320 if iyu <= 2397
qt dec 634 if b <= 837
cp dec 342 if ih > 324
ioe dec -119 if qt > 1120
x dec 564 if hfh != 56
tc dec 779 if qt < 1130
hm dec 586 if ioe > 695
yg dec 373 if ioe > 683
xly inc 344 if iyu <= 2397
hm inc 91 if qt < 1121
ih inc -219 if ioe <= 699
ih inc -937 if ioe <= 702
hfh dec -805 if t != -221
itw dec -385 if ioe == 693
nwe inc 946 if js != -1564
qt dec 685 if hm == -2052
itw dec -625 if bhp <= -1674
ih inc 933 if qt < 442
js inc 971 if itw != 3437
hfh dec 444 if uoc >= -1617
itw inc 182 if itw >= 3433
hfh inc 509 if gy > -353
ioe dec 952 if bhp > -1688
b inc 210 if uoc > -1615
itw inc -579 if hm < -2044
hm inc -427 if j <= 739
uoc inc 90 if nwe > -3190
ih inc -678 if x <= -1407
js dec -238 if x >= -1419
ih inc -263 if tc > 1244
ioe dec 30 if gy == -352
t inc -76 if ih < -837
yr dec -442 if cp < 400
iyu inc -425 if j < 751
qk inc -189 if js <= -350
yg inc 282 if t >= -311
b inc 11 if j > 737
qt dec -839 if bi == 1863
tc inc -40 if bhp >= -1688
b inc 439 if yg >= -2142
bhp dec 144 if b == 1057
gy dec 460 if b <= 1065
qk inc -681 if itw >= 3035
iyu dec 0 if j > 740
ih inc -189 if uoc <= -1520
bi inc 360 if hm > -2058
yr inc 603 if xly <= -1232
j dec 68 if fi < 90
yg dec -425 if gy <= -804
x dec 295 if x != -1405
nwe inc -355 if j <= 679
ioe dec -506 if xly < -1229
bhp inc -489 if j >= 671
hm inc 730 if ioe > 209
gy inc -464 if iyu < 1968
bi dec 191 if yg > -1731
j dec 707 if gy < -804
ioe dec -180 if qt == 437
ioe inc 909 if yr != -231
yr dec -627 if ih == -1027
itw dec -145 if gy == -818
hm inc -156 if gif == -1460
hm inc 301 if yg < -1735
gif dec 137 if bi == 2039
cp dec 777 if yg <= -1717
fi dec -90 if yg != -1726
cp dec 256 if tc <= 1217
gy dec -401 if ioe >= 393
js inc 850 if tc <= 1219
iyu inc 124 if hm >= -1478
tc dec 341 if nwe > -3534
yr dec -13 if tc <= 1218
x inc -44 if cp != -634
t inc 591 if qk >= -2229
t dec -38 if itw <= 3045
xly inc -996 if ioe >= 396
cp inc -21 if x > -1757
t dec -989 if itw >= 3033
uoc inc 193 if fi >= 74
x inc 234 if uoc > -1333
hm dec -353 if gif > -1597
cp inc 684 if t == 1315
bhp dec -437 if iyu < 2095
xly inc 59 if j == -30
nwe inc 176 if fi >= 81
b inc 573 if iyu > 2086
gif inc 526 if gy > -418
ioe dec 261 if b >= 1625
qk inc -976 if bi != 2040
qt inc -648 if cp >= 31
x dec -490 if iyu <= 2099
bi dec 450 if x <= -1021
hm dec -317 if hm >= -1485
x inc 462 if ih != -1032
qt dec 436 if yr == 409
uoc inc 289 if b <= 1638
qk inc -998 if uoc > -1045
j dec 387 if b < 1622
gy dec 13 if cp >= 24
bi dec -822 if js != 489
ioe dec -140 if xly < -2170
uoc dec -15 if ih < -1021
x dec -906 if hfh < 923
itw dec -986 if yr > 402
cp inc 474 if gy < -421
gy dec 530 if fi == 72
fi inc -277 if tc < 1214
ioe inc -26 if gif < -1066
itw dec -900 if ih < -1025
fi dec 443 if yg < -1719
hfh inc 602 if uoc == -1023
yg inc 409 if ih == -1027
t dec -366 if t != 1323
gif inc 158 if yr == 409
iyu inc 464 if hfh >= 1525
xly inc -71 if yg > -1319
cp dec -247 if qk > -4201
cp dec -278 if cp > 745
tc inc -903 if ioe < 257
itw inc 221 if bhp < -1870
yg dec -352 if j >= -31
ih dec -77 if qk < -4188
itw inc -921 if gy < -414
tc dec 565 if iyu != 2561
uoc inc -564 if cp <= 1035
ih dec -962 if qt == 1
hfh dec 646 if itw < 4222
js dec 604 if qk != -4186
hfh dec 389 if itw != 4214
cp inc -819 if bhp == -1876
tc inc 121 if qk <= -4185
cp dec 330 if t == 1677
nwe inc -864 if gy >= -417
uoc dec -459 if hm == -1155
x inc 886 if tc >= -124
tc inc 954 if yg != -973
qk inc 408 if x <= -563
hfh inc 37 if uoc == -1587
bhp dec -426 if cp == 197
nwe inc -46 if itw < 4233
j dec -546 if fi <= -637
bi dec 421 if xly > -2253
cp inc -991 if nwe >= -3417
gif inc -9 if hfh >= 1169
yg dec 397 if tc >= 816
gif inc 258 if js > -115
bhp dec -594 if tc != 822
t inc -975 if ioe != 253
tc inc 999 if ioe >= 242
gif inc 746 if yr < 419
gy inc -980 if yr > 407
qt inc 892 if cp > -789
itw inc 806 if yr != 402
hm dec 977 if iyu <= 2559
gy inc 848 if gy >= -1405
bi dec -310 if qt == 893
hfh inc -40 if yg == -1353
js inc -626 if j == 516
iyu dec 863 if yr < 408
bi inc 321 if b < 1633
qt inc 895 if hfh <= 1178
hfh dec -782 if js == -735
cp inc -443 if yr < 410
yg inc -446 if bi >= 2626
uoc inc -251 if tc < 1824
ioe dec 549 if yg < -1360
x dec -961 if yg <= -1362
gif inc -184 if js > -743
js dec 929 if x <= 397
yg inc 994 if yg == -1362
js dec -246 if bi < 2629
yg dec 875 if j <= 522
ih inc 287 if yg >= -1240
itw inc 615 if hfh == 1958
ih dec -665 if qt <= 1790
itw inc -747 if qt == 1788
iyu inc -631 if tc > 1810
j inc 178 if bhp >= -1273
nwe dec 85 if itw >= 4897
tc inc 530 if cp > -1222
fi dec 329 if tc >= 1819
uoc inc 1 if iyu != 1932
t dec 905 if nwe > -3497
j inc -836 if ioe >= -299
xly dec 185 if nwe > -3499
b inc 41 if gif >= -92
iyu dec 315 if t > -196
nwe dec 676 if uoc != -1839
bi dec 380 if tc != 1819
ih dec -397 if gif != -102
yr dec 449 if js > -1420
hfh inc 584 if bhp <= -1282
ih inc -524 if ioe >= -304
gif dec 688 if hfh == 2542
hm dec -370 if bi == 2621
js dec 575 if hm != -1768
qk dec -85 if itw != 4893
xly inc 961 if tc == 1819
itw inc 913 if qk <= -3699
bi inc -858 if js != -1420
t dec 622 if bi != 1764
qt inc -980 if fi >= -964
fi dec -380 if js == -1418
qt dec -446 if bi != 1772
gy inc -414 if yr != -50
qt dec 628 if itw < 5813
bi inc -649 if fi >= -588
uoc dec 821 if iyu >= 1916
gy dec 19 if xly > -1472
qt dec 137 if qt <= 1608
bhp dec -850 if t >= -830
bhp inc 19 if iyu != 1928
yr dec 272 if ih == 153
js inc 628 if hm > -1775
qk inc 893 if b != 1630
fi dec -959 if gif < -783
x inc 848 if bhp <= -405
bhp dec -555 if j != -318
gif dec -906 if hm > -1772
itw dec -722 if hfh == 2542
bhp dec 960 if j != -327
js dec 670 if gif >= 123
itw inc 153 if yr > -313
t dec -712 if ioe == -299
ioe inc 345 if itw == 6693
hfh dec -411 if bi <= 1123
hfh dec 368 if qt == 1469
ioe dec -688 if itw != 6684
cp inc 888 if bhp != -818
iyu dec 184 if fi != 378
xly inc 143 if fi == 371
cp dec 691 if ih <= 151
bi dec -712 if xly != -1333
nwe inc 231 if bhp < -808
qt dec -736 if fi != 361
hm inc 536 if gy < -988
yr inc -539 if j < -311
xly inc -311 if bi <= 1833
qk inc 271 if yg != -1237
uoc dec -40 if gif < 119
nwe inc 223 if yg >= -1239
qt inc 501 if itw <= 6685
xly inc 11 if t >= -109
hm dec 147 if j != -328
js inc -15 if hm == -1379
bhp inc -841 if fi > 362
fi inc -26 if ioe != 397
fi dec 283 if tc > 1817
uoc inc 105 if j < -314
ioe inc -885 if gif < 113
xly dec -849 if iyu == 1746
j inc -614 if yg != -1243
yg inc 115 if fi <= 62
itw inc -405 if cp != -1222
hm inc 854 if gy >= -988
tc dec 143 if cp >= -1232
gy dec -111 if b < 1635
ioe inc -939 if hfh == 2585
gif dec 43 if bi < 1831
gif dec 519 if itw < 6284
nwe inc 197 if gif != -449
bi dec -71 if bi != 1835
ih inc 479 if hfh >= 2595
ih inc -979 if qk <= -3438
iyu dec 653 if uoc == -2513
yr dec -346 if hfh <= 2585
gif inc 572 if xly != -1638
qk dec -891 if j > -322
qk dec -911 if ih == 150
tc inc 995 if iyu <= 1094
hfh inc -129 if x == 1241
xly dec -533 if gy > -884
gif dec 806 if b == 1630
hfh dec -805 if hfh >= 2453
iyu inc 242 if qt < 2714
fi dec 878 if bhp <= -1661
t dec 166 if hm == -1379
ioe inc -456 if hm == -1379
hm dec 225 if cp >= -1228
xly dec 820 if qt <= 2707
tc dec -786 if bi != 1898
uoc inc 751 if qk != -2547
b dec 922 if x >= 1238
j dec -179 if b <= 715
hfh dec -590 if qk == -2539
x dec 187 if itw == 6280
bi dec -967 if js < -804
tc inc 133 if qt <= 2713
t dec -411 if tc <= 3596
uoc inc 264 if fi < 61
gy inc 223 if ioe <= -1000
gif dec -942 if ioe >= -1008
hfh inc 999 if gy > -662
fi inc -485 if ih <= 153
nwe dec 733 if ioe > -1015
x inc -753 if bhp <= -1653
js dec 784 if gif < 263
bhp dec -307 if b != 703
itw dec -757 if ih == 153
iyu dec 785 if bi >= 2874
nwe inc -450 if t == 136
bi dec -77 if yg > -1132
js dec 990 if yr > -515
iyu inc 337 if bi > 2933
uoc dec 241 if ih != 153
bi dec 459 if hm == -1604
iyu inc -85 if bhp > -1353
qk inc -437 if hm != -1597
x inc -852 if qt <= 2708
gy dec 311 if hm != -1604
iyu dec 876 if t <= 143
x dec 779 if yr <= -513
hfh inc -843 if qt < 2707
bi inc 536 if yg < -1120
itw inc -886 if hfh != 4003
js inc 107 if hfh <= 4009
x dec 156 if yr >= -504
hfh dec 971 if j < -140
tc inc -480 if hm != -1595
x inc 993 if j > -142
tc inc 910 if yg != -1120
tc dec 571 if x != 432
x dec -194 if nwe > -4935
yr inc 288 if b <= 717
fi inc 970 if tc >= 3454
xly dec -554 if fi <= -431
ih inc -380 if iyu <= 706
qk dec 162 if nwe == -4925
fi dec 396 if gif != 261
xly inc 698 if yr >= -224
qt dec 57 if hm >= -1607
tc inc 555 if x <= 644
x inc -20 if xly == -1217
ih dec -766 if ioe <= -1005
tc dec -101 if gy != -660
hm dec 989 if ioe == -1006
yr inc 995 if gy != -651
cp inc -70 if cp < -1232
gy inc -408 if hfh > 3027
yg dec 360 if bhp >= -1352
hm inc 263 if qk < -3137
qt dec -603 if yg > -1495
xly inc 516 if js <= -2466
ioe inc -442 if uoc >= -1764
ih inc 898 if gy < -1053
gif dec 863 if tc >= 4114
hfh dec 478 if hfh != 3030
js dec -623 if hm > -2328
itw dec 792 if qt > 3244
yg inc -367 if tc == 4105
gy inc -330 if b <= 709
x inc 821 if t == 136
bi inc -979 if cp >= -1235
nwe dec -608 if qt == 3252
cp inc 885 if yr == 778
js dec 197 if b > 717
ih inc -737 if qt != 3248
iyu dec -570 if tc < 4107
x dec 63 if bi == 2039
t dec 323 if j < -133
tc inc -962 if tc > 4098
ih inc 663 if xly == -701
ih inc 489 if ih >= 1363
uoc inc -393 if js >= -2480
j inc -62 if cp == -342
gy dec -29 if bhp < -1348
yg inc -202 if nwe >= -4322
fi dec -628 if bi == 2039
itw inc -338 if itw > 5350
qt dec 24 if qt > 3242
tc inc 877 if t == -188
nwe inc 521 if ioe != -1448
gif inc -328 if gif > 261
ih inc -277 if bhp != -1351
tc inc -61 if qk > -3148
nwe inc 588 if xly != -702
qk dec -762 if qk >= -3141
gif inc 937 if b <= 708
t inc -749 if fi == -191
js inc 814 if qk == -2376
j inc -479 if bhp < -1346
iyu inc -941 if gif == 876
js dec 411 if hm > -2326
itw inc -832 if gy == -1364
iyu dec 537 if xly != -706
yg inc 687 if js == -1658
b inc 741 if tc > 3073
x dec 86 if ih < 1584
fi inc 851 if t > -940
x dec 680 if qk >= -2373
gy inc 882 if itw != 4183
tc dec 711 if fi < 658
yg dec -166 if qt == 3228
yg dec 684 if fi > 661
bi inc -715 if gif < 875
gy dec 168 if nwe != -3719
gif inc 301 if yg != -1204
ioe inc 293 if cp >= -344
yr dec -395 if bhp <= -1351
qk inc 97 if js > -1659
hm inc 371 if gif > 865
gif inc 443 if uoc >= -2151
nwe dec -677 if cp > -351
qk dec -845 if yg != -1201
ih dec -890 if cp != -337
nwe dec -932 if hfh > 2549
xly dec -281 if ih <= 2473
cp dec 76 if j == -682
hfh dec 236 if gy == -650
uoc inc 804 if b != 1448
iyu dec -791 if yg <= -1205
yr inc -720 if bi == 1324
b inc -234 if yg == -1204
bhp dec 728 if t != -938
qk dec 703 if itw == 4194
hfh dec 944 if uoc <= -1360
yg dec -384 if hfh < 2324
hm inc -765 if fi == 660
b inc -769 if hfh == 2322
ioe dec 681 if b >= 455
iyu inc -795 if itw > 4180
bhp dec -533 if t < -933
j inc 861 if ih < 2475
ioe inc 136 if itw < 4189
b dec 400 if fi >= 663
fi inc -483 if gif < 868
bhp dec 815 if qt == 3228
x inc -236 if yr > 456
xly inc 516 if b > 443
bhp inc -95 if x != 1295
uoc inc 174 if js > -1668
bi inc 126 if b <= 454
uoc inc -742 if gy >= -655
ih dec -381 if qt > 3235
itw inc -493 if bi <= 1449
fi inc -673 if fi <= 653
ih inc 55 if x >= 1284
tc inc 55 if fi == 657
ioe inc -715 if yg <= -820
nwe inc -665 if qk != -1432
qt inc -129 if b <= 443
fi inc 984 if yg <= -819
cp dec 112 if bi < 1454
j dec -268 if hm <= -2729
fi inc -620 if itw <= 4191
nwe dec 330 if tc != 3077
xly dec -490 if bi == 1450
cp dec 922 if yg >= -825
itw inc -752 if uoc < -1914
yg inc 502 if tc == 3082
hm inc 987 if qt > 3226
qk inc -234 if bi <= 1451
ioe inc 201 if iyu > -64
fi dec 730 if xly <= 591
js inc 506 if qk <= -1661
x dec 453 if yr <= 455
t inc -941 if uoc > -1923
hm inc 30 if bi < 1458
gy inc 947 if fi > 292
hm dec 976 if iyu == -56
tc inc -420 if qk <= -1667
tc dec -779 if gy < 302
bi inc -814 if x < 845
qt dec 484 if bi > 643
yr inc 363 if fi >= 290
ioe inc -314 if gif > 866
ioe inc 84 if yg < -312
bhp dec -941 if itw >= 3447
gif inc -791 if ioe >= -1898
yg dec -440 if ioe != -1904
b dec -673 if x != 835
tc dec -547 if qt <= 3233
t inc 641 if yg < 120
xly dec 548 if fi < 293
iyu dec -947 if hm >= -2687
gif inc 643 if yg > 119
iyu dec 681 if j >= 174
ioe dec 326 if cp == -1452
cp dec 489 if js == -1151
bi dec -814 if js != -1152
tc inc -803 if yr >= 812
qk inc 155 if itw >= 3429
bhp inc -445 if tc < 3188
itw inc -280 if js > -1147
js inc -921 if ioe > -2227
gy inc 200 if b == 446
js inc -943 if nwe > -3125
gif inc -412 if hfh == 2322
t inc 240 if ioe == -2225
iyu inc 774 if b != 440
yr dec -305 if ih != 2522
ih dec -24 if uoc > -1928
yr inc 770 if gif != 1102
iyu inc 440 if yg != 115
gy inc 148 if qt >= 3226
ih inc 534 if tc != 3182
yr inc -140 if js > -3019
ih dec 369 if bhp < -2894
x dec -155 if uoc <= -1915
x inc -627 if nwe < -3114
yr dec -446 if tc != 3180
cp dec -638 if xly <= 593
gy inc -97 if x <= 370
qk inc -769 if fi == 298
hfh dec 365 if bhp < -2906
j dec -533 if xly != 586
x dec 24 if uoc < -1910
fi inc 391 if tc <= 3182
yg inc -329 if b >= 440
ih dec -722 if t >= -1633
qk dec -517 if x >= 340
gif inc 404 if qt != 3225
x inc -52 if ih >= 2716
hm dec 759 if js < -3008
nwe dec -356 if yg >= -212
ioe inc -337 if x == 332
xly inc 610 if cp < -811
j dec 778 if j >= 178
qk dec -477 if ioe != -2231
gy inc 61 if b < 451
hfh inc -726 if gy <= 615
iyu inc -899 if itw < 3441
ih dec 980 if qt >= 3232
js inc 165 if yr != 1425
js dec -668 if yg == -207
b dec 584 if x < 339
qk dec -606 if cp < -806
gif inc 269 if hm <= -3437
js dec -472 if t > -1638
fi inc -853 if xly < 1197
gif inc -716 if iyu != 522
cp inc -536 if itw < 3440
hfh dec -848 if hfh <= 1594
hfh inc -135 if iyu <= 525
fi dec -789 if hfh > 1459
bhp dec -689 if uoc == -1919
hfh inc -507 if ioe <= -2221
yg dec 440 if ih > 2714
qk inc 99 if bhp != -2213
qk dec -330 if hfh >= 958
j inc -946 if ih != 2717
bhp dec -107 if gy < 619
cp dec -797 if t >= -1641
cp dec 615 if fi <= 228
nwe dec 502 if t != -1641
hm dec -58 if hm < -3433
t inc -324 if nwe < -3257
hfh dec 754 if gif > 1067
hm dec 2 if ih == 2709
bi dec -992 if fi == 230
t dec 646 if ih == 2709
qk dec -549 if nwe == -3261
tc dec -772 if nwe <= -3260
fi dec 640 if fi == 230
ioe dec 921 if tc >= 3957
tc dec 79 if tc <= 3964
tc inc 541 if yr == 1427
qk dec 331 if cp >= -546
ih dec 561 if itw >= 3438
itw inc 558 if qk != 119
hfh inc 809 if hfh >= 948
itw dec 981 if xly < 1197
ih dec -431 if js > -1708
yr dec -737 if hfh != 1754
tc inc 873 if ih != 2709
bhp dec -53 if hm != -3395
fi inc -207 if j >= -1541
bi inc -484 if itw >= 2449
js inc 548 if j > -1549
tc inc 336 if nwe != -3257
gy dec 39 if hm == -3386
gif inc 186 if itw > 2450
hm dec 671 if fi > -404
bi inc 950 if qt > 3227
gif inc 667 if fi > -414
x inc 380 if qk != 111
t dec -194 if js > -1172
yr inc -954 if itw < 2449
bhp inc 61 if j <= -1539
x inc -893 if bhp > -1997
ih dec -169 if iyu > 522
b dec 211 if cp >= -548
gif inc -278 if fi < -406
js dec 22 if t < -2404
ih dec -350 if nwe <= -3255
yg dec 198 if qk != 116
fi inc -466 if bi != 2091
yr inc -28 if yg == -405
cp dec 165 if xly < 1201
hm inc 875 if bhp >= -1995
qk inc 856 if gy > 565
hm inc -20 if iyu == 525
hfh dec -771 if itw > 2448
ioe dec 162 if fi == -874
yg inc -662 if ih < 3233
yr inc -214 if t == -2413
xly dec -251 if yg != -1065
itw inc 361 if ih < 3234
qk inc 206 if yg <= -1060
t inc 398 if ioe == -3146
qk dec 35 if b > 437
itw inc -841 if xly != 1445
hfh inc -461 if yr == 1922
hm dec -698 if itw != 1968
fi dec -98 if bhp < -1986
t inc 224 if hfh < 2080
j inc -767 if hm == -1833
uoc dec -817 if j > -2320
qt inc -801 if tc < 4761
hfh dec 597 if iyu == 525
itw dec 670 if cp != -709
ih inc 173 if bhp <= -1983
js inc 904 if hm == -1833
ih dec 913 if bi >= 2091
x inc 841 if itw < 1297
t dec 74 if gif == 1634
j dec 302 if yr != 1916
xly dec -815 if bi > 2092
qk inc -349 if x <= -168
xly inc 974 if fi >= -783
ih inc -251 if t == -1865
ioe dec 314 if nwe != -3253
fi dec -245 if ih < 2239
js dec -69 if js <= -272
yg inc -33 if j != -2623
xly dec -146 if ioe < -3451
bhp dec 757 if itw < 1309
qt inc -472 if yr >= 1918
xly dec 662 if hfh < 1477
nwe dec 66 if iyu <= 528
gy inc 47 if j > -2623
bhp dec -567 if bhp < -2746
gy inc 968 if nwe >= -3333
t dec -731 if iyu >= 519
gif dec -522 if j > -2621
qk inc -855 if tc > 4754
gif inc 125 if gif <= 2159
iyu dec -400 if hfh >= 1476
nwe dec -751 if qk >= -67
hm inc -645 if ioe != -3466
bi inc -235 if nwe >= -2571
b dec 981 if yr >= 1922
gy dec -452 if hm <= -2475
fi dec -532 if qt <= 1959
cp inc 612 if hm >= -2487
fi dec -455 if bi < 2097
t dec 80 if iyu <= 934
xly inc -694 if hm == -2478
uoc dec -759 if qk >= -59
ih dec 244 if yr >= 1918
tc inc 638 if ih > 1998
cp inc 1 if fi < 458
cp inc 454 if nwe > -2573
gy inc -134 if hm < -2471
yg dec 1 if iyu >= 919
js inc 91 if yg < -1108
yg inc 173 if ioe <= -3458
gy dec -760 if t != -1214
qk dec 816 if iyu < 920
gy dec 226 if itw < 1301
bi dec 704 if tc == 4755
iyu inc -661 if nwe != -2572
nwe inc -425 if qt < 1963
gy inc 197 if qk == -58
t inc 853 if nwe == -3001
js dec 603 if yg >= -928
j dec 708 if xly == 2026
hm dec 836 if b == -535
x dec -132 if itw > 1299
cp inc 587 if gif > 2278
t dec -715 if nwe <= -2994
b inc 63 if yr == 1922
tc inc 147 if bi > 1383
yg dec -24 if cp <= 476
gy dec -11 if j == -3322
bhp inc -488 if tc >= 4903
iyu inc 890 if hfh >= 1476
qk inc 224 if yr <= 1928
bhp dec 845 if itw > 1303
uoc inc 965 if qk != 166
fi dec -186 if cp == 482
t dec 388 if js >= -814
hm dec -157 if t < 357
hfh inc -353 if nwe == -3008
x dec 846 if cp >= 491
fi inc -436 if iyu <= 1154
qk dec 465 if bhp == -3027
yg inc -489 if t <= 354
js inc 809 if gif >= 2284
iyu inc 757 if hfh != 1479
uoc inc -765 if yg != -1415
bi inc 637 if nwe == -3005
ih dec 369 if ih == 1993"
  end
end

defmodule Santa.Day10.Input do
  def input() do
    "94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243"
  end
end

defmodule Santa.Day11.Input do
  @doc false
  def input() do
    "s,sw,s,ne,s,se,s,s,s,se,se,se,se,ne,ne,n,ne,ne,ne,ne,ne,s,ne,ne,ne,ne,n,n,n,n,n,n,se,n,n,n,se,s,ne,n,n,nw,n,n,n,nw,n,s,n,n,n,n,s,s,nw,se,nw,s,n,nw,nw,nw,nw,ne,sw,nw,nw,nw,n,se,nw,nw,n,s,n,sw,sw,nw,sw,nw,nw,nw,se,ne,sw,n,sw,sw,sw,n,sw,sw,sw,ne,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,se,nw,sw,sw,s,sw,s,s,s,s,sw,sw,sw,se,s,sw,s,n,sw,ne,s,s,n,nw,s,s,s,s,s,s,se,s,sw,s,sw,sw,s,s,s,s,s,s,s,nw,s,se,se,s,se,s,s,s,se,n,nw,se,s,se,s,s,n,se,ne,s,s,s,s,s,s,ne,se,s,nw,se,s,s,nw,s,s,se,s,s,se,se,se,se,se,s,se,se,se,s,s,se,se,s,se,se,se,se,se,s,se,se,s,se,se,s,s,se,s,se,se,s,se,n,s,sw,se,se,se,se,se,ne,se,se,sw,nw,ne,se,se,ne,se,se,ne,ne,se,ne,se,se,ne,se,s,se,ne,se,se,ne,n,se,se,sw,ne,se,se,se,ne,se,ne,nw,ne,nw,se,ne,se,se,se,se,se,nw,se,ne,s,nw,se,ne,ne,ne,ne,n,se,s,nw,ne,se,ne,se,ne,ne,ne,ne,ne,ne,ne,nw,ne,ne,ne,ne,ne,n,se,sw,ne,se,se,ne,s,se,ne,ne,ne,ne,ne,ne,ne,s,ne,ne,n,ne,ne,ne,ne,ne,ne,ne,ne,sw,ne,n,ne,n,ne,ne,sw,n,ne,ne,ne,s,ne,ne,ne,ne,ne,ne,ne,n,ne,ne,n,sw,s,ne,n,ne,ne,nw,ne,ne,se,n,n,ne,n,ne,ne,ne,ne,n,nw,ne,ne,ne,ne,ne,n,ne,n,n,ne,sw,n,ne,ne,ne,n,n,n,sw,n,n,se,ne,n,ne,n,sw,n,n,ne,n,ne,n,n,n,ne,ne,nw,nw,ne,nw,s,se,n,n,n,n,n,ne,n,nw,n,n,sw,sw,se,se,ne,n,se,nw,sw,ne,n,nw,ne,n,n,n,se,se,ne,s,n,nw,n,se,n,ne,ne,se,ne,n,n,sw,ne,ne,n,n,n,n,n,se,sw,n,n,n,n,n,nw,n,n,n,n,s,n,sw,n,n,n,n,n,nw,sw,n,n,n,n,n,n,ne,n,ne,s,n,n,n,n,n,n,se,n,n,n,se,n,n,n,se,s,n,n,n,nw,n,n,n,n,n,nw,sw,n,n,n,n,n,n,nw,n,n,nw,n,nw,n,n,nw,se,n,sw,n,nw,nw,s,se,nw,nw,nw,nw,nw,sw,se,nw,n,nw,n,nw,n,nw,nw,n,nw,n,s,nw,s,n,nw,n,n,nw,n,n,nw,n,sw,n,nw,nw,n,nw,n,n,n,nw,n,nw,s,n,nw,n,sw,nw,n,nw,se,ne,nw,n,nw,n,nw,se,n,n,nw,se,nw,nw,n,s,n,n,nw,nw,sw,nw,nw,ne,nw,nw,n,nw,nw,nw,nw,nw,nw,n,n,nw,nw,nw,nw,ne,sw,nw,n,s,nw,nw,n,n,sw,nw,n,nw,s,s,n,n,nw,nw,nw,s,nw,nw,nw,s,nw,nw,nw,n,nw,n,nw,nw,nw,nw,nw,nw,ne,nw,se,nw,se,nw,nw,n,nw,nw,ne,nw,nw,se,sw,sw,s,nw,nw,nw,nw,nw,nw,nw,nw,nw,sw,nw,nw,nw,sw,nw,nw,s,nw,nw,nw,nw,ne,nw,nw,ne,s,nw,nw,nw,sw,nw,n,ne,nw,nw,sw,nw,nw,ne,nw,sw,sw,nw,nw,ne,sw,se,nw,nw,nw,n,nw,nw,sw,nw,s,sw,sw,nw,nw,nw,nw,nw,n,nw,nw,ne,sw,sw,nw,sw,nw,se,sw,nw,nw,nw,nw,nw,sw,nw,sw,nw,nw,sw,sw,nw,nw,nw,sw,nw,sw,sw,sw,ne,nw,nw,n,nw,sw,n,nw,ne,nw,s,nw,nw,sw,nw,n,nw,sw,nw,nw,sw,n,nw,sw,ne,sw,s,sw,sw,sw,sw,sw,nw,nw,nw,nw,se,sw,nw,sw,nw,sw,nw,nw,nw,sw,ne,nw,nw,nw,sw,nw,nw,ne,se,sw,nw,nw,nw,sw,sw,sw,nw,sw,sw,nw,sw,nw,sw,nw,nw,nw,nw,nw,s,sw,nw,nw,se,n,se,se,sw,nw,nw,nw,sw,nw,ne,nw,nw,sw,sw,n,nw,s,sw,sw,s,nw,sw,sw,nw,nw,sw,sw,nw,nw,nw,sw,ne,sw,se,ne,sw,nw,nw,sw,ne,sw,sw,sw,sw,se,sw,sw,sw,nw,sw,sw,nw,nw,sw,sw,sw,sw,se,sw,nw,sw,sw,n,sw,sw,nw,sw,sw,se,se,sw,sw,sw,sw,sw,sw,sw,sw,se,sw,sw,sw,s,sw,sw,se,sw,sw,sw,sw,sw,sw,sw,nw,sw,sw,sw,sw,sw,se,sw,sw,n,nw,sw,sw,sw,sw,s,sw,sw,sw,se,se,n,ne,sw,sw,sw,sw,sw,se,sw,sw,sw,sw,sw,ne,ne,s,sw,sw,n,nw,sw,se,sw,sw,sw,sw,sw,sw,sw,sw,sw,nw,sw,n,sw,nw,sw,s,sw,sw,sw,sw,sw,s,sw,sw,sw,sw,sw,sw,sw,sw,sw,s,sw,sw,nw,s,s,se,sw,sw,sw,sw,n,n,sw,s,s,sw,se,s,sw,sw,sw,n,n,sw,sw,nw,ne,nw,nw,s,sw,s,s,sw,sw,sw,se,sw,s,sw,sw,sw,s,sw,s,ne,sw,sw,s,sw,sw,sw,sw,ne,s,s,s,n,s,se,sw,sw,s,sw,sw,s,s,sw,nw,sw,sw,se,ne,s,s,sw,sw,ne,sw,sw,se,sw,s,sw,sw,sw,sw,s,n,sw,ne,sw,sw,s,sw,sw,sw,sw,se,s,s,sw,s,s,sw,sw,n,s,sw,s,sw,ne,s,sw,sw,s,sw,sw,s,s,sw,sw,sw,s,sw,se,s,s,sw,s,s,se,sw,s,s,sw,s,sw,sw,sw,s,s,s,s,ne,s,sw,sw,nw,n,sw,sw,sw,s,sw,sw,sw,n,s,se,s,sw,s,s,sw,ne,sw,s,s,nw,sw,s,n,nw,sw,sw,s,sw,s,sw,s,s,s,s,s,sw,sw,s,s,sw,sw,se,s,sw,se,sw,s,se,n,s,se,s,s,sw,s,sw,s,ne,sw,s,ne,s,sw,sw,s,sw,s,s,s,sw,ne,s,se,s,nw,s,s,sw,s,s,ne,sw,s,s,sw,sw,s,se,s,sw,sw,sw,sw,s,s,sw,nw,n,n,n,sw,s,sw,se,s,s,s,s,sw,ne,sw,s,sw,nw,s,s,sw,s,s,s,nw,s,sw,s,s,s,s,s,s,s,s,s,s,se,se,se,s,s,sw,s,s,n,s,ne,s,nw,se,s,s,nw,nw,n,s,s,s,sw,se,s,s,s,s,n,s,s,s,s,s,sw,s,s,s,s,s,s,s,s,ne,s,nw,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,nw,s,s,s,s,s,nw,s,s,s,sw,s,s,s,n,sw,s,s,s,s,s,se,s,s,s,s,s,s,s,s,sw,s,s,s,se,s,n,s,nw,s,s,n,sw,s,se,s,se,s,sw,n,s,s,s,s,s,s,s,s,n,s,s,s,s,s,se,s,nw,se,se,s,s,se,s,se,s,s,s,s,s,s,s,se,s,s,s,n,se,nw,s,se,s,s,s,s,nw,se,s,s,s,se,se,s,s,nw,s,s,n,s,s,se,n,se,s,s,se,s,ne,s,s,s,s,s,s,se,s,s,s,se,s,se,s,s,ne,s,s,s,s,s,s,s,s,se,s,s,se,s,se,s,s,s,s,se,s,se,s,se,n,s,se,se,s,s,s,s,s,s,s,se,ne,s,s,s,se,s,s,se,se,s,se,se,n,ne,s,se,se,s,se,se,se,se,se,ne,s,se,s,se,sw,sw,n,se,s,s,se,se,s,se,nw,s,s,s,se,se,s,s,s,s,se,s,s,se,s,s,se,s,se,se,s,se,s,se,se,nw,s,ne,se,se,s,se,ne,se,ne,se,s,nw,se,se,se,s,se,s,s,s,se,ne,se,se,se,se,s,s,se,se,se,se,s,se,n,nw,s,n,s,se,se,se,se,s,nw,s,s,s,se,n,s,se,nw,s,s,ne,se,se,se,s,s,sw,s,s,s,s,nw,sw,se,se,s,se,nw,s,s,se,s,n,s,s,se,s,ne,s,s,se,se,s,se,se,sw,se,s,s,s,se,se,ne,se,se,s,se,nw,s,se,n,se,se,s,se,se,s,se,ne,nw,s,s,se,se,sw,s,n,s,s,se,s,se,se,s,sw,se,s,ne,s,se,se,s,se,se,s,se,nw,se,s,se,se,s,s,se,s,nw,se,n,n,se,sw,n,n,se,se,s,s,se,se,se,n,se,nw,se,s,se,s,s,se,ne,s,se,se,nw,se,s,sw,nw,s,ne,se,se,nw,se,s,s,se,se,s,ne,s,se,s,se,sw,ne,sw,s,s,s,n,se,se,se,se,se,se,nw,ne,se,se,se,sw,se,s,se,se,nw,se,se,nw,nw,se,se,se,se,se,s,se,nw,nw,se,se,se,se,se,se,se,se,se,se,sw,n,nw,se,se,nw,n,se,se,se,s,se,sw,s,se,se,se,sw,se,nw,n,s,se,n,n,nw,se,se,se,nw,se,se,se,n,se,se,se,ne,se,sw,s,se,se,se,se,se,se,se,se,se,se,n,se,se,se,n,s,s,n,se,se,se,se,se,se,se,se,s,n,se,se,se,se,se,sw,se,se,se,se,n,se,se,se,sw,se,ne,se,se,sw,se,se,se,n,sw,se,sw,se,ne,se,se,se,se,se,se,se,se,ne,se,sw,se,se,se,se,se,se,se,nw,ne,se,se,se,s,se,se,se,se,se,se,s,se,nw,nw,se,nw,se,nw,se,se,se,se,se,se,se,se,se,s,se,s,se,se,se,se,se,se,se,nw,se,sw,se,se,n,se,se,se,s,se,se,se,se,se,se,se,s,ne,se,se,ne,se,se,se,ne,se,se,se,se,se,se,se,se,se,se,sw,se,ne,ne,se,se,se,se,se,se,nw,se,se,se,se,se,nw,se,sw,ne,se,se,se,se,se,se,se,ne,se,se,se,se,ne,s,se,se,se,se,se,se,nw,se,se,ne,se,se,se,se,se,ne,s,se,ne,se,se,se,se,se,se,se,ne,nw,ne,se,se,se,se,se,se,nw,se,se,nw,se,se,n,se,se,ne,se,se,se,se,se,se,se,se,nw,ne,se,se,se,se,ne,se,se,ne,se,ne,se,n,se,se,se,se,s,se,se,se,nw,ne,se,se,se,ne,se,ne,se,n,ne,s,sw,s,se,ne,s,se,s,ne,se,n,se,se,n,se,se,ne,ne,se,se,se,s,ne,se,se,ne,se,se,se,ne,ne,se,se,se,ne,se,se,se,se,ne,ne,ne,se,ne,se,n,ne,se,se,sw,se,se,se,ne,se,s,se,se,se,ne,se,se,se,se,se,n,ne,s,se,se,se,se,s,ne,se,ne,se,se,n,ne,ne,ne,se,se,se,se,ne,se,se,ne,se,s,se,se,s,ne,ne,se,se,se,se,ne,ne,se,ne,se,se,sw,ne,se,ne,ne,se,s,ne,n,ne,se,ne,se,se,se,se,se,se,se,ne,ne,ne,se,ne,se,ne,se,se,se,se,ne,ne,ne,ne,se,ne,ne,se,ne,sw,ne,ne,ne,se,se,nw,ne,se,se,ne,ne,se,n,sw,ne,se,ne,ne,ne,ne,se,nw,nw,se,ne,ne,se,sw,ne,s,ne,se,ne,se,ne,se,se,ne,ne,se,se,sw,n,n,ne,se,se,n,n,ne,se,se,se,n,se,n,se,ne,se,nw,se,ne,s,se,ne,se,se,se,se,se,se,se,se,se,se,ne,ne,sw,ne,se,ne,se,ne,ne,se,n,se,ne,ne,se,se,ne,ne,ne,ne,se,se,ne,nw,se,ne,se,sw,s,se,nw,se,se,se,se,se,ne,ne,nw,se,se,se,se,sw,ne,ne,se,s,se,ne,sw,se,sw,ne,sw,ne,ne,ne,s,se,ne,ne,ne,n,ne,nw,se,ne,ne,se,se,ne,n,n,sw,ne,se,se,ne,sw,s,ne,se,ne,ne,ne,se,sw,ne,se,s,ne,ne,n,ne,ne,se,nw,ne,n,ne,ne,se,ne,ne,nw,s,sw,nw,se,sw,s,ne,ne,s,s,ne,ne,sw,sw,se,ne,ne,ne,ne,se,ne,ne,sw,ne,sw,ne,ne,se,se,s,se,ne,s,ne,ne,ne,ne,ne,ne,ne,s,se,se,sw,nw,n,se,se,ne,ne,ne,ne,sw,ne,ne,ne,se,se,s,nw,nw,n,se,ne,s,ne,ne,ne,ne,n,se,ne,se,ne,ne,ne,se,ne,sw,ne,se,nw,ne,n,sw,ne,sw,ne,s,ne,ne,nw,sw,ne,ne,ne,s,n,se,ne,ne,sw,nw,ne,ne,ne,ne,ne,ne,ne,se,ne,ne,sw,ne,se,ne,n,ne,se,se,sw,s,se,ne,se,ne,ne,se,ne,ne,ne,s,nw,ne,ne,ne,se,ne,se,ne,ne,ne,nw,se,ne,ne,nw,se,ne,ne,nw,se,ne,ne,nw,se,ne,n,ne,ne,ne,ne,s,ne,se,se,se,ne,n,ne,ne,ne,sw,ne,ne,ne,ne,ne,ne,nw,ne,ne,ne,ne,ne,sw,ne,se,se,ne,ne,n,ne,ne,s,ne,ne,ne,ne,ne,nw,ne,sw,s,ne,ne,ne,ne,ne,n,ne,ne,ne,ne,ne,ne,n,ne,ne,ne,ne,ne,ne,nw,se,ne,se,ne,ne,se,se,nw,sw,ne,ne,ne,ne,ne,ne,ne,ne,ne,n,ne,ne,ne,s,n,ne,ne,ne,ne,ne,ne,sw,ne,ne,ne,ne,ne,se,ne,ne,ne,se,ne,nw,ne,ne,ne,nw,ne,ne,ne,ne,n,ne,sw,ne,ne,se,ne,n,ne,ne,ne,ne,se,ne,ne,sw,nw,n,ne,ne,nw,ne,ne,ne,ne,ne,ne,ne,ne,nw,ne,n,sw,ne,ne,ne,n,se,ne,ne,ne,ne,ne,ne,se,ne,n,ne,se,ne,n,n,ne,ne,ne,ne,ne,se,ne,ne,ne,nw,sw,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,se,n,s,ne,ne,sw,ne,ne,ne,se,ne,ne,ne,se,ne,se,s,ne,ne,ne,ne,n,n,nw,ne,ne,ne,ne,ne,ne,ne,ne,sw,ne,ne,s,n,ne,n,ne,ne,ne,ne,ne,ne,n,ne,ne,ne,sw,n,ne,n,ne,ne,sw,ne,ne,ne,n,ne,ne,n,ne,ne,ne,ne,ne,s,ne,n,ne,ne,ne,ne,n,n,ne,ne,ne,ne,ne,n,se,ne,n,n,ne,s,ne,ne,ne,ne,ne,ne,ne,s,ne,nw,s,ne,n,ne,n,n,n,ne,ne,ne,ne,n,ne,ne,n,ne,ne,n,ne,ne,n,ne,n,n,ne,ne,n,se,se,ne,n,ne,n,s,n,ne,nw,ne,ne,ne,ne,ne,ne,ne,ne,s,ne,n,n,n,ne,ne,ne,ne,ne,n,n,ne,n,ne,n,n,n,ne,ne,ne,ne,ne,ne,s,n,n,sw,n,n,ne,ne,ne,ne,n,ne,ne,n,ne,n,ne,ne,n,nw,ne,n,ne,ne,n,ne,ne,ne,n,n,ne,ne,ne,ne,n,ne,ne,nw,n,n,ne,ne,n,sw,n,ne,sw,s,ne,se,ne,nw,ne,n,ne,n,ne,ne,n,ne,n,ne,ne,ne,ne,ne,ne,nw,ne,nw,n,sw,n,ne,s,nw,se,ne,ne,sw,ne,sw,ne,ne,sw,ne,ne,ne,ne,n,n,ne,ne,n,ne,ne,n,sw,ne,n,ne,ne,n,n,s,sw,nw,n,ne,n,se,n,ne,ne,n,s,sw,n,ne,s,n,ne,ne,ne,se,se,ne,n,ne,n,ne,ne,ne,n,se,n,ne,ne,ne,ne,s,ne,n,n,n,n,ne,n,ne,n,ne,se,ne,sw,ne,s,n,ne,n,n,n,n,sw,n,sw,ne,n,ne,sw,ne,ne,se,ne,ne,ne,ne,n,s,n,se,ne,ne,n,n,sw,ne,ne,ne,n,ne,se,n,ne,ne,ne,ne,ne,sw,ne,ne,n,nw,ne,n,ne,n,n,n,n,ne,n,n,n,ne,ne,n,ne,ne,n,n,ne,n,ne,sw,n,ne,n,n,n,ne,ne,ne,ne,n,n,ne,ne,n,ne,se,n,n,ne,sw,n,n,ne,s,se,n,nw,sw,n,ne,ne,n,nw,ne,se,n,ne,ne,nw,ne,n,ne,ne,ne,n,n,n,s,ne,n,n,s,n,se,n,n,n,sw,ne,sw,n,n,n,ne,n,ne,n,n,nw,n,n,se,ne,n,n,n,n,n,n,ne,n,ne,n,n,n,ne,ne,n,n,ne,ne,n,s,n,n,n,n,n,ne,n,ne,sw,n,n,n,n,ne,n,ne,ne,ne,ne,n,s,n,ne,n,n,ne,s,n,ne,sw,n,ne,ne,n,ne,ne,sw,n,s,n,ne,n,ne,ne,n,ne,ne,sw,ne,ne,ne,nw,n,n,s,n,n,nw,n,sw,n,n,n,ne,sw,ne,ne,n,n,n,sw,n,n,n,sw,ne,s,s,n,ne,ne,n,n,se,n,ne,nw,s,ne,ne,n,n,n,n,n,n,n,sw,se,n,ne,n,ne,n,ne,nw,ne,n,n,n,n,n,s,n,n,n,ne,ne,n,sw,ne,nw,ne,n,n,n,sw,s,ne,ne,ne,ne,s,n,n,n,n,n,nw,ne,sw,n,ne,ne,ne,n,n,ne,sw,n,ne,n,n,sw,n,ne,n,se,n,n,n,ne,n,sw,n,n,n,n,n,n,n,ne,nw,n,s,n,se,ne,se,n,n,n,n,sw,n,n,n,ne,n,ne,ne,n,se,ne,n,ne,n,n,n,s,n,n,ne,n,ne,n,n,n,se,ne,ne,ne,ne,n,ne,ne,n,se,ne,s,n,ne,n,n,n,n,s,ne,n,se,ne,n,n,n,ne,n,n,n,n,n,n,n,n,n,n,n,n,n,n,ne,n,n,ne,ne,n,s,s,ne,n,n,ne,ne,ne,sw,sw,ne,n,nw,ne,n,n,ne,n,n,n,se,n,n,n,n,ne,n,n,n,se,n,nw,n,ne,n,sw,n,ne,ne,s,n,s,nw,ne,n,ne,sw,n,n,se,nw,n,se,n,se,n,ne,n,s,n,ne,n,ne,se,n,n,n,n,n,n,nw,ne,n,ne,n,n,n,ne,se,ne,ne,n,n,n,n,se,n,n,n,ne,n,ne,n,sw,n,n,n,n,sw,n,n,se,n,n,sw,n,n,n,se,n,n,n,ne,s,n,ne,ne,n,sw,n,n,n,n,n,ne,n,ne,n,n,n,n,ne,s,n,n,ne,sw,n,ne,n,s,n,ne,nw,s,ne,s,ne,sw,n,n,ne,n,ne,n,n,n,n,n,n,n,n,n,sw,n,s,n,n,n,sw,n,n,n,sw,n,n,ne,n,n,ne,n,n,se,n,n,n,n,n,n,n,se,nw,n,n,n,se,n,n,ne,nw,n,ne,n,n,n,n,n,n,n,ne,sw,n,n,ne,n,n,n,n,n,n,n,n,s,s,ne,n,n,n,ne,n,ne,n,ne,ne,n,sw,n,s,n,sw,sw,sw,n,nw,n,n,n,ne,n,n,n,n,nw,nw,n,ne,n,n,n,n,n,n,n,n,n,s,n,n,n,s,n,ne,n,ne,n,nw,n,n,nw,n,ne,sw,n,n,n,n,n,n,n,nw,n,n,se,n,n,se,n,n,n,n,n,nw,n,s,n,n,n,n,n,nw,n,n,s,n,n,n,n,ne,n,nw,n,n,n,n,n,n,nw,n,n,n,n,n,nw,n,n,n,n,n,sw,nw,n,n,n,n,n,nw,s,n,n,n,sw,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,se,nw,n,n,n,nw,n,n,n,ne,n,n,n,se,n,n,n,n,se,n,n,nw,nw,n,n,sw,n,nw,n,n,n,n,n,nw,n,n,n,n,nw,n,n,sw,ne,se,nw,n,s,n,n,sw,n,sw,n,n,n,nw,n,n,n,s,n,n,n,n,n,n,sw,n,nw,sw,sw,n,s,n,n,n,n,n,s,n,n,n,n,n,nw,n,sw,nw,se,n,n,n,n,ne,se,n,n,nw,n,n,n,n,n,n,se,n,n,n,s,n,se,nw,n,nw,n,nw,n,nw,n,n,n,n,ne,n,s,n,s,n,n,ne,n,n,sw,n,n,n,n,n,n,ne,nw,n,n,n,n,nw,ne,n,n,n,n,n,n,n,n,n,sw,sw,n,n,n,nw,sw,n,n,n,n,n,n,n,n,n,n,n,n,se,n,n,n,n,se,n,n,ne,n,n,ne,nw,n,n,n,nw,n,n,n,n,nw,nw,n,n,n,nw,nw,n,n,s,n,n,nw,nw,n,n,nw,n,n,nw,n,nw,se,n,nw,n,s,n,n,ne,n,sw,se,n,n,n,nw,n,n,n,ne,n,s,nw,n,ne,ne,n,n,s,n,n,s,n,n,n,nw,sw,n,nw,n,s,nw,nw,nw,nw,ne,n,n,n,n,n,n,n,nw,nw,n,n,s,n,n,nw,n,n,n,nw,n,n,se,nw,n,s,nw,n,nw,n,n,ne,n,n,n,n,n,n,n,n,n,nw,nw,se,n,n,ne,nw,n,n,n,n,n,n,nw,n,nw,n,se,n,n,n,n,s,ne,ne,nw,n,n,n,n,nw,n,nw,n,n,s,n,nw,nw,n,ne,n,n,ne,nw,sw,nw,n,s,n,nw,n,n,n,sw,n,n,n,n,s,n,n,nw,ne,n,nw,se,nw,n,s,n,n,sw,n,n,n,n,sw,nw,nw,n,n,n,n,n,n,n,n,nw,n,nw,n,n,se,ne,nw,se,n,sw,n,nw,sw,n,s,sw,nw,nw,n,n,nw,n,n,nw,n,n,n,n,n,n,se,nw,n,n,nw,nw,s,n,se,n,n,nw,n,nw,nw,nw,se,nw,n,n,n,ne,nw,n,sw,n,n,n,nw,nw,nw,ne,nw,nw,n,n,n,nw,ne,n,nw,nw,n,nw,se,n,n,nw,nw,n,n,n,nw,n,n,n,n,se,n,n,n,n,n,n,nw,nw,nw,n,n,n,sw,n,n,n,s,n,n,s,s,nw,nw,n,se,se,n,nw,n,n,nw,sw,s,n,nw,nw,n,sw,n,n,ne,n,sw,nw,n,n,s,nw,nw,n,s,nw,s,n,nw,nw,n,sw,nw,nw,n,n,n,nw,n,n,n,nw,n,n,n,n,n,nw,nw,s,nw,nw,n,n,se,n,n,nw,n,n,n,se,n,n,n,n,nw,ne,n,n,ne,n,n,n,n,n,nw,n,nw,se,n,nw,n,nw,n,nw,se,n,n,se,nw,n,n,n,nw,sw,n,s,n,nw,n,n,n,n,n,n,n,s,s,n,nw,se,nw,se,nw,se,n,nw,nw,n,sw,n,n,nw,sw,ne,n,nw,nw,s,s,n,n,sw,nw,se,nw,nw,sw,n,se,nw,ne,sw,nw,nw,sw,nw,n,n,n,n,n,n,nw,n,n,n,n,nw,nw,n,n,sw,n,nw,nw,n,se,se,nw,nw,n,n,n,se,nw,nw,n,nw,nw,nw,ne,n,nw,nw,n,s,se,n,n,nw,nw,n,nw,nw,n,n,nw,nw,nw,sw,se,n,n,n,s,n,nw,nw,sw,n,nw,n,n,n,s,ne,nw,nw,s,n,n,nw,n,n,s,se,n,n,n,nw,n,nw,nw,nw,nw,nw,nw,n,nw,n,sw,nw,ne,nw,nw,n,s,nw,n,nw,nw,ne,nw,ne,nw,n,nw,nw,sw,n,nw,nw,s,s,ne,sw,n,se,nw,n,nw,nw,sw,nw,n,n,n,nw,n,nw,nw,n,nw,n,s,n,s,n,n,n,n,nw,nw,n,ne,n,se,nw,nw,nw,nw,n,nw,n,nw,nw,n,n,n,se,nw,s,nw,ne,n,n,n,nw,nw,nw,nw,ne,n,n,nw,nw,ne,n,nw,nw,nw,se,nw,nw,se,nw,n,n,nw,nw,s,n,se,nw,n,n,n,nw,n,nw,nw,nw,nw,n,nw,se,se,ne,nw,n,nw,n,se,sw,n,nw,nw,nw,nw,nw,nw,nw,n,nw,se,nw,nw,nw,se,n,nw,n,nw,s,nw,nw,se,nw,ne,nw,nw,n,nw,n,nw,n,nw,se,nw,sw,nw,nw,n,nw,n,n,ne,nw,sw,nw,ne,n,nw,nw,se,nw,nw,ne,s,n,n,nw,se,nw,nw,se,n,sw,nw,nw,ne,n,nw,s,n,se,n,nw,nw,nw,nw,n,nw,s,ne,nw,nw,sw,nw,nw,s,nw,s,nw,n,n,nw,n,s,nw,nw,nw,nw,nw,n,nw,n,nw,n,nw,nw,sw,nw,nw,se,n,n,nw,nw,nw,nw,nw,s,n,n,n,n,s,n,n,nw,nw,sw,n,nw,s,sw,nw,n,nw,se,s,nw,nw,nw,s,nw,nw,se,nw,nw,n,nw,nw,n,s,nw,n,nw,nw,nw,nw,nw,sw,nw,sw,n,n,nw,n,nw,nw,nw,nw,n,n,nw,nw,n,s,nw,n,nw,nw,nw,nw,sw,nw,nw,n,ne,se,nw,n,nw,n,n,n,s,nw,n,nw,nw,nw,nw,ne,nw,n,nw,n,nw,nw,nw,n,nw,nw,nw,nw,se,nw,n,n,nw,nw,se,nw,nw,nw,n,nw,nw,s,n,nw,s,ne,nw,ne,n,n,s,n,n,nw,nw,nw,nw,nw,nw,n,n,nw,nw,nw,s,nw,ne,nw,n,n,nw,nw,nw,nw,n,se,nw,nw,n,n,n,nw,nw,nw,nw,nw,nw,n,sw,nw,nw,nw,nw,nw,n,n,nw,nw,n,n,se,nw,nw,nw,nw,nw,s,n,sw,se,nw,ne,nw,n,nw,se,s,nw,n,nw,se,sw,nw,n,se,n,nw,nw,sw,nw,se,nw,nw,nw,nw,n,nw,sw,nw,nw,nw,nw,n,n,nw,nw,nw,se,se,sw,nw,n,n,n,sw,se,ne,nw,nw,sw,nw,n,se,nw,nw,nw,nw,se,sw,nw,nw,nw,nw,n,nw,n,n,n,s,nw,nw,nw,nw,n,ne,nw,n,sw,se,sw,n,nw,nw,nw,nw,sw,nw,nw,nw,ne,nw,nw,n,sw,nw,nw,nw,sw,n,nw,nw,n,nw,n,nw,n,nw,nw,sw,sw,nw,nw,sw,se,s,nw,nw,nw,s,se,nw,nw,nw,n,nw,nw,nw,nw,sw,nw,sw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,n,nw,nw,nw,nw,nw,nw,nw,nw,n,nw,sw,nw,sw,n,n,n,s,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,s,nw,nw,nw,nw,nw,nw,n,nw,ne,nw,n,nw,n,nw,nw,nw,nw,nw,nw,nw,nw,nw,n,nw,nw,nw,sw,nw,se,nw,nw,nw,nw,nw,nw,s,nw,se,nw,nw,nw,sw,nw,ne,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,sw,nw,nw,nw,nw,nw,nw,n,nw,nw,nw,nw,nw,sw,nw,s,nw,nw,nw,nw,nw,ne,nw,nw,n,nw,nw,nw,n,nw,nw,nw,nw,n,se,nw,nw,s,nw,sw,nw,nw,n,n,nw,nw,n,nw,nw,sw,nw,nw,nw,se,nw,nw,nw,se,nw,nw,sw,nw,nw,nw,sw,nw,nw,nw,n,n,nw,nw,nw,nw,nw,n,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,sw,se,nw,nw,nw,s,s,nw,nw,nw,nw,ne,nw,nw,nw,n,nw,ne,nw,se,se,se,s,s,s,s,s,s,s,sw,s,s,s,sw,n,sw,sw,sw,se,sw,n,sw,sw,sw,nw,sw,sw,nw,nw,n,nw,nw,sw,nw,nw,nw,nw,nw,n,nw,nw,n,nw,n,n,nw,sw,s,nw,ne,n,n,n,n,n,n,n,ne,n,n,n,n,n,n,ne,n,n,ne,ne,ne,n,ne,n,n,ne,ne,n,n,ne,n,s,ne,n,ne,ne,n,ne,ne,ne,ne,s,sw,ne,ne,n,sw,ne,ne,se,se,ne,ne,se,nw,ne,ne,ne,ne,ne,se,ne,se,ne,s,n,se,ne,se,sw,ne,ne,ne,s,se,se,se,se,se,se,se,se,se,se,se,se,sw,se,ne,ne,ne,se,se,se,sw,ne,se,se,se,se,se,sw,ne,se,se,n,se,se,n,se,n,sw,se,s,s,se,sw,se,nw,s,s,se,se,se,s,ne,s,s,se,se,se,s,s,s,n,se,se,s,nw,se,n,se,se,s,s,se,s,s,s,s,s,se,s,se,se,s,s,s,s,s,s,s,se,s,s,s,sw,s,s,sw,s,sw,s,n,se,s,nw,s,s,s,s,s,s,s,nw,nw,s,s,s,nw,s,s,sw,s,s,ne,sw,s,s,s,sw,s,s,s,s,s,s,s,s,sw,n,sw,s,sw,s,n,s,se,s,s,s,s,sw,n,se,s,s,sw,sw,s,nw,sw,sw,sw,s,sw,sw,s,sw,ne,sw,s,sw,sw,nw,sw,s,sw,sw,sw,s,nw,sw,sw,s,sw,s,s,sw,sw,s,sw,sw,se,n,s,sw,sw,sw,nw,sw,sw,sw,sw,sw,se,ne,sw,se,n,sw,sw,sw,se,nw,sw,se,sw,sw,sw,sw,nw,sw,nw,nw,sw,nw,sw,sw,nw,nw,n,sw,nw,sw,s,sw,sw,sw,sw,s,sw,sw,sw,sw,nw,sw,sw,nw,ne,sw,sw,sw,n,sw,sw,se,s,sw,sw,sw,sw,nw,sw,sw,nw,sw,sw,nw,nw,nw,se,sw,nw,nw,n,sw,nw,s,nw,nw,sw,sw,sw,sw,sw,s,nw,n,se,sw,sw,sw,nw,sw,nw,nw,nw,nw,sw,ne,sw,nw,nw,nw,n,sw,sw,nw,sw,nw,nw,nw,se,nw,ne,nw,nw,nw,nw,nw,nw,nw,nw,n,s,nw,nw,nw,nw,nw,nw,nw,nw,sw,nw,nw,sw,nw,nw,nw,nw,nw,se,s,nw,ne,nw,nw,nw,n,nw,n,nw,nw,nw,n,nw,nw,n,se,nw,se,nw,nw,s,nw,nw,nw,nw,s,nw,nw,n,s,sw,s,nw,nw,nw,nw,n,nw,ne,sw,nw,nw,n,nw,nw,n,n,nw,nw,s,nw,nw,nw,s,n,nw,nw,nw,se,sw,nw,nw,n,nw,nw,nw,n,ne,nw,n,nw,nw,s,n,nw,n,nw,nw,n,nw,n,ne,se,nw,n,nw,nw,n,ne,nw,s,n,nw,n,s,n,nw,s,nw,nw,nw,nw,nw,nw,n,n,nw,nw,nw,ne,n,se,nw,n,n,n,n,nw,se,nw,n,nw,nw,n,nw,sw,n,n,n,ne,s,sw,n,nw,n,s,nw,sw,nw,nw,n,nw,n,nw,n,nw,se,n,nw,n,n,n,n,n,nw,n,s,sw,n,n,n,s,n,ne,n,nw,n,se,n,n,n,s,n,nw,n,n,n,n,n,n,se,nw,nw,n,nw,n,n,n,n,n,ne,n,n,n,nw,nw,nw,n,s,n,n,n,nw,n,nw,n,n,n,n,nw,nw,n,n,se,nw,n,n,n,se,n,s,nw,n,n,n,n,sw,ne,nw,n,n,n,n,n,n,n,n,n,n,s,n,n,se,n,sw,se,n,n,se,n,n,n,se,n,n,n,n,n,se,n,nw,n,ne,sw,s,n,n,n,n,n,n,n,n,n,n,s,n,n,ne,n,n,n,se,ne,ne,n,n,nw,n,n,n,ne,ne,n,n,n,n,ne,n,se,n,ne,n,n,s,n,n,ne,ne,n,ne,n,ne,s,n,ne,ne,n,n,sw,n,s,ne,sw,n,ne,ne,se,ne,ne,n,n,ne,s,n,n,n,s,n,n,nw,n,n,ne,n,n,n,n,ne,se,ne,ne,n,n,n,ne,ne,ne,n,n,n,nw,ne,n,n,n,n,n,ne,n,ne,n,ne,n,ne,ne,n,n,ne,n,ne,n,ne,sw,nw,ne,ne,n,ne,n,n,n,n,ne,ne,ne,ne,ne,n,n,ne,sw,ne,sw,ne,ne,ne,ne,ne,sw,n,n,ne,ne,sw,sw,ne,ne,ne,ne,n,sw,ne,ne,ne,ne,se,ne,n,n,ne,n,sw,sw,n,ne,ne,ne,ne,ne,ne,ne,s,ne,s,ne,ne,ne,ne,s,n,n,ne,ne,ne,ne,ne,ne,se,s,n,ne,nw,s,n,s,ne,s,n,nw,ne,ne,ne,se,ne,ne,se,ne,ne,n,ne,n,s,ne,ne,ne,ne,ne,ne,ne,s,ne,ne,s,ne,ne,ne,ne,sw,sw,se,sw,ne,ne,sw,se,ne,s,ne,ne,ne,ne,ne,ne,ne,s,s,s,se,ne,ne,n,ne,ne,ne,n,sw,ne,nw,nw,ne,ne,ne,sw,sw,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,n,s,n,ne,n,s,n,ne,ne,ne,ne,se,se,ne,se,ne,ne,ne,ne,sw,ne,ne,n,ne,se,ne,ne,ne,nw,ne,ne,ne,ne,ne,nw,ne,se,se,ne,ne,ne,ne,ne,se,ne,ne,se,ne,ne,ne,s,ne,se,ne,n,ne,ne,nw,nw,se,ne,s,se,ne,ne,ne,ne,sw,nw,se,ne,ne,ne,ne,se,se,ne,ne,ne,ne,ne,ne,ne,se,ne,n,se,ne,s,ne,se,se,ne,se,ne,ne,ne,se,nw,s,se,ne,ne,nw,se,ne,ne,ne,nw,ne,se,sw,ne,se,ne,n,se,se,nw,se,ne,ne,se,ne,se,se,se,sw,se,se,ne,n,ne,s,ne,ne,ne,ne,nw,n,sw,ne,s,ne,ne,se,s,ne,se,se,ne,ne,se,ne,n,ne,se,se,se,se,se,ne,ne,ne,se,ne,se,ne,se,se,se,se,se,ne,ne,s,ne,ne,se,se,se,se,nw,se,ne,ne,ne,ne,se,s,ne,ne,ne,nw,nw,se,ne,s,ne,se,se,ne,se,se,se,ne,se,se,se,se,ne,ne,se,ne,n,sw,se,se,ne,se,se,sw,se,s,nw,se,ne,ne,s,n,se,s,se,ne,se,se,n,se,se,ne,se,se,se,se,ne,nw,ne,nw,se,se,se,n,ne,se,se,se,se,se,n,se,se,sw,se,ne,se,nw,se,s,sw,se,se,s,ne,ne,se,se,se,se,ne,se,ne,se,ne,ne,se,se,se,se,sw,se,ne,ne,ne,se,se,ne,se,se,se,se,se,se,nw,nw,se,se,n,n,nw,ne,se,se,se,se,sw,se,se,se,se,se,se,n,se,se,se,se,se,se,se,se,s,se,se,se,ne,se,se,se,sw,se,se,se,se,se,s,se,se,se,s,s,se,se,se,se,nw,sw,n,se,se,se,se,se,se,se,se,se,se,ne,se,se,nw,se,se,s,se,se,se,se,sw,se,n,se,se,se,s,se,se,se,se,se,s,n,ne,sw,se,s,se,s,se,se,se,n,se,sw,n,se,se,se,n,se,nw,se,se,s,se,se,se,se,se,se,se,n,se,se,nw,se,se,se,se,sw,se,se,sw,se,nw,se,se,sw,se,se,se,sw,nw,se,se,sw,se,se,se,se,se,se,nw,se,se,ne,se,se,s,se,s,se,se,se,se,se,sw,s,se,se,se,s,se,se,s,se,s,se,ne,se,nw,s,s,se,s,se,se,se,s,se,se,s,se,s,se,se,se,ne,se,se,se,se,se,se,nw,s,se,ne,s,se,n,se,se,ne,se,se,se,se,nw,s,nw,n,ne,s,se,se,n,s,s,se,se,s,sw,se,se,sw,n,se,se,se,se,se,se,s,s,s,se,s,n,se,se,se,se,sw,n,se,se,s,nw,se,s,se,s,se,se,se,se,se,se,se,s,ne,s,se,ne,ne,n,se,n,s,se,se,se,ne,se,n,se,s,se,se,s,sw,s,se,s,s,s,n,s,se,se,n,ne,s,se,n,se,s,se,s,n,sw,nw,n,se,ne,se,se,s,s,s,se,s,ne,se,s,se,s,se,se,se,s,s,n,ne,se,s,s,s,se,ne,s,se,se,se,se,n,n,se,n,sw,se,se,nw,s,s,se,s,se,ne,s,ne,se,se,se,s,s,se,s,s,se,s,s,s,s,s,s,se,ne,s,se,s,s,se,ne,s,se,se,s,se,se,s,s,s,se,s,nw,sw,se,s,se,se,nw,s,s,s,se,s,s,s,s,s,s,nw,s,se,se,s,s,n,s,se,s,se,se,se,s,s,s,se,n,se,se,se,se,se,s,ne,n,ne,s,se,se,s,se,s,se,s,n,s,s,n,s,s,nw,n,s,s,ne,ne,ne,s,s,s,se,s,sw,s,s,se,s,se,se,se,s,ne,s,se,se,se,s,s,s,se,s,se,s,sw,s,s,s,n,s,s,s,se,s,s,s,s,s,se,n,s,se,se,s,s,ne,s,se,s,s,s,n,se,nw,nw,s,s,n,s,ne,s,s,nw,s,s,s,s,s,s,s,se,s,s,s,n,s,s,s,s,se,se,s,s,nw,s,ne,s,sw,s,s,s,n,s,nw,sw,s,s,s,s,s,sw,s,s,s,ne,s,se,s,s,s,s,s,s,s,s,s,nw,s,s,s,se,s,s,s,s,s,se,nw,se,nw,s,n,s,s,s,s,s,s,sw,s,s,s,s,s,s,s,n,s,s,ne,n,s,s,s,s,nw,ne,s,s,s,s,s,s,nw,s,s,ne,s,ne,s,s,s,se,s,s,s,s,s,s,s,s,s,s,s,n,nw,s,s,s,s,s,sw,s,nw,s,s,sw,s,s,s,s,s,se,s,s,s,s,s,s,s,sw,s,s,s,se,s,s,nw,s,s,s,s,s,n,s,nw,s,s,sw,n,s,n,s,s,s,s,sw,nw,se,s,s,s,nw,s,s,sw,s,s,s,s,n,s,sw,s,se,s,sw,s,s,s,sw,s,sw,s,s,s,s,s,s,sw,ne,s,s,s,n,sw,s,s,s,s,s,s,s,s,s,s,s,s,s,sw,s,s,s,s,s,sw,s,s,sw,s,s,ne,se,se,sw,n,s,s,s,s,s,s,s,s,nw,s,ne,sw,se,s,s,sw,s,s,nw,se,s,s,sw,ne,s,sw,s,se,s,s,nw,se,sw,s,s,s,s,sw,sw,se,sw,sw,sw,s,s,s,sw,s,ne,se,se,ne,s,s,s,sw,s,s,sw,sw,s,sw,s,sw,s,s,s,s,sw,s,nw,s,sw,s,s,s,s,s,sw,n,s,se,s,sw,sw,sw,s,sw,s,sw,s,s,nw,n,sw,s,s,s,s,sw,s,s,s,s,sw,s,s,sw,n,s,s,nw,sw,ne,nw,s,s,s,sw,s,n,nw,sw,s,s,s,sw,s,sw,s,s,s,sw,s,s,s,sw,s,ne,sw,ne,s,sw,s,s,s,s,s,ne,sw,se,s,sw,sw,sw,s,sw,s,s,s,s,se,s,sw,s,sw,sw,s,sw,sw,sw,s,s,s,s,n,se,s,sw,s,s,sw,s,s,s,sw,sw,sw,s,sw,s,sw,ne,ne,s,s,s,sw,s,nw,nw,se,sw,s,s,s,s,sw,s,s,sw,se,s,ne,s,n,s,sw,sw,s,sw,ne,se,sw,s,s,s,ne,sw,s,sw,s,sw,s,n,sw,sw,s,sw,sw,s,nw,ne,s,nw,sw,s,sw,sw,s,s,n,sw,sw,s,s,s,n,sw,s,s,nw,s,sw,sw,s,sw,sw,se,s,s,s,s,s,s,s,sw,nw,sw,sw,se,s,s,se,sw,s,sw,se,nw,nw,s,nw,s,sw,ne,s,s,ne,s,sw,s,s,s,sw,sw,s,se,sw,n,sw,s,sw,sw,s,sw,sw,sw,s,s,s,se,sw,s,s,s,s,s,sw,sw,s,sw,se,sw,s,sw,sw,se,sw,sw,s,sw,sw,s,s,s,s,s,s,s,s,sw,sw,sw,sw,ne,ne,ne,ne,s,nw,se,s,s,sw,s,s,s,s,s,sw,sw,sw,s,s,sw,s,s,s,sw,sw,s,s,sw,sw,sw,sw,s,sw,sw,sw,s,sw,sw,sw,ne,sw,ne,s,sw,sw,ne,sw,se,sw,sw,sw,s,s,s,sw,se,s,sw,sw,sw,nw,s,sw,se,sw,sw,s,se,sw,se,sw,s,sw,sw,sw,s,sw,sw,sw,s,se,sw,s,s,sw,sw,s,s,sw,sw,sw,s,sw,sw,s,s,s,sw,sw,sw,s,sw,s,ne,se,sw,s,sw,sw,s,sw,sw,sw,se,sw,sw,sw,sw,n,s,sw,sw,se,ne,sw,sw,sw,s,sw,sw,sw,sw,sw,n,sw,s,s,se,sw,sw,sw,sw,s,sw,sw,s,sw,s,s,s,sw,sw,nw,s,sw,sw,sw,sw,sw,n,sw,sw,sw,sw,s,sw,sw,sw,sw,ne,sw,sw,sw,s,ne,sw,sw,s,sw,sw,sw,ne,s,ne,sw,se,s,sw,sw,s,s,sw,sw,se,sw,nw,sw,sw,sw,sw,sw,n,sw,se,nw,sw,sw,sw,sw,sw,n,sw,sw,sw,s,sw,sw,sw,sw,sw,se,sw,sw,sw,sw,se,sw,sw,sw,sw,sw,s,se,sw,sw,sw,ne,sw,sw,se,sw,s,sw,sw,ne,sw,sw,sw,sw,ne,ne,sw,ne,sw,sw,sw,s,sw,sw,sw,s,s,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,se,sw,sw,nw,sw,sw,sw,nw,nw,sw,sw,sw,sw,n,sw,ne,se,s,sw,sw,sw,se,ne,sw,ne,se,sw,sw,sw,sw,sw,nw,sw,sw,sw"
  end
end

defmodule Santa.Day12.Input do
  def input() do
"0 <-> 1352, 1864
1 <-> 430
2 <-> 1202, 1416
3 <-> 303, 363, 635
4 <-> 1041, 1383
5 <-> 143, 1513
6 <-> 6
7 <-> 619
8 <-> 167, 1246, 1822
9 <-> 577, 1274, 1347
10 <-> 10, 899
11 <-> 790
12 <-> 606
13 <-> 1134, 1894
14 <-> 539, 1974
15 <-> 683, 1783
16 <-> 438, 1235
17 <-> 17
18 <-> 760, 1162
19 <-> 1146
20 <-> 678, 1235, 1397, 1911
21 <-> 127, 615, 980
22 <-> 22, 187
23 <-> 192, 552
24 <-> 169
25 <-> 471
26 <-> 1030, 1329, 1333
27 <-> 1404
28 <-> 1249
29 <-> 1755
30 <-> 198
31 <-> 510, 1884
32 <-> 1088
33 <-> 54, 372, 616, 938, 1548
34 <-> 1574
35 <-> 238, 1411
36 <-> 114
37 <-> 1962
38 <-> 1554, 1633
39 <-> 72, 486, 1002
40 <-> 40, 1224, 1342, 1511, 1886
41 <-> 1244, 1644
42 <-> 1784
43 <-> 169, 1142
44 <-> 96, 827
45 <-> 842, 1512
46 <-> 1248
47 <-> 283, 1125, 1130
48 <-> 212, 327, 1922
49 <-> 490
50 <-> 359, 1386
51 <-> 1186
52 <-> 414
53 <-> 808
54 <-> 33, 673
55 <-> 614
56 <-> 56
57 <-> 257
58 <-> 482, 701, 1702, 1921
59 <-> 515, 695, 1073
60 <-> 809
61 <-> 865
62 <-> 106, 764
63 <-> 416, 1119, 1360
64 <-> 552
65 <-> 978, 1042
66 <-> 97, 130, 168
67 <-> 180, 339
68 <-> 1076
69 <-> 1550, 1621
70 <-> 1163, 1574, 1834
71 <-> 163, 1789
72 <-> 39, 1461, 1693
73 <-> 266, 652, 1612, 1877
74 <-> 860, 936, 1685
75 <-> 502, 768
76 <-> 1064
77 <-> 1685, 1967
78 <-> 501
79 <-> 743, 1261, 1608
80 <-> 80, 1901
81 <-> 1684
82 <-> 82
83 <-> 234, 504, 1159
84 <-> 1529
85 <-> 705
86 <-> 362, 644
87 <-> 223
88 <-> 1870
89 <-> 1412, 1633
90 <-> 191, 1141, 1993, 1999
91 <-> 588
92 <-> 92
93 <-> 918
94 <-> 972, 1401
95 <-> 1043, 1299
96 <-> 44
97 <-> 66, 1089
98 <-> 1300
99 <-> 243, 1220, 1957
100 <-> 1327
101 <-> 815, 1700, 1782
102 <-> 427, 723, 780, 1033
103 <-> 589, 1586
104 <-> 486
105 <-> 1296
106 <-> 62, 1677
107 <-> 1855
108 <-> 174, 1792
109 <-> 1518
110 <-> 1943
111 <-> 1799
112 <-> 112, 845
113 <-> 1189
114 <-> 36, 1895
115 <-> 775, 894, 987, 1832
116 <-> 455, 1317
117 <-> 436, 630, 944, 1229
118 <-> 1316
119 <-> 282
120 <-> 1022
121 <-> 446, 685
122 <-> 908, 1189
123 <-> 850, 1058, 1400
124 <-> 124, 867
125 <-> 316, 1198
126 <-> 1938
127 <-> 21
128 <-> 514, 871
129 <-> 597
130 <-> 66, 187, 541, 952
131 <-> 637, 728, 1951
132 <-> 1480
133 <-> 921, 1443
134 <-> 506, 576
135 <-> 1510
136 <-> 755
137 <-> 842
138 <-> 1077, 1219
139 <-> 1284, 1664
140 <-> 403
141 <-> 452, 1400
142 <-> 142, 442, 1462, 1463
143 <-> 5, 331, 492
144 <-> 144
145 <-> 844
146 <-> 774, 1141
147 <-> 351, 458
148 <-> 1984
149 <-> 499, 909, 935
150 <-> 153, 1300
151 <-> 151
152 <-> 1331
153 <-> 150
154 <-> 783
155 <-> 155, 1985
156 <-> 1752
157 <-> 716, 1144
158 <-> 647, 750
159 <-> 583
160 <-> 493, 770, 1669, 1675
161 <-> 161, 291, 1890
162 <-> 744, 1088, 1250
163 <-> 71, 803
164 <-> 231
165 <-> 1799
166 <-> 1101
167 <-> 8, 167
168 <-> 66, 441
169 <-> 24, 43, 1015
170 <-> 414, 509, 1222, 1392, 1947
171 <-> 180, 260, 1448
172 <-> 1894
173 <-> 585, 1662
174 <-> 108, 1300, 1304, 1789
175 <-> 574, 1245
176 <-> 1776, 1809
177 <-> 1625
178 <-> 1463
179 <-> 1469, 1648
180 <-> 67, 171
181 <-> 506, 1333
182 <-> 1703
183 <-> 370, 745, 816, 961
184 <-> 1143, 1608, 1696
185 <-> 431, 447, 633
186 <-> 352, 752
187 <-> 22, 130, 201, 232, 279
188 <-> 921
189 <-> 1460
190 <-> 445, 891, 972
191 <-> 90, 1078, 1383, 1957
192 <-> 23, 1057
193 <-> 1438
194 <-> 196, 819, 873
195 <-> 1403
196 <-> 194, 508
197 <-> 865, 1625
198 <-> 30, 855, 1397
199 <-> 399, 401
200 <-> 458
201 <-> 187, 457, 796
202 <-> 754, 964, 1736, 1737
203 <-> 937, 1890
204 <-> 676, 1148, 1472
205 <-> 205
206 <-> 206, 872, 1504
207 <-> 1575, 1717
208 <-> 1349, 1582, 1619, 1952
209 <-> 375, 523
210 <-> 1029
211 <-> 588, 623
212 <-> 48, 404, 820
213 <-> 213
214 <-> 1370
215 <-> 360, 412, 1507, 1800, 1874
216 <-> 287, 634, 1023, 1986
217 <-> 662, 1863
218 <-> 417, 1154
219 <-> 1250, 1442, 1962
220 <-> 595, 935, 1153, 1181
221 <-> 290
222 <-> 1328, 1407
223 <-> 87, 464, 925, 1131
224 <-> 1398
225 <-> 225, 768
226 <-> 999
227 <-> 311, 581, 697
228 <-> 983, 1019
229 <-> 719, 1469, 1820
230 <-> 453, 806, 1205, 1449
231 <-> 164, 451, 1741
232 <-> 187
233 <-> 664, 1419
234 <-> 83, 295, 482, 1684
235 <-> 622
236 <-> 236, 1662
237 <-> 287
238 <-> 35, 1308, 1608, 1805
239 <-> 1365
240 <-> 1177
241 <-> 387, 778, 1051
242 <-> 1233
243 <-> 99, 806, 1614
244 <-> 609, 1468
245 <-> 625, 1618
246 <-> 841, 1835
247 <-> 1720
248 <-> 1382, 1859
249 <-> 601, 1882
250 <-> 250, 1189, 1603, 1673, 1793
251 <-> 511
252 <-> 627
253 <-> 1195
254 <-> 400, 1562, 1893, 1951
255 <-> 285, 588, 1180
256 <-> 1408, 1468
257 <-> 57, 1211, 1940
258 <-> 695, 1174
259 <-> 610, 1176, 1516
260 <-> 171
261 <-> 268
262 <-> 502, 837, 1963
263 <-> 263
264 <-> 622, 638, 1815, 1945
265 <-> 570, 1590
266 <-> 73
267 <-> 336, 1040
268 <-> 261, 1815
269 <-> 652, 966, 1487
270 <-> 508, 522
271 <-> 1259, 1298, 1477, 1767
272 <-> 553, 1103, 1982
273 <-> 593, 1919
274 <-> 934, 1361
275 <-> 1839
276 <-> 329
277 <-> 1017, 1047
278 <-> 671
279 <-> 187, 853
280 <-> 1097, 1370
281 <-> 629
282 <-> 119, 1431
283 <-> 47, 974
284 <-> 1359, 1471
285 <-> 255
286 <-> 321, 883
287 <-> 216, 237
288 <-> 630, 880
289 <-> 289, 1853
290 <-> 221, 1307
291 <-> 161, 691, 1213
292 <-> 808
293 <-> 877, 1031, 1192
294 <-> 294, 1935
295 <-> 234
296 <-> 520, 1725
297 <-> 578, 702, 1184, 1209, 1431
298 <-> 979
299 <-> 1297, 1506
300 <-> 540, 995, 1187, 1774
301 <-> 675
302 <-> 739, 842
303 <-> 3, 1149, 1369, 1484
304 <-> 690
305 <-> 1306
306 <-> 481
307 <-> 474, 1723
308 <-> 323
309 <-> 995
310 <-> 544, 797, 1000
311 <-> 227, 927, 1127
312 <-> 1519
313 <-> 1989
314 <-> 314
315 <-> 536, 560, 1758
316 <-> 125, 582, 679, 1362
317 <-> 317, 510
318 <-> 1382, 1678
319 <-> 682
320 <-> 1733
321 <-> 286, 575
322 <-> 905, 1898
323 <-> 308, 366, 788, 1022, 1119, 1615
324 <-> 1151, 1390, 1540
325 <-> 1022
326 <-> 335, 1858
327 <-> 48
328 <-> 1076, 1456
329 <-> 276, 329, 1268
330 <-> 796, 919, 1330, 1565, 1926
331 <-> 143, 1522
332 <-> 831
333 <-> 1746
334 <-> 1254, 1844
335 <-> 326
336 <-> 267, 913
337 <-> 454, 1365, 1618
338 <-> 1800
339 <-> 67
340 <-> 1061
341 <-> 957
342 <-> 1130, 1242
343 <-> 782, 1629
344 <-> 350, 1216
345 <-> 898
346 <-> 586, 1372
347 <-> 1537
348 <-> 1899
349 <-> 378
350 <-> 344
351 <-> 147, 381, 993
352 <-> 186
353 <-> 353, 646, 913
354 <-> 465
355 <-> 1423, 1695
356 <-> 573, 1241
357 <-> 1695
358 <-> 1100, 1271
359 <-> 50, 1472, 1910
360 <-> 215
361 <-> 1512
362 <-> 86, 1768, 1997
363 <-> 3, 1731
364 <-> 657, 1713, 1992
365 <-> 475, 1490, 1968
366 <-> 323, 1482, 1765
367 <-> 831
368 <-> 1393
369 <-> 1135
370 <-> 183, 592
371 <-> 637, 1518
372 <-> 33
373 <-> 1494
374 <-> 1761
375 <-> 209
376 <-> 1587
377 <-> 1522
378 <-> 349, 686, 1006, 1946
379 <-> 379, 487
380 <-> 1763
381 <-> 351, 1048
382 <-> 1895
383 <-> 1623
384 <-> 1423, 1450
385 <-> 1350, 1770
386 <-> 798, 890
387 <-> 241, 894
388 <-> 1756
389 <-> 629
390 <-> 507
391 <-> 1780
392 <-> 1144
393 <-> 1240, 1279
394 <-> 1227
395 <-> 1361
396 <-> 396
397 <-> 397, 1228
398 <-> 454
399 <-> 199
400 <-> 254, 1147
401 <-> 199, 1068, 1445, 1918
402 <-> 636, 1601, 1934
403 <-> 140, 921
404 <-> 212, 430, 468, 1064, 1346, 1434
405 <-> 633
406 <-> 949
407 <-> 512, 951
408 <-> 1900, 1908
409 <-> 409
410 <-> 1724
411 <-> 624, 733
412 <-> 215, 1455
413 <-> 1612
414 <-> 52, 170
415 <-> 907, 1600
416 <-> 63
417 <-> 218, 835
418 <-> 418, 1116
419 <-> 1531
420 <-> 1154, 1538
421 <-> 485, 511
422 <-> 422
423 <-> 665
424 <-> 1891
425 <-> 1140
426 <-> 1409
427 <-> 102
428 <-> 463
429 <-> 575, 1432, 1779
430 <-> 1, 404
431 <-> 185, 1473
432 <-> 1065, 1720
433 <-> 1554
434 <-> 965
435 <-> 562, 1577
436 <-> 117
437 <-> 960
438 <-> 16, 926, 1342
439 <-> 1171
440 <-> 1198, 1622
441 <-> 168
442 <-> 142
443 <-> 443
444 <-> 1399, 1836
445 <-> 190
446 <-> 121
447 <-> 185, 1249
448 <-> 1505
449 <-> 542, 1433
450 <-> 1859
451 <-> 231, 590, 1641, 1651
452 <-> 141, 452
453 <-> 230
454 <-> 337, 398, 511
455 <-> 116, 455
456 <-> 1231, 1368
457 <-> 201, 1294
458 <-> 147, 200, 1799
459 <-> 1113
460 <-> 1521, 1841
461 <-> 882
462 <-> 683, 1744, 1887
463 <-> 428, 1221
464 <-> 223, 1310
465 <-> 354, 880
466 <-> 960, 1216
467 <-> 511, 1185
468 <-> 404
469 <-> 469
470 <-> 470
471 <-> 25, 494, 747, 1078
472 <-> 855, 1358
473 <-> 473
474 <-> 307, 1288
475 <-> 365, 688, 1881
476 <-> 1129, 1496, 1753
477 <-> 828, 1032, 1376
478 <-> 1474, 1950
479 <-> 479
480 <-> 480, 1318
481 <-> 306, 487
482 <-> 58, 234, 902, 1003
483 <-> 1681, 1730
484 <-> 484, 1071
485 <-> 421, 800, 975, 1124
486 <-> 39, 104
487 <-> 379, 481, 918
488 <-> 488, 1297
489 <-> 897, 1216, 1384
490 <-> 49, 1286
491 <-> 1283
492 <-> 143, 1098
493 <-> 160, 1120
494 <-> 471, 1781
495 <-> 495, 642, 714, 1004
496 <-> 1227
497 <-> 513, 1349, 1382
498 <-> 1846
499 <-> 149, 499
500 <-> 621, 1005, 1640
501 <-> 78, 1254
502 <-> 75, 262, 1008, 1404, 1915
503 <-> 1707
504 <-> 83
505 <-> 1809
506 <-> 134, 181, 506
507 <-> 390, 621, 1630
508 <-> 196, 270
509 <-> 170, 1819
510 <-> 31, 317, 656
511 <-> 251, 421, 454, 467, 1532
512 <-> 407, 1258, 1430
513 <-> 497
514 <-> 128, 840
515 <-> 59, 619, 662
516 <-> 1649
517 <-> 517
518 <-> 731
519 <-> 1522
520 <-> 296, 847
521 <-> 1667
522 <-> 270, 690, 1047, 1246
523 <-> 209, 1087
524 <-> 1267
525 <-> 714, 1126, 1896
526 <-> 1788
527 <-> 1660, 1733
528 <-> 1309, 1955
529 <-> 1912
530 <-> 838
531 <-> 1167, 1515
532 <-> 532, 1128
533 <-> 1602
534 <-> 564, 1108
535 <-> 535, 810
536 <-> 315, 1842
537 <-> 1947
538 <-> 1208, 1445
539 <-> 14, 825
540 <-> 300
541 <-> 130, 1188
542 <-> 449, 1197
543 <-> 1302
544 <-> 310, 1131
545 <-> 1409
546 <-> 772
547 <-> 1954
548 <-> 1662
549 <-> 1601
550 <-> 875, 1899
551 <-> 1208
552 <-> 23, 64, 1140
553 <-> 272, 1616, 1632, 1988
554 <-> 554, 622, 1169
555 <-> 555, 705, 772
556 <-> 625
557 <-> 867, 1210
558 <-> 870, 1039, 1320, 1509, 1995
559 <-> 835, 968
560 <-> 315
561 <-> 1335
562 <-> 435
563 <-> 658
564 <-> 534, 640, 1456, 1610
565 <-> 1130
566 <-> 741
567 <-> 1018
568 <-> 839
569 <-> 1544, 1767
570 <-> 265
571 <-> 1006, 1852
572 <-> 1320
573 <-> 356, 787, 816, 1947
574 <-> 175, 1287, 1381, 1413
575 <-> 321, 429
576 <-> 134
577 <-> 9, 668, 1979
578 <-> 297, 1576
579 <-> 1779
580 <-> 798
581 <-> 227, 832
582 <-> 316, 1033
583 <-> 159, 1550
584 <-> 1988
585 <-> 173, 1275
586 <-> 346, 1343
587 <-> 1499, 1911
588 <-> 91, 211, 255
589 <-> 103, 703
590 <-> 451, 1686
591 <-> 828, 1746
592 <-> 370
593 <-> 273
594 <-> 1358, 1607
595 <-> 220
596 <-> 1050, 1375
597 <-> 129, 1463
598 <-> 598, 1804
599 <-> 1606, 1671
600 <-> 1930
601 <-> 249, 869, 1233, 1336
602 <-> 1150, 1203
603 <-> 603
604 <-> 604, 620
605 <-> 835, 1556, 1770
606 <-> 12, 696, 1634, 1897
607 <-> 1629
608 <-> 608
609 <-> 244, 738
610 <-> 259, 684, 1309
611 <-> 1398
612 <-> 612, 1178, 1808
613 <-> 1245
614 <-> 55, 1701, 1969
615 <-> 21, 759
616 <-> 33, 1160
617 <-> 617, 824
618 <-> 725, 840
619 <-> 7, 515
620 <-> 604, 1247
621 <-> 500, 507, 1039
622 <-> 235, 264, 554, 665, 1476
623 <-> 211, 773, 1382, 1839
624 <-> 411, 943
625 <-> 245, 556, 794, 1277
626 <-> 988, 1429, 1495
627 <-> 252, 708, 1780
628 <-> 981
629 <-> 281, 389, 1132
630 <-> 117, 288, 802, 1438, 1492
631 <-> 753, 1279, 1716, 1843
632 <-> 1715
633 <-> 185, 405
634 <-> 216
635 <-> 3
636 <-> 402, 1983
637 <-> 131, 371
638 <-> 264, 1157
639 <-> 815
640 <-> 564
641 <-> 1473, 1727
642 <-> 495
643 <-> 967, 1996
644 <-> 86, 1586
645 <-> 1063, 1994
646 <-> 353
647 <-> 158, 1732
648 <-> 910, 1758
649 <-> 685, 704
650 <-> 1443, 1490
651 <-> 891
652 <-> 73, 269, 1374
653 <-> 756
654 <-> 1026
655 <-> 1177
656 <-> 510
657 <-> 364
658 <-> 563, 1264, 1402, 1457, 1617
659 <-> 899, 1543
660 <-> 874, 1691
661 <-> 1979
662 <-> 217, 515
663 <-> 953, 1147
664 <-> 233, 820, 1152
665 <-> 423, 622, 856
666 <-> 1020
667 <-> 1093, 1611
668 <-> 577, 892
669 <-> 1563
670 <-> 984, 1927
671 <-> 278, 803, 982
672 <-> 1143
673 <-> 54, 1650, 1902
674 <-> 1624, 1694
675 <-> 301, 1473
676 <-> 204, 1726
677 <-> 786, 1772
678 <-> 20
679 <-> 316
680 <-> 722, 1885
681 <-> 845
682 <-> 319, 777, 1935
683 <-> 15, 462
684 <-> 610
685 <-> 121, 649, 836, 1137
686 <-> 378, 1709
687 <-> 687
688 <-> 475
689 <-> 1950
690 <-> 304, 522, 1357, 1814
691 <-> 291
692 <-> 838, 1183
693 <-> 1298
694 <-> 1493
695 <-> 59, 258, 1529
696 <-> 606
697 <-> 227, 1590
698 <-> 870
699 <-> 1699
700 <-> 1740
701 <-> 58, 1535
702 <-> 297
703 <-> 589, 703
704 <-> 649, 1289, 1652
705 <-> 85, 555
706 <-> 1523
707 <-> 1606
708 <-> 627, 710, 1498, 1598
709 <-> 852, 1835
710 <-> 708
711 <-> 711
712 <-> 1786
713 <-> 991, 1558
714 <-> 495, 525
715 <-> 1185, 1473
716 <-> 157
717 <-> 1425
718 <-> 718, 1316
719 <-> 229, 1568
720 <-> 1977
721 <-> 721, 1272
722 <-> 680, 734, 1547
723 <-> 102
724 <-> 1982
725 <-> 618, 807
726 <-> 1771
727 <-> 1447
728 <-> 131, 1232, 1972
729 <-> 1345, 1875
730 <-> 730, 991
731 <-> 518, 1201, 1484
732 <-> 1972
733 <-> 411, 1596
734 <-> 722
735 <-> 1253
736 <-> 936, 1160
737 <-> 799, 865, 957, 1319
738 <-> 609
739 <-> 302, 1047
740 <-> 1532
741 <-> 566, 1226, 1395
742 <-> 742, 1215
743 <-> 79, 1552
744 <-> 162, 1110
745 <-> 183
746 <-> 1861
747 <-> 471
748 <-> 1671
749 <-> 1099, 1899
750 <-> 158
751 <-> 751
752 <-> 186, 804, 1013, 1403
753 <-> 631, 753
754 <-> 202, 1423, 1670
755 <-> 136, 803, 1343
756 <-> 653, 756
757 <-> 819
758 <-> 758
759 <-> 615
760 <-> 18, 1044
761 <-> 1636
762 <-> 1184, 1251
763 <-> 879, 1227
764 <-> 62, 1486
765 <-> 1993
766 <-> 966, 1665
767 <-> 773, 925, 1295
768 <-> 75, 225
769 <-> 1675
770 <-> 160
771 <-> 801, 916, 985, 996, 1984
772 <-> 546, 555
773 <-> 623, 767, 927, 1447
774 <-> 146, 979, 1583
775 <-> 115, 947
776 <-> 1055
777 <-> 682, 1114, 1480
778 <-> 241
779 <-> 1735
780 <-> 102, 781, 1942
781 <-> 780, 1062
782 <-> 343, 809, 881, 1766
783 <-> 154, 1389, 1763
784 <-> 1168, 1935
785 <-> 1286
786 <-> 677, 833, 1140
787 <-> 573, 1801
788 <-> 323, 1036
789 <-> 983, 1599
790 <-> 11, 1322, 1503, 1667
791 <-> 956
792 <-> 1708
793 <-> 1431
794 <-> 625, 1842
795 <-> 795
796 <-> 201, 330
797 <-> 310, 1425, 1749
798 <-> 386, 580
799 <-> 737, 1561, 1722
800 <-> 485
801 <-> 771
802 <-> 630, 1363
803 <-> 163, 671, 755, 1897, 1953
804 <-> 752, 1356
805 <-> 1459
806 <-> 230, 243
807 <-> 725, 855
808 <-> 53, 292, 1303, 1494
809 <-> 60, 782
810 <-> 535, 1713
811 <-> 1670
812 <-> 1910
813 <-> 1458
814 <-> 896, 1098
815 <-> 101, 639, 815, 898
816 <-> 183, 573
817 <-> 817, 1021, 1262
818 <-> 818, 1077
819 <-> 194, 757, 861
820 <-> 212, 664, 1944
821 <-> 1081, 1109
822 <-> 1769
823 <-> 1758
824 <-> 617
825 <-> 539
826 <-> 1150, 1844
827 <-> 44, 831
828 <-> 477, 591
829 <-> 1947
830 <-> 1165, 1669
831 <-> 332, 367, 827
832 <-> 581
833 <-> 786
834 <-> 1022, 1530
835 <-> 417, 559, 605
836 <-> 685, 1647
837 <-> 262, 1560, 1944
838 <-> 530, 692, 1717
839 <-> 568, 843, 1021, 1788
840 <-> 514, 618, 1390
841 <-> 246, 1190, 1646
842 <-> 45, 137, 302, 1306
843 <-> 839
844 <-> 145, 1715, 1833
845 <-> 112, 681
846 <-> 1136, 1718
847 <-> 520, 1818
848 <-> 1916
849 <-> 1641
850 <-> 123
851 <-> 1072
852 <-> 709
853 <-> 279, 941
854 <-> 1188
855 <-> 198, 472, 807, 1469
856 <-> 665
857 <-> 1318
858 <-> 1608
859 <-> 1901
860 <-> 74
861 <-> 819, 920
862 <-> 1378
863 <-> 1683
864 <-> 1880
865 <-> 61, 197, 737, 1289
866 <-> 866, 1221
867 <-> 124, 557
868 <-> 1485
869 <-> 601, 1029
870 <-> 558, 698
871 <-> 128, 1738
872 <-> 206
873 <-> 194
874 <-> 660, 1100
875 <-> 550, 900
876 <-> 1029
877 <-> 293, 1087, 1486
878 <-> 1726
879 <-> 763, 1252, 1597
880 <-> 288, 465
881 <-> 782
882 <-> 461, 1223, 1252, 1585
883 <-> 286, 954, 1046, 1491
884 <-> 1184
885 <-> 1393, 1487
886 <-> 907, 995, 1123
887 <-> 988, 997
888 <-> 1218, 1679
889 <-> 1544
890 <-> 386, 1867
891 <-> 190, 651, 917, 1593
892 <-> 668
893 <-> 893
894 <-> 115, 387
895 <-> 1378, 1857
896 <-> 814
897 <-> 489, 1829
898 <-> 345, 815
899 <-> 10, 659
900 <-> 875
901 <-> 901, 1132, 1550
902 <-> 482
903 <-> 1172, 1271, 1657
904 <-> 1954
905 <-> 322
906 <-> 1716
907 <-> 415, 886, 1929
908 <-> 122
909 <-> 149
910 <-> 648
911 <-> 911
912 <-> 1292, 1962
913 <-> 336, 353
914 <-> 914
915 <-> 1744
916 <-> 771, 1421
917 <-> 891, 1084, 1428
918 <-> 93, 487
919 <-> 330, 1672
920 <-> 861
921 <-> 133, 188, 403, 1418, 1921
922 <-> 1020, 1515
923 <-> 1597
924 <-> 1102
925 <-> 223, 767, 1175, 1920
926 <-> 438
927 <-> 311, 773
928 <-> 1162, 1317, 1571
929 <-> 929
930 <-> 1485, 1875
931 <-> 1809
932 <-> 1130, 1190, 1553
933 <-> 1303
934 <-> 274
935 <-> 149, 220
936 <-> 74, 736, 1574
937 <-> 203, 1942
938 <-> 33
939 <-> 1616
940 <-> 940, 1577
941 <-> 853
942 <-> 1315, 1345
943 <-> 624
944 <-> 117, 1531
945 <-> 1130
946 <-> 1789
947 <-> 775
948 <-> 1275
949 <-> 406, 1279, 1795
950 <-> 1803
951 <-> 407, 1440, 1785
952 <-> 130
953 <-> 663, 1460
954 <-> 883
955 <-> 1954
956 <-> 791, 1625
957 <-> 341, 737
958 <-> 1932
959 <-> 1624
960 <-> 437, 466, 1501
961 <-> 183, 994
962 <-> 1663
963 <-> 963, 1734
964 <-> 202, 1784
965 <-> 434, 1240
966 <-> 269, 766, 1395
967 <-> 643, 1414
968 <-> 559, 1523
969 <-> 1664
970 <-> 1670
971 <-> 1485
972 <-> 94, 190
973 <-> 984
974 <-> 283
975 <-> 485, 1584
976 <-> 1835
977 <-> 977
978 <-> 65
979 <-> 298, 774
980 <-> 21, 1217, 1897
981 <-> 628, 981
982 <-> 671
983 <-> 228, 789, 1321, 1933
984 <-> 670, 973, 1105
985 <-> 771, 1703
986 <-> 1478
987 <-> 115, 1725
988 <-> 626, 887, 1266
989 <-> 1366, 1601, 1964
990 <-> 1990
991 <-> 713, 730, 1932
992 <-> 1183
993 <-> 351
994 <-> 961
995 <-> 300, 309, 886, 995, 1305
996 <-> 771, 1048, 1203
997 <-> 887
998 <-> 1787
999 <-> 226, 1093, 1367
1000 <-> 310
1001 <-> 1468, 1913
1002 <-> 39, 1718
1003 <-> 482
1004 <-> 495
1005 <-> 500
1006 <-> 378, 571
1007 <-> 1256
1008 <-> 502
1009 <-> 1127
1010 <-> 1567
1011 <-> 1011, 1122, 1840
1012 <-> 1012
1013 <-> 752
1014 <-> 1538, 1775
1015 <-> 169, 1426
1016 <-> 1016, 1960
1017 <-> 277
1018 <-> 567, 1619
1019 <-> 228, 1495
1020 <-> 666, 922
1021 <-> 817, 839
1022 <-> 120, 323, 325, 834
1023 <-> 216, 1271
1024 <-> 1502
1025 <-> 1097
1026 <-> 654, 1455, 1739
1027 <-> 1823, 1906
1028 <-> 1903
1029 <-> 210, 869, 876, 1474, 1756
1030 <-> 26
1031 <-> 293, 1458
1032 <-> 477
1033 <-> 102, 582
1034 <-> 1984
1035 <-> 1977
1036 <-> 788
1037 <-> 1037
1038 <-> 1312, 1796, 1855
1039 <-> 558, 621, 1831
1040 <-> 267, 1478, 1776
1041 <-> 4
1042 <-> 65, 1881
1043 <-> 95, 1202, 1794
1044 <-> 760
1045 <-> 1426
1046 <-> 883
1047 <-> 277, 522, 739
1048 <-> 381, 996, 1578
1049 <-> 1554, 1779
1050 <-> 596
1051 <-> 241, 1117
1052 <-> 1533, 1637
1053 <-> 1797
1054 <-> 1814
1055 <-> 776, 1380
1056 <-> 1732, 1965
1057 <-> 192
1058 <-> 123
1059 <-> 1114
1060 <-> 1282
1061 <-> 340, 1325, 1517
1062 <-> 781, 1810
1063 <-> 645, 1567
1064 <-> 76, 404
1065 <-> 432
1066 <-> 1081, 1384, 1592
1067 <-> 1139
1068 <-> 401
1069 <-> 1327, 1873
1070 <-> 1070
1071 <-> 484, 1334
1072 <-> 851, 1072
1073 <-> 59
1074 <-> 1948
1075 <-> 1227
1076 <-> 68, 328, 1076
1077 <-> 138, 818, 1906
1078 <-> 191, 471, 1966
1079 <-> 1869
1080 <-> 1735, 1811, 1855
1081 <-> 821, 1066, 1895
1082 <-> 1740
1083 <-> 1083
1084 <-> 917
1085 <-> 1111
1086 <-> 1440, 1916
1087 <-> 523, 877
1088 <-> 32, 162
1089 <-> 97, 1854, 1948
1090 <-> 1102, 1896
1091 <-> 1466
1092 <-> 1655
1093 <-> 667, 999, 1159
1094 <-> 1248, 1961
1095 <-> 1243
1096 <-> 1635
1097 <-> 280, 1025
1098 <-> 492, 814
1099 <-> 749
1100 <-> 358, 874
1101 <-> 166, 1846
1102 <-> 924, 1090
1103 <-> 272, 1766
1104 <-> 1712
1105 <-> 984, 1105
1106 <-> 1106, 1620
1107 <-> 1107
1108 <-> 534, 1529
1109 <-> 821
1110 <-> 744
1111 <-> 1085, 1111, 1207
1112 <-> 1373
1113 <-> 459, 1430
1114 <-> 777, 1059
1115 <-> 1115
1116 <-> 418
1117 <-> 1051
1118 <-> 1474, 1583
1119 <-> 63, 323, 1194, 1302
1120 <-> 493
1121 <-> 1464
1122 <-> 1011
1123 <-> 886, 1848
1124 <-> 485
1125 <-> 47, 1705
1126 <-> 525
1127 <-> 311, 1009, 1566
1128 <-> 532
1129 <-> 476
1130 <-> 47, 342, 565, 932, 945
1131 <-> 223, 544
1132 <-> 629, 901
1133 <-> 1133, 1526
1134 <-> 13, 1913
1135 <-> 369, 1871
1136 <-> 846, 1879
1137 <-> 685
1138 <-> 1752
1139 <-> 1067, 1202
1140 <-> 425, 552, 786
1141 <-> 90, 146
1142 <-> 43, 1818
1143 <-> 184, 672, 1411
1144 <-> 157, 392, 1851
1145 <-> 1145, 1628
1146 <-> 19, 1574
1147 <-> 400, 663, 1265
1148 <-> 204, 1289, 1499, 1899
1149 <-> 303, 1797
1150 <-> 602, 826
1151 <-> 324
1152 <-> 664, 1518
1153 <-> 220, 1990
1154 <-> 218, 420, 1811
1155 <-> 1658
1156 <-> 1581, 1778, 1865
1157 <-> 638, 1849
1158 <-> 1445
1159 <-> 83, 1093, 1680
1160 <-> 616, 736
1161 <-> 1161
1162 <-> 18, 928
1163 <-> 70
1164 <-> 1868
1165 <-> 830, 1769
1166 <-> 1687
1167 <-> 531
1168 <-> 784, 1761
1169 <-> 554
1170 <-> 1206, 1327, 1754
1171 <-> 439, 1307, 1525
1172 <-> 903, 1172, 1542
1173 <-> 1173
1174 <-> 258
1175 <-> 925, 1869
1176 <-> 259
1177 <-> 240, 655, 1738
1178 <-> 612
1179 <-> 1537
1180 <-> 255, 1675
1181 <-> 220
1182 <-> 1377, 1877
1183 <-> 692, 992
1184 <-> 297, 762, 884, 1718
1185 <-> 467, 715, 1369
1186 <-> 51, 1600
1187 <-> 300
1188 <-> 541, 854
1189 <-> 113, 122, 250, 1653
1190 <-> 841, 932, 1448
1191 <-> 1225, 1345, 1427
1192 <-> 293, 1927
1193 <-> 1193, 1436
1194 <-> 1119
1195 <-> 253, 1864
1196 <-> 1432
1197 <-> 542, 1368, 1825
1198 <-> 125, 440
1199 <-> 1199, 1570, 1740
1200 <-> 1565
1201 <-> 731
1202 <-> 2, 1043, 1139, 1303
1203 <-> 602, 996, 1250
1204 <-> 1204
1205 <-> 230, 1514
1206 <-> 1170
1207 <-> 1111
1208 <-> 538, 551, 1904
1209 <-> 297
1210 <-> 557
1211 <-> 257, 1629
1212 <-> 1999
1213 <-> 291
1214 <-> 1977
1215 <-> 742, 1311, 1502, 1721
1216 <-> 344, 466, 489, 1457, 1810
1217 <-> 980
1218 <-> 888, 1294
1219 <-> 138
1220 <-> 99
1221 <-> 463, 866
1222 <-> 170
1223 <-> 882, 1817
1224 <-> 40, 1267
1225 <-> 1191, 1857
1226 <-> 741, 1309
1227 <-> 394, 496, 763, 1075, 1673
1228 <-> 397
1229 <-> 117, 1237
1230 <-> 1435, 1551, 1555
1231 <-> 456, 1885
1232 <-> 728
1233 <-> 242, 601
1234 <-> 1699
1235 <-> 16, 20, 1335
1236 <-> 1417
1237 <-> 1229, 1554
1238 <-> 1561
1239 <-> 1731, 1864
1240 <-> 393, 965, 1624
1241 <-> 356, 1241
1242 <-> 342
1243 <-> 1095, 1540
1244 <-> 41, 1802
1245 <-> 175, 613
1246 <-> 8, 522
1247 <-> 620
1248 <-> 46, 1094, 1430
1249 <-> 28, 447
1250 <-> 162, 219, 1203
1251 <-> 762
1252 <-> 879, 882
1253 <-> 735, 1867
1254 <-> 334, 501
1255 <-> 1396
1256 <-> 1007, 1366, 1636
1257 <-> 1257
1258 <-> 512, 1488
1259 <-> 271
1260 <-> 1898, 1978
1261 <-> 79
1262 <-> 817
1263 <-> 1387, 1405, 1580
1264 <-> 658
1265 <-> 1147, 1943
1266 <-> 988
1267 <-> 524, 1224
1268 <-> 329
1269 <-> 1537, 1608
1270 <-> 1540
1271 <-> 358, 903, 1023, 1398
1272 <-> 721, 1410
1273 <-> 1379
1274 <-> 9
1275 <-> 585, 948
1276 <-> 1920
1277 <-> 625
1278 <-> 1483
1279 <-> 393, 631, 949
1280 <-> 1572, 1842
1281 <-> 1281, 1833
1282 <-> 1060, 1283, 1422
1283 <-> 491, 1282
1284 <-> 139
1285 <-> 1893
1286 <-> 490, 785, 1598, 1661
1287 <-> 574
1288 <-> 474, 1600, 1824
1289 <-> 704, 865, 1148, 1681
1290 <-> 1660
1291 <-> 1291, 1301
1292 <-> 912, 1416
1293 <-> 1293, 1605
1294 <-> 457, 1218
1295 <-> 767
1296 <-> 105, 1859
1297 <-> 299, 488
1298 <-> 271, 693
1299 <-> 95, 1445
1300 <-> 98, 150, 174, 1422
1301 <-> 1291
1302 <-> 543, 1119
1303 <-> 808, 933, 1202, 1883
1304 <-> 174
1305 <-> 995
1306 <-> 305, 842
1307 <-> 290, 1171
1308 <-> 238, 1385
1309 <-> 528, 610, 1226
1310 <-> 464, 1520
1311 <-> 1215, 1830
1312 <-> 1038
1313 <-> 1835
1314 <-> 1916
1315 <-> 942
1316 <-> 118, 718
1317 <-> 116, 928
1318 <-> 480, 857
1319 <-> 737
1320 <-> 558, 572
1321 <-> 983
1322 <-> 790, 1368
1323 <-> 1846
1324 <-> 1920
1325 <-> 1061, 1409, 1459
1326 <-> 1486
1327 <-> 100, 1069, 1170, 1441, 1641
1328 <-> 222, 1328
1329 <-> 26
1330 <-> 330
1331 <-> 152, 1358, 1380, 1690, 1888
1332 <-> 1776
1333 <-> 26, 181
1334 <-> 1071
1335 <-> 561, 1235
1336 <-> 601
1337 <-> 1689
1338 <-> 1528, 1689
1339 <-> 1422, 1588
1340 <-> 1911, 1964
1341 <-> 1887
1342 <-> 40, 438
1343 <-> 586, 755, 1806, 1956
1344 <-> 1344, 1354, 1623, 1923
1345 <-> 729, 942, 1191
1346 <-> 404
1347 <-> 9, 1524, 1757, 1945, 1978
1348 <-> 1828
1349 <-> 208, 497
1350 <-> 385
1351 <-> 1399
1352 <-> 0
1353 <-> 1731
1354 <-> 1344, 1587, 1870
1355 <-> 1522, 1625
1356 <-> 804, 1356
1357 <-> 690
1358 <-> 472, 594, 1331
1359 <-> 284, 1934
1360 <-> 63, 1403
1361 <-> 274, 395, 1361
1362 <-> 316
1363 <-> 802, 1779, 1975
1364 <-> 1377
1365 <-> 239, 337
1366 <-> 989, 1256, 1420
1367 <-> 999, 1483
1368 <-> 456, 1197, 1322
1369 <-> 303, 1185
1370 <-> 214, 280, 1747, 1764
1371 <-> 1371
1372 <-> 346, 1546
1373 <-> 1112, 1665
1374 <-> 652
1375 <-> 596, 1375
1376 <-> 477, 1730, 1807
1377 <-> 1182, 1364
1378 <-> 862, 895, 1378, 1391
1379 <-> 1273, 1698, 1715
1380 <-> 1055, 1331, 1847
1381 <-> 574, 1654
1382 <-> 248, 318, 497, 623
1383 <-> 4, 191
1384 <-> 489, 1066
1385 <-> 1308
1386 <-> 50
1387 <-> 1263
1388 <-> 1567
1389 <-> 783
1390 <-> 324, 840, 1772
1391 <-> 1378
1392 <-> 170, 1641
1393 <-> 368, 885
1394 <-> 1645, 1655
1395 <-> 741, 966
1396 <-> 1255, 1396, 1965
1397 <-> 20, 198
1398 <-> 224, 611, 1271, 1549
1399 <-> 444, 1351, 1660
1400 <-> 123, 141
1401 <-> 94
1402 <-> 658
1403 <-> 195, 752, 1360
1404 <-> 27, 502
1405 <-> 1263
1406 <-> 1514
1407 <-> 222
1408 <-> 256
1409 <-> 426, 545, 1325, 1980, 1981
1410 <-> 1272
1411 <-> 35, 1143, 1444
1412 <-> 89
1413 <-> 574
1414 <-> 967, 1680
1415 <-> 1593
1416 <-> 2, 1292
1417 <-> 1236, 1599
1418 <-> 921, 1464
1419 <-> 233
1420 <-> 1366, 1564
1421 <-> 916
1422 <-> 1282, 1300, 1339
1423 <-> 355, 384, 754
1424 <-> 1488
1425 <-> 717, 797
1426 <-> 1015, 1045
1427 <-> 1191
1428 <-> 917
1429 <-> 626, 1911
1430 <-> 512, 1113, 1248
1431 <-> 282, 297, 793, 1663
1432 <-> 429, 1196
1433 <-> 449
1434 <-> 404
1435 <-> 1230
1436 <-> 1193, 1827
1437 <-> 1809
1438 <-> 193, 630
1439 <-> 1773
1440 <-> 951, 1086
1441 <-> 1327
1442 <-> 219, 1808
1443 <-> 133, 650
1444 <-> 1411, 1527, 1689
1445 <-> 401, 538, 1158, 1299
1446 <-> 1831
1447 <-> 727, 773
1448 <-> 171, 1190
1449 <-> 230, 1751
1450 <-> 384
1451 <-> 1451
1452 <-> 1607
1453 <-> 1453
1454 <-> 1454
1455 <-> 412, 1026
1456 <-> 328, 564
1457 <-> 658, 1216
1458 <-> 813, 1031
1459 <-> 805, 1325
1460 <-> 189, 953
1461 <-> 72, 1461, 1573
1462 <-> 142
1463 <-> 142, 178, 597
1464 <-> 1121, 1418, 1905
1465 <-> 1810
1466 <-> 1091, 1751
1467 <-> 1484, 1697
1468 <-> 244, 256, 1001, 1503
1469 <-> 179, 229, 855
1470 <-> 1889
1471 <-> 284
1472 <-> 204, 359, 1748
1473 <-> 431, 641, 675, 715, 1753
1474 <-> 478, 1029, 1118
1475 <-> 1475, 1595
1476 <-> 622
1477 <-> 271
1478 <-> 986, 1040
1479 <-> 1479
1480 <-> 132, 777
1481 <-> 1481
1482 <-> 366
1483 <-> 1278, 1367
1484 <-> 303, 731, 1467, 1778
1485 <-> 868, 930, 971
1486 <-> 764, 877, 1326
1487 <-> 269, 885
1488 <-> 1258, 1424
1489 <-> 1839
1490 <-> 365, 650
1491 <-> 883, 1655
1492 <-> 630, 1505, 1710, 1866
1493 <-> 694, 1785
1494 <-> 373, 808, 1838
1495 <-> 626, 1019
1496 <-> 476, 1666
1497 <-> 1916
1498 <-> 708
1499 <-> 587, 1148, 1596
1500 <-> 1800, 1974
1501 <-> 960
1502 <-> 1024, 1215, 1676
1503 <-> 790, 1468
1504 <-> 206
1505 <-> 448, 1492
1506 <-> 299
1507 <-> 215, 1579
1508 <-> 1678
1509 <-> 558
1510 <-> 135, 1693
1511 <-> 40
1512 <-> 45, 361, 1635
1513 <-> 5
1514 <-> 1205, 1406
1515 <-> 531, 922, 1730
1516 <-> 259
1517 <-> 1061
1518 <-> 109, 371, 1152
1519 <-> 312, 1835
1520 <-> 1310
1521 <-> 460
1522 <-> 331, 377, 519, 1355, 1852, 1871
1523 <-> 706, 968, 1607
1524 <-> 1347
1525 <-> 1171, 1728
1526 <-> 1133
1527 <-> 1444
1528 <-> 1338
1529 <-> 84, 695, 1108
1530 <-> 834
1531 <-> 419, 944
1532 <-> 511, 740
1533 <-> 1052
1534 <-> 1572
1535 <-> 701, 1606
1536 <-> 1536
1537 <-> 347, 1179, 1269
1538 <-> 420, 1014
1539 <-> 1648
1540 <-> 324, 1243, 1270
1541 <-> 1952
1542 <-> 1172
1543 <-> 659
1544 <-> 569, 889
1545 <-> 1545
1546 <-> 1372
1547 <-> 722
1548 <-> 33
1549 <-> 1398
1550 <-> 69, 583, 901
1551 <-> 1230, 1907
1552 <-> 743, 1841
1553 <-> 932
1554 <-> 38, 433, 1049, 1237
1555 <-> 1230
1556 <-> 605
1557 <-> 1728, 1980
1558 <-> 713
1559 <-> 1573
1560 <-> 837, 1803
1561 <-> 799, 1238, 1712
1562 <-> 254
1563 <-> 669, 1610
1564 <-> 1420, 1832
1565 <-> 330, 1200
1566 <-> 1127
1567 <-> 1010, 1063, 1388
1568 <-> 719
1569 <-> 1851
1570 <-> 1199
1571 <-> 928, 1752
1572 <-> 1280, 1534
1573 <-> 1461, 1559
1574 <-> 34, 70, 936, 1146
1575 <-> 207
1576 <-> 578
1577 <-> 435, 940
1578 <-> 1048
1579 <-> 1507
1580 <-> 1263, 1610
1581 <-> 1156
1582 <-> 208, 1839
1583 <-> 774, 1118, 1902
1584 <-> 975
1585 <-> 882
1586 <-> 103, 644, 1637
1587 <-> 376, 1354
1588 <-> 1339, 1913
1589 <-> 1791, 1855
1590 <-> 265, 697
1591 <-> 1631
1592 <-> 1066, 1692, 1707
1593 <-> 891, 1415, 1723
1594 <-> 1594, 1959
1595 <-> 1475
1596 <-> 733, 1499
1597 <-> 879, 923, 1991
1598 <-> 708, 1286, 1965
1599 <-> 789, 1417
1600 <-> 415, 1186, 1288
1601 <-> 402, 549, 989
1602 <-> 533, 1602
1603 <-> 250
1604 <-> 1784
1605 <-> 1293
1606 <-> 599, 707, 1535
1607 <-> 594, 1452, 1523
1608 <-> 79, 184, 238, 858, 1269
1609 <-> 1638
1610 <-> 564, 1563, 1580
1611 <-> 667
1612 <-> 73, 413, 1612
1613 <-> 1997
1614 <-> 243
1615 <-> 323
1616 <-> 553, 939
1617 <-> 658
1618 <-> 245, 337, 1618
1619 <-> 208, 1018
1620 <-> 1106
1621 <-> 69
1622 <-> 440, 1876
1623 <-> 383, 1344
1624 <-> 674, 959, 1240
1625 <-> 177, 197, 956, 1355
1626 <-> 1626, 1790
1627 <-> 1722
1628 <-> 1145
1629 <-> 343, 607, 1211
1630 <-> 507
1631 <-> 1591, 1886
1632 <-> 553
1633 <-> 38, 89
1634 <-> 606
1635 <-> 1096, 1512
1636 <-> 761, 1256
1637 <-> 1052, 1586
1638 <-> 1609, 1638
1639 <-> 1978
1640 <-> 500, 1712
1641 <-> 451, 849, 1327, 1392, 1994
1642 <-> 1785
1643 <-> 1960
1644 <-> 41, 1644
1645 <-> 1394
1646 <-> 841, 1646, 1936
1647 <-> 836
1648 <-> 179, 1539
1649 <-> 516, 1717
1650 <-> 673
1651 <-> 451
1652 <-> 704
1653 <-> 1189
1654 <-> 1381, 1654, 1928
1655 <-> 1092, 1394, 1491
1656 <-> 1755, 1767
1657 <-> 903
1658 <-> 1155, 1658
1659 <-> 1659, 1747
1660 <-> 527, 1290, 1399
1661 <-> 1286
1662 <-> 173, 236, 548
1663 <-> 962, 1431
1664 <-> 139, 969, 1664
1665 <-> 766, 1373
1666 <-> 1496
1667 <-> 521, 790, 1919
1668 <-> 1668
1669 <-> 160, 830
1670 <-> 754, 811, 970
1671 <-> 599, 748
1672 <-> 919
1673 <-> 250, 1227
1674 <-> 1771, 1838
1675 <-> 160, 769, 1180, 1729
1676 <-> 1502
1677 <-> 106
1678 <-> 318, 1508
1679 <-> 888
1680 <-> 1159, 1414
1681 <-> 483, 1289
1682 <-> 1905
1683 <-> 863, 1798, 1891
1684 <-> 81, 234
1685 <-> 74, 77
1686 <-> 590
1687 <-> 1166, 1709
1688 <-> 1820
1689 <-> 1337, 1338, 1444
1690 <-> 1331
1691 <-> 660
1692 <-> 1592
1693 <-> 72, 1510, 1957
1694 <-> 674
1695 <-> 355, 357, 1755
1696 <-> 184, 1828
1697 <-> 1467
1698 <-> 1379
1699 <-> 699, 1234, 1699, 1784
1700 <-> 101
1701 <-> 614, 1728, 1743
1702 <-> 58
1703 <-> 182, 985
1704 <-> 1704
1705 <-> 1125
1706 <-> 1706
1707 <-> 503, 1592
1708 <-> 792, 1880, 1882
1709 <-> 686, 1687
1710 <-> 1492
1711 <-> 1711
1712 <-> 1104, 1561, 1640
1713 <-> 364, 810
1714 <-> 1714, 1845
1715 <-> 632, 844, 1379
1716 <-> 631, 906
1717 <-> 207, 838, 1649, 1717
1718 <-> 846, 1002, 1184
1719 <-> 1719
1720 <-> 247, 432, 1720, 1914
1721 <-> 1215
1722 <-> 799, 1627
1723 <-> 307, 1593
1724 <-> 410, 1756, 1887
1725 <-> 296, 987
1726 <-> 676, 878, 1837
1727 <-> 641
1728 <-> 1525, 1557, 1701, 1875
1729 <-> 1675
1730 <-> 483, 1376, 1515, 1916
1731 <-> 363, 1239, 1353
1732 <-> 647, 1056
1733 <-> 320, 527, 1733
1734 <-> 963
1735 <-> 779, 1080
1736 <-> 202
1737 <-> 202
1738 <-> 871, 1177
1739 <-> 1026
1740 <-> 700, 1082, 1199, 1941
1741 <-> 231
1742 <-> 1936
1743 <-> 1701
1744 <-> 462, 915
1745 <-> 1798
1746 <-> 333, 591
1747 <-> 1370, 1659
1748 <-> 1472, 1930
1749 <-> 797
1750 <-> 1949
1751 <-> 1449, 1466
1752 <-> 156, 1138, 1571
1753 <-> 476, 1473
1754 <-> 1170
1755 <-> 29, 1656, 1695
1756 <-> 388, 1029, 1724
1757 <-> 1347
1758 <-> 315, 648, 823, 1851
1759 <-> 1759
1760 <-> 1760, 1889
1761 <-> 374, 1168
1762 <-> 1814
1763 <-> 380, 783, 1863
1764 <-> 1370, 1949
1765 <-> 366
1766 <-> 782, 1103
1767 <-> 271, 569, 1656
1768 <-> 362
1769 <-> 822, 1165
1770 <-> 385, 605, 1800
1771 <-> 726, 1674, 1907
1772 <-> 677, 1390
1773 <-> 1439, 1773
1774 <-> 300
1775 <-> 1014
1776 <-> 176, 1040, 1332
1777 <-> 1841
1778 <-> 1156, 1484
1779 <-> 429, 579, 1049, 1363
1780 <-> 391, 627
1781 <-> 494
1782 <-> 101
1783 <-> 15
1784 <-> 42, 964, 1604, 1699, 1996
1785 <-> 951, 1493, 1642
1786 <-> 712, 1786
1787 <-> 998, 1787
1788 <-> 526, 839
1789 <-> 71, 174, 946, 1973
1790 <-> 1626
1791 <-> 1589
1792 <-> 108, 1842
1793 <-> 250
1794 <-> 1043
1795 <-> 949
1796 <-> 1038
1797 <-> 1053, 1149
1798 <-> 1683, 1745, 1830
1799 <-> 111, 165, 458
1800 <-> 215, 338, 1500, 1770
1801 <-> 787
1802 <-> 1244
1803 <-> 950, 1560
1804 <-> 598
1805 <-> 238
1806 <-> 1343
1807 <-> 1376
1808 <-> 612, 1442
1809 <-> 176, 505, 931, 1437
1810 <-> 1062, 1216, 1465
1811 <-> 1080, 1154
1812 <-> 1832, 1858
1813 <-> 1813
1814 <-> 690, 1054, 1762
1815 <-> 264, 268
1816 <-> 1816
1817 <-> 1223
1818 <-> 847, 1142
1819 <-> 509
1820 <-> 229, 1688
1821 <-> 1976
1822 <-> 8
1823 <-> 1027
1824 <-> 1288
1825 <-> 1197
1826 <-> 1894, 1977
1827 <-> 1436
1828 <-> 1348, 1696
1829 <-> 897
1830 <-> 1311, 1798
1831 <-> 1039, 1446
1832 <-> 115, 1564, 1812
1833 <-> 844, 1281
1834 <-> 70
1835 <-> 246, 709, 976, 1313, 1519
1836 <-> 444
1837 <-> 1726
1838 <-> 1494, 1674
1839 <-> 275, 623, 1489, 1582
1840 <-> 1011
1841 <-> 460, 1552, 1777
1842 <-> 536, 794, 1280, 1792
1843 <-> 631
1844 <-> 334, 826
1845 <-> 1714
1846 <-> 498, 1101, 1323, 1846
1847 <-> 1380
1848 <-> 1123
1849 <-> 1157
1850 <-> 1850
1851 <-> 1144, 1569, 1758
1852 <-> 571, 1522
1853 <-> 289
1854 <-> 1089
1855 <-> 107, 1038, 1080, 1589
1856 <-> 1947
1857 <-> 895, 1225
1858 <-> 326, 1812
1859 <-> 248, 450, 1296
1860 <-> 1860
1861 <-> 746, 1861
1862 <-> 1882
1863 <-> 217, 1763
1864 <-> 0, 1195, 1239
1865 <-> 1156, 1925
1866 <-> 1492
1867 <-> 890, 1253, 1867
1868 <-> 1164, 1893
1869 <-> 1079, 1175
1870 <-> 88, 1354
1871 <-> 1135, 1522, 1940
1872 <-> 1872
1873 <-> 1069
1874 <-> 215
1875 <-> 729, 930, 1728
1876 <-> 1622, 1903
1877 <-> 73, 1182
1878 <-> 1970
1879 <-> 1136, 1912, 1958, 1989
1880 <-> 864, 1708
1881 <-> 475, 1042
1882 <-> 249, 1708, 1862
1883 <-> 1303
1884 <-> 31
1885 <-> 680, 1231
1886 <-> 40, 1631
1887 <-> 462, 1341, 1724
1888 <-> 1331
1889 <-> 1470, 1760
1890 <-> 161, 203
1891 <-> 424, 1683
1892 <-> 1892
1893 <-> 254, 1285, 1868
1894 <-> 13, 172, 1826
1895 <-> 114, 382, 1081
1896 <-> 525, 1090
1897 <-> 606, 803, 980
1898 <-> 322, 1260
1899 <-> 348, 550, 749, 1148
1900 <-> 408, 1900
1901 <-> 80, 859
1902 <-> 673, 1583
1903 <-> 1028, 1876
1904 <-> 1208
1905 <-> 1464, 1682
1906 <-> 1027, 1077
1907 <-> 1551, 1771
1908 <-> 408
1909 <-> 1909
1910 <-> 359, 812
1911 <-> 20, 587, 1340, 1429
1912 <-> 529, 1879
1913 <-> 1001, 1134, 1588
1914 <-> 1720
1915 <-> 502
1916 <-> 848, 1086, 1314, 1497, 1730
1917 <-> 1951
1918 <-> 401
1919 <-> 273, 1667
1920 <-> 925, 1276, 1324
1921 <-> 58, 921
1922 <-> 48
1923 <-> 1344
1924 <-> 1993
1925 <-> 1865
1926 <-> 330
1927 <-> 670, 1192
1928 <-> 1654
1929 <-> 907
1930 <-> 600, 1748
1931 <-> 1931
1932 <-> 958, 991
1933 <-> 983
1934 <-> 402, 1359
1935 <-> 294, 682, 784
1936 <-> 1646, 1742
1937 <-> 1937, 1971
1938 <-> 126, 1938
1939 <-> 1998
1940 <-> 257, 1871
1941 <-> 1740
1942 <-> 780, 937
1943 <-> 110, 1265
1944 <-> 820, 837
1945 <-> 264, 1347
1946 <-> 378
1947 <-> 170, 537, 573, 829, 1856
1948 <-> 1074, 1089
1949 <-> 1750, 1764
1950 <-> 478, 689
1951 <-> 131, 254, 1917
1952 <-> 208, 1541
1953 <-> 803
1954 <-> 547, 904, 955, 1954
1955 <-> 528
1956 <-> 1343
1957 <-> 99, 191, 1693
1958 <-> 1879
1959 <-> 1594
1960 <-> 1016, 1643
1961 <-> 1094
1962 <-> 37, 219, 912
1963 <-> 262
1964 <-> 989, 1340
1965 <-> 1056, 1396, 1598
1966 <-> 1078
1967 <-> 77
1968 <-> 365
1969 <-> 614
1970 <-> 1878, 1970
1971 <-> 1937
1972 <-> 728, 732
1973 <-> 1789
1974 <-> 14, 1500
1975 <-> 1363
1976 <-> 1821, 1976
1977 <-> 720, 1035, 1214, 1826
1978 <-> 1260, 1347, 1639
1979 <-> 577, 661
1980 <-> 1409, 1557
1981 <-> 1409
1982 <-> 272, 724
1983 <-> 636
1984 <-> 148, 771, 1034
1985 <-> 155
1986 <-> 216
1987 <-> 1987
1988 <-> 553, 584
1989 <-> 313, 1879
1990 <-> 990, 1153
1991 <-> 1597
1992 <-> 364
1993 <-> 90, 765, 1924
1994 <-> 645, 1641
1995 <-> 558
1996 <-> 643, 1784
1997 <-> 362, 1613
1998 <-> 1939, 1998
1999 <-> 90, 1212"
  end
end
