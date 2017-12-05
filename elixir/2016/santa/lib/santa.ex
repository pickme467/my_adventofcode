defmodule Santa.Day13 do
  @moduledoc """

  Day 13 is tricky. We know where we can go based on our current
  location and three new paths from that location. Three, because one
  is just our origin where we came from. So to make move we direct
  three locations to assess themselves and that's it.

  We need stopping conditions. Here they are:
  - current location is invalid
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
  - arbiter that:
    - informs about shortest path found or longest path allowed
    - accepts new shortest path
    - returns shortest path

  """
  @doc """
  Day 13 Part one acceptance test

  iex> Santa.Day13.go_part_one
  86
  """
  def go_part_one do
    main_supervisor = make_supervision_tree(Santa.Day13.Crawler)
    run_first_crawler(main_supervisor)
    {crawler_supervisor, arbiter} = get_crawler_sup_and_arbiter(main_supervisor)
    wait_for_result(crawler_supervisor)
    Santa.Day13.Arbiter.get_shortest(arbiter)
  end

  @doc """
  Day 13 Part two acceptance test

  iex> Santa.Day13.go_part_two
  127
  """
  def go_part_two do
    main_supervisor = make_supervision_tree(Santa.Day13.Crawler50)
    run_first_crawler(main_supervisor)
    {crawler_supervisor, arbiter} = get_crawler_sup_and_arbiter(main_supervisor)
    wait_for_result(crawler_supervisor)
    Santa.Day13.Arbiter.get_all_coordinates(arbiter)
  end

  def day_13_input do
    {1364, 31, 39}
  end

  def wait_for_result(supervisor) do
    case [] == Supervisor.which_children(supervisor) do
      true -> :ok
      false -> :timer.sleep(500)
      wait_for_result(supervisor)
    end
  end

  defp make_supervision_tree(crawler_module) do
    {:ok, main_supervisor} = Supervisor.start_link([
      Santa.Day13.Arbiter.child_spec([]),
      Santa.Day13.Crawler.Supervisor.child_spec(crawler_module)],
      strategy: :one_for_one)
    main_supervisor
  end

  defp get_crawler_sup_and_arbiter(main_supervisor) do
    [{_, crawler_supervisor, :supervisor, _}, {_, arbiter, :worker, _}] =
      Supervisor.which_children(main_supervisor)
    {crawler_supervisor, arbiter}
  end

  defp run_first_crawler(main_supervisor) do
    {crawler_supervisor, arbiter} = get_crawler_sup_and_arbiter(main_supervisor)
    {_my_number, target_x, target_y} = day_13_input()
    start = {1, 1}
    target = {target_x, target_y}
    {:ok, _first_task} = Santa.Day13.Crawler.Supervisor.create_crawler(
      crawler_supervisor, arbiter, target, start, [])
  end

end

defmodule Santa.Day13.Step do
  @doc false

  @doc """
  The following coordinate is a wall for my input puzzle

  iex> Santa.Day13.Step.is_wall(Santa.Day13.Step.get_my_number, {1, 2})
  true

  The following coordinate is not a wall for my input puzzle

  iex> Santa.Day13.Step.is_wall(Santa.Day13.Step.get_my_number, {1, 1})
  false

  """
  def is_wall number, {x, y} do
    require Integer
    value = number + (x * x + 3 * x + 2 * x * y + y + y * y)
    sum_all_ones = fn (bit, sum) -> sum + bit end
    all_ones = Integer.digits(value, 2)
    |> Enum.reduce(0, sum_all_ones)
    Integer.is_odd(all_ones)
  end

  @doc " Return my day13 puzzle input number"
  def get_my_number() do
    {number, _, _} = Santa.Day13.day_13_input()
    number
  end

  @doc """
  If target is equal current - we found it. Return path with
  the final point excluded

  iex> Santa.Day13.Step.execute(10, {5,5}, {5,5}, [:path])
  {:found, [:path]}
  """
  def execute(_max_path, target, target, path) do
    {:found, path}
  end
  def execute(_max_path, _target, {x, y}, path) when
  x < 0 or y < 0 do
    {:not_found, path}
  end
  def execute(max_path, _target, _current, path)
  when max_path <= length(path) do
    {:not_found, path}
  end
  def execute(_max_path, _target, {x, y} = current, path) do
    case (false == is_wall(get_my_number(), {x, y}))
     and (false == Enum.member?(path, current))
     do
       true  -> {:continue, [current | path], get_neighbours(current)}
       false -> {:not_found, path}
     end
  end

  defp get_neighbours({x, y}) do
    for a <- [x+1, x-1] do
      {a, y}
      end
    ++
    for b <- [y+1, y-1] do
      {x, b}
    end
  end
end

defmodule Santa.Day13.Arbiter do
  use GenServer, start: {__MODULE__, :start_link, []}

  def start_link() do
    GenServer.start_link(__MODULE__, %{shortest: 100,
                                       all_coordinates: MapSet.new})
  end

  def get_shortest(server) do
    GenServer.call(server, :get_shortest)
  end

  def new_shortest(server, list) do
    GenServer.call(server, {:new_shortest, list})
  end

  def add_path(server, list) do
    GenServer.cast(server, {:add_path, list})
  end

  def get_all_coordinates(server) do
    GenServer.call(server, :get_all_coordinates)
  end

  def handle_call(:get_shortest, _from, %{shortest: value} = map)
  when is_integer(value) do
    {:reply, value, map}
  end

  def handle_call(:get_shortest, _from, %{shortest: value} = map) do
    {:reply, length(value), map}
  end

  def handle_call({:new_shortest, value}, _from, %{shortest: old} = map)
  when (is_integer(old) and length(value) < old)
  or (length(value) < length(old)) do
    {:reply, :ok, %{map | shortest: value}}
  end

  def handle_call({:new_shortest, _value}, _from, map) do
    {:reply, :ok, map}
  end

  def handle_call(:get_all_coordinates, _from, %{all_coordinates: all} = map) do
    {:reply, MapSet.size(all), map}
  end

  def handle_cast({:add_path, list}, %{all_coordinates: set} = map) do
    new = MapSet.new(list)
    {:noreply, %{map | all_coordinates: MapSet.union(new, set)}}
  end
end

defmodule Santa.Day13.Crawler do
  use Task, start: {__MODULE__, :start_link, []}

  @doc false
  def start_link(supervisor, arbiter, target, current, path) do
    Task.start_link(__MODULE__, :run, [supervisor, arbiter,
                                       target, current, path])
  end

  def run(supervisor, arbiter, target, current, path) do
    shortest_path = Santa.Day13.Arbiter.get_shortest(arbiter)
    case Santa.Day13.Step.execute(shortest_path, target, current, path) do
      {:not_found, _path} -> :ok
      {:found, path} -> Santa.Day13.Arbiter.new_shortest(arbiter, path)
      {:continue, path, neighbours} ->
        Enum.each(neighbours,
          fn (new_location) ->
            Santa.Day13.Crawler.Supervisor.create_crawler(
              supervisor, arbiter, target, new_location, path)
          end)
    end
  end
end

defmodule Santa.Day13.Crawler50 do
  use Task, start: {__MODULE__, :start_link, []}

  @doc false
  def start_link(supervisor, arbiter, target, current, path) do
    Task.start_link(__MODULE__, :run, [supervisor, arbiter,
                                       target, current, path])
  end

  def run(supervisor, arbiter, target, current, path) do
    shortest_path = 51 #first location plus 50 steps
    case Santa.Day13.Step.execute(shortest_path, target, current, path) do
      {:not_found, path} -> Santa.Day13.Arbiter.add_path(arbiter, path)
      {:continue, path, neighbours} ->
        Enum.each(neighbours,
          fn (new_location) ->
            Santa.Day13.Crawler.Supervisor.create_crawler(
              supervisor, arbiter, target, new_location, path)
          end)
    end
  end
end

defmodule Santa.Day13.Crawler.Supervisor do
  use Supervisor, strategy: :simple_one_for_one

  def start_link(crawler_module) do
    Supervisor.start_link(__MODULE__, [crawler_module])
  end

  def init([crawler_module]) do
    Supervisor.init([crawler_module.child_spec([])],
      strategy: :simple_one_for_one)
  end

  def create_crawler(supervisor, arbiter, target, current, path) do
    Supervisor.start_child(supervisor, [supervisor, arbiter, target,
                                        current, path])
  end
end

defmodule Santa.Day14 do
  @doc """
  noiex> Santa.Day14.go_part_one
  15035
  """
  def go_part_one do
    generate_keys(&calculate_md5/1)
    #:all_wrong
  end

  @doc """
  noiex> Santa.Day14.go_part_two
  19968
  """
  def go_part_two do
    generate_keys(&calculate_md5_2016/1)
  end

  def generate_keys(calculation) do
    generate_keys(calculation, 0, {{0, []}, 0, 0})
  end

  def generate_keys(_, _, {_, last_found, 64}) do
    last_found
  end

  def generate_keys(calculation, index,
    {cache, found_index, found_count}) do
    new_cache = make_new_cache(calculation, cache, index)
    case is_key(new_cache, index) do
      true -> generate_keys(calculation, index + 1,
                              {new_cache, index, found_count + 1})
      false -> generate_keys(calculation, index + 1,
                               {new_cache,
                                found_index,
                                found_count})
    end
  end

  defp get_threes_and_fives({_, cache}, index) do
    {^index, found} =
      List.keyfind(cache, index, 0, {index, [threes: [], fives: []]})
    found
  end

  defp make_new_cache(_, {first_not_calculated, _} = old_cache, index)
  when first_not_calculated > index + 1001 do
                                old_cache
  end

  defp make_new_cache(calculation, {first_not_calculated, cache}, index) do
    new_cache = make_cache(calculation, cache, first_not_calculated)
    make_new_cache(calculation, {first_not_calculated + 1, new_cache}, index)
  end

  defp make_cache(calculation, cache, index) do
    string = make_string(index)
    md5 = calculation.(string)
    case has_three_or_five(md5) do
      [] -> cache
      has_it -> [{index, has_it} | cache]
    end
  end

  def is_key(cache, index) do
      [threes: found_three, fives:  _ ] = get_threes_and_fives(cache, index)
    case found_three do
      [] -> false
      _ -> check_fives_for(cache, index, found_three, 1000)
    end
  end

  defp check_fives_for(_, _, _, 0) do
    false
  end

  defp check_fives_for(cache, index, found, iteration) do
    [_ , fives: found_fives] =
      get_threes_and_fives(cache, index + 1 + (1000 - iteration))
    case found_fives do
      [] ->
        check_fives_for(cache, index, found, iteration - 1)
      _ -> case MapSet.size(
             MapSet.intersection(
               MapSet.new(found), MapSet.new(found_fives))) > 0 do
             true  -> true
             false -> check_fives_for(cache, index,
                        found, iteration - 1)
           end
    end
  end

  def input() do
    "ihaygndm"
  end

  @doc """
  Number plus input

  iex> Santa.Day14.make_string(1)
  "ihaygndm1"
  """
  def make_string(number) do
    input() <> Integer.to_string(number, 10)
  end

  def calculate_md5(input) do
    String.downcase(Base.encode16(:crypto.hash(:md5, input)))
  end

  def calculate_md5_2016(input) do
    first = calculate_md5(input)
    calculate_md5_times(first, 2016)
  end

  defp calculate_md5_times(last, 0) do
    last
  end

  defp calculate_md5_times(middle, times) do
    calculate_md5_times(calculate_md5(middle), times - 1)
  end

  @doc """
  Let's check if it works

  iex> Santa.Day14.has_three_or_five("aaabc")
  [threes: ["a"], fives: []]

  iex> Santa.Day14.has_three_or_five("777bc77")
  [threes: ["7"], fives: []]

  iex> Santa.Day14.has_three_or_five("77bc77")
  [threes: [], fives: []]

  iex> Santa.Day14.has_three_or_five("abbaccc77dddbc77")
  [threes: ["c"], fives: []]

  iex> Santa.Day14.has_three_or_five("abbzzzzzzzacc77ddbc77")
  [threes: ["z"], fives: ["z"]]
  """
  def has_three_or_five(input) do
    has_three_or_five(input, MapSet.new, MapSet.new)
  end

  def has_three_or_five(
    <<a::bytes-size(1), a::bytes-size(1), a::bytes-size(1),
      a::bytes-size(1), a::bytes-size(1), rest::binary>>,
    found_three, found_five) do
    new_three = case MapSet.size(found_three) do
                  0 -> MapSet.put(found_three, a)
                  1 -> found_three
                end
    has_three_or_five(rest, new_three,
      MapSet.put(found_five, a))
  end

  def has_three_or_five(
    <<a::bytes-size(1), a::bytes-size(1), a::bytes-size(1), rest::binary>>,
    found_three, found_five) do
      new_three = case MapSet.size(found_three) do
                    0 -> MapSet.put(found_three, a)
                    1 -> found_three
                  end
    has_three_or_five(rest, new_three, found_five)
  end

  def has_three_or_five(anything, found_three, found_five) do
    case String.length(anything) <= 3 do
      true -> [threes: Enum.sort(MapSet.to_list(found_three)),
               fives:  Enum.sort(MapSet.to_list(found_five))]
      false ->
        <<_a::bytes-size(1), rest::binary>> = anything
        has_three_or_five(rest, found_three, found_five)
    end
  end
end

defmodule Santa.Day15 do
  @moduledoc """
  This one is easy. I won't do any translations from original input
  into some internal representation. Instead I will make this
  representation right away and just use it as a set of matchers. If
  all matchers match given time that meant we found the right
  time. End of story.
  """

  @doc """
  Acceptance test for part one

  iex> Santa.Day15.go_part_one()
  148737
  """
  def go_part_one() do
    matchers = get_matchers()
    find_first_that_match(19 + 5, matchers)
  end

  @doc """
  Part two has one additional matcher, so let's use previous set,
  extend it with the new one and pass to the function that does the
  search

  Acceptance test for part two

  iex> Santa.Day15.go_part_two()
  2353212
  """
  def go_part_two() do
    matchers = get_matchers()
    matchers = [fn (x) -> rem(x + 7, 11) == 0 end,] ++ matchers
    find_first_that_match(19 + 5, matchers)
  end

  defp get_matchers() do
    [fn (x) -> rem(x - 2,  5) == 0 end,
     fn (x) -> rem(x - 4, 13) == 0 end,
     fn (x) -> rem(x - 4, 17) == 0 end,
     fn (x) -> rem(x + 3,  3) == 0 end,
     fn (x) -> rem(x - 5, 19) == 0 end,
     fn (x) -> rem(x + 6,  7) == 0 end,
    ]
  end

  defp find_first_that_match(t, matchers) do
    case Enum.all?(matchers, fn (matcher) -> matcher.(t) end) do
      true -> t
      false -> find_first_that_match(t + 19, matchers)
    end
  end
end

defmodule Santa.Day16 do
  @moduledoc """

  This day's exercise is tricky. Attempt done for the first part is
  insufficient for the second part. It requires huge amount of
  computation power and/or memory. So the solution is to find pattern
  instead of building brute force attempt.

  If input is A and its reversed version is B then each iteration is
  like the following pattern:

  AxBxAxBxAxB

  Where x is eiter zero or one. So to resolve the exercise it is
  enough to find correct sequence of xxxx with the total length of
  string greater or equal to given length. Each new generation gives 2
  times previous length plus one, so it is easy to find how many x'es
  there will be in the final sequence.

  x'es follow the following rule
  [sequence 0] = [0]
  [sequence (a+1)] = [sequence (a)] ++ [0] ++ [sequence (a) reversed]

  So let's implement it that way
  """
  require Integer

  @doc """
  Acceptance test

  iex>Santa.Day16.go_part_one()
  [1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1]
  """
  def go_part_one() do
    length = 272
    input = my_input_list()
    reversed = reverse(my_input_list())
    punctuators = find_for_given(length, length(input), <<>>)
    calculate_checksum(length, punctuators, input, reversed)
  end
  @doc """
  Acceptance test

  noiex>Santa.Day16.go_part_two()
  [0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1]
  """
  def go_part_two() do
    length = 35651584
    input = my_input_list()
    reversed = reverse(my_input_list())
    punctuators = find_for_given(length, length(input), <<>>)
    calculate_checksum(length, punctuators, input, reversed)
  end

  defp my_input_list() do
    [1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0]
  end

  defp reverse(input) when is_list(input) do
    Enum.reverse(Enum.map(input, fn (1) -> 0
                       (0) -> 1 end))
  end

  defp reverse(input) do
    reverse(input, <<>>)
  end

  defp reverse(<<>>, output) do
    output
  end

  defp reverse(<<1::size(1), rest::bitstring()>>, output) do
    reverse(rest, <<0::size(1), output::bitstring()>>)
  end

  defp reverse(<<0::size(1), rest::bitstring()>>, output) do
    reverse(rest, <<1::size(1), output::bitstring()>>)
  end

  defp find_for_given(total_length, _, result) when total_length <= 0 do
    result
  end

  defp find_for_given(total_length, part_length, previous) do
    find_for_given(total_length - 2 * part_length - 1, part_length,
      <<previous::bitstring(), 0::size(1), reverse(previous)::bitstring()>>)
  end

  defp calculate_checksum(length, punctuators, input, reversed) do
    calculate_checksum(length, punctuators, input, reversed, [], <<>>)
  end

  defp calculate_checksum(length, <<>>, input, _reversed,
    remainder, checksum) do
    {_, partial, _} = partial_checksum(length, remainder ++ input)
    calculate_odd_checksum(checksum ++ partial)
  end

  defp calculate_checksum(length, <<x::size(1), rest::bitstring()>>,
    input, reversed, remainder, checksum) do
    {new_length, partial, new_remainder} = partial_checksum(length,
      remainder ++ input ++ [x])
    case new_length do
      0 ->
        calculate_odd_checksum(<<checksum::bitstring(), partial::bitstring()>>)
      _ ->
        calculate_checksum(new_length, rest, reversed, input,
          new_remainder, <<checksum::bitstring(), partial::bitstring()>>)
    end
  end

  defp partial_checksum(length, input) do
    partial_checksum(length, input, <<>>)
  end

  defp partial_checksum(length, input, output)
  when length == 0 or input == [] do
    {length, output, []}
  end

  defp partial_checksum(length, input, output) when length(input) == 1 do
    {length, output, input}
  end

  defp partial_checksum(length, [a, a | rest], output) do
    partial_checksum(length - 2, rest, <<output::bitstring(), 1::size(1)>>)
  end

  defp partial_checksum(length, [_, _ | rest], output) do
    partial_checksum(length - 2, rest, <<output::bitstring(), 0::size(1)>>)
  end

  defp calculate_odd_checksum(checksum)
  when Integer.is_odd(bit_size(checksum)) do
    to_list([], checksum)
  end

  defp calculate_odd_checksum(checksum) do
    calculate_odd_checksum(checksum, <<>>)
  end

  defp calculate_odd_checksum(<<>>, new_checksum)
  when Integer.is_odd(bit_size(new_checksum)) do
    to_list([], new_checksum)
  end

  defp calculate_odd_checksum(<<>>, new_checksum) do
    calculate_odd_checksum(new_checksum, <<>>)
  end

  defp calculate_odd_checksum(<<a::size(1), a::size(1), rest::bitstring()>>,
    output) do
    calculate_odd_checksum(rest, <<output::bitstring(), 1::size(1)>>)
  end

  defp calculate_odd_checksum(<<_::size(1), _::size(1), rest::bitstring()>>,
    output) do
    calculate_odd_checksum(rest, <<output::bitstring(), 0::size(1)>>)
  end

  defp to_list(list, <<>>) do
    Enum.reverse(list)
  end

  defp to_list(list, <<a::size(1), rest::bitstring()>>) do
    to_list([a | list], rest)
  end
end