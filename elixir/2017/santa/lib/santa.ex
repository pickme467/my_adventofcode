defmodule Santa.Day1 do
  @doc """
  Acceptance test
  iex> Santa.Day1.part_one
  1049
  """
  def part_one do
    [first | rest] = input()
    find_sum(0, [first] ++ rest ++ [first])
  end

  @doc """
  Acceptance test
  iex> Santa.Day1.part_two
  1508
  """
  def part_two do
    {l1, l2} = Enum.split(input(), round(length(input())/2))
    find_distant_sum(0, input(), l2 ++ l1)
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

  defp input() do
    '5255443714755555317777152441826784321918285999594221531636242944998363716119294845838579943562543247239969555791772392681567883449837982119239536325341263524415397123824358467891963762948723327774545715851542429832119179139914471523515332247317441719184556891362179267368325486642376685657759623876854958721636574219871249645773738597751429959437466876166273755524873351452951411628479352522367714269718514838933283861425982562854845471512652555633922878128558926123935941858532446378815929573452775348599693982834699757734714187831337546474515678577158721751921562145591166634279699299418269158557557996583881642468274618196335267342897498486869925262896125146867124596587989531495891646681528259624674792728146526849711139146268799436334618974547539561587581268886449291817335232859391493839167111246376493191985145848531829344198536568987996894226585837348372958959535969651573516542581144462536574953764413723147957237298324458181291167587791714172674717898567269547766636143732438694473231473258452166457194797819423528139157452148236943283374193561963393846385622218535952591588353565319432285579711881559343544515461962846879685879431767963975654347569385354482226341261768547328749947163864645168428953445396361398873536434931823635522467754782422557998262858297563862492652464526366171218276176258582444923497181776129436396397333976215976731542182878979389362297155819461685361676414725597335759976285597713332688275241271664658286868697167515329811831234324698345159949135474463624749624626518247831448143876183133814263977611564339865466321244399177464822649611969896344874381978986453566979762911155931362394192663943526834148596342268321563885255765614418141828934971927998994739769141789185165461976425151855846739959338649499379657223196885539386154935586794548365861759354865453211721551776997576289811595654171672259129335243531518228282393326395241242185795828261319215164262237957743232558971289145639852148197184265766291885259847236646615935963759631145338159257538114359781854685695429348428884248972177278361353814766653996675994784195827214295462389532422825696456457332417366426619555'
  end
end

defmodule Santa.Day2 do

  @doc """
  Acceptance test
  iex> Santa.Day2.part_one
  47623
  """
  def part_one() do
    Enum.sum(Enum.map(input(), fn (list) -> Enum.max(list) - Enum.min(list) end))
  end

  @doc """
  Acceptance test
  iex> Santa.Day2.part_two
  312
  """
  def part_two() do
    Enum.sum(Enum.map(input(), fn (list) ->
          find_division(list) end))
  end

  defp find_division(list) do
    [{one, two}] = for x <- list, y <- list, rem(x, y) == 0, x != y do
      {x, y}
    end
    round(Enum.max([one, two]) / Enum.min([one, two]))
  end

  defp input() do
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
    full_sides = round(Float.floor(distance_to_corner / (side_size - 1)))
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
    # initialize with first element that falls under generic case.
    # Special cases excluded that way
    crawl(7, 7, 6, [3, 2], %{1 => 1, 2 => 1, 3 => 2, 4 => 4, 5 => 5, 6 => 10}, expected_sum)
  end

  def crawl(index, sum_squares, last_4_squares, next_squares, dictionary, expected_sum) do
    new_sum = case point_type(index, sum_squares, next_squares) do
                :at -> count_at(index, last_4_squares, dictionary)
                :before -> count_before(index, last_4_squares, dictionary)
                :after -> count_after(index, last_4_squares, dictionary)
                :rest -> count_rest(index, last_4_squares, dictionary)
              end
    case new_sum >= expected_sum do
      true -> new_sum
      false ->
        new_dictionary = put(index, new_sum, dictionary)
        {new_index, new_sum_squares,
         new_last_4, new_next} =
          count_new_squares(index, sum_squares, last_4_squares, next_squares)
        crawl(new_index, new_sum_squares, new_last_4, new_next, new_dictionary, expected_sum)
    end
  end

  def count_new_squares(index, sum, last_4, next)  do
    case point_type(index, sum, next) do
      :at -> {index + 1, get_new_sum(sum, next), last_4 + 2, get_new_next(next)}
      _   -> {index + 1, sum, last_4, next}
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
    :at
  end

  def point_type(index, sum_squares, [_, _]) when index + 1 == sum_squares do
                                                    :before
  end

  def point_type(index, sum_squares, [_, b]) when index + b - 1 == sum_squares do
                                                                   :after
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
