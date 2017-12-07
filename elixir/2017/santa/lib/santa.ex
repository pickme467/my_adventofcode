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
    distribute(next_index(index, data), value - 1, Map.put(data, index, data[index] + 1))
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
      {right_key, right_values}
    end)
    |> Enum.reduce({[], []}, fn ({key, values}, {all_keys, all_values}) ->
      {[key] ++ all_keys, values ++ all_values} end)
    Enum.find(all_keys, fn (key) -> not( key in all_values) end)
  end

  @doc """
  iex> Santa.Day7.part_two()
  :not_implemented
  """
  def part_two() do
    lines = Santa.Day7.Input.input()
    |> String.split("\n")
    {leaves, branches} = split_leaves_from_branches(lines, {[], []})
    new_input = incorporate_leaves_to_branches(leaves, branches)
    check_and_reduce(new_input)
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
        {leaves, [ {keyword, weight, value_list} | branches]}
        false -> {[extract_keyword_weight(line) | leaves], branches}
      end)
  end

  defp extract_keyword_weight(key) do
    [keyword, "(" <> weight] = String.split(key, " ")
    {keyword, String.to_integer(String.trim_trailing(weight, ")"))}
  end

  defp incorporate_leaves_to_branches(leaves, branches) do
    {leaves, branches}
    Keyword.keys(leaves)
    |> Enum.reduce(branches, fn (key, dictionary) ->
      Enum.map(dictionary, fn ({branch, weight, list}) ->
        {branch, weight, Enum.map(list, fn (^key) ->
            List.keyfind(leaves, key, 0)
            (other) -> other end)} end) end)
  end

  defp check_and_reduce([{name, weight, list} | rest]) do
    case all_filled(list) do
      true ->
        case check(list) do
          :valid -> reduce(weight, list)
          :invalid -> list
        end
      false -> list
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
