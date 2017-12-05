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
    crawl(7, 7, 6, [3, 2], %{1 => 1, 2 => 1, 3 => 2, 4 => 4, 5 => 5, 6 => 10}, expected_sum)
  end

  def crawl(index, sum_squares, last_4_squares, next_squares, dictionary, expected_sum) do
    new_sum = case point_type(index, sum_squares, next_squares) do
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
        crawl(new_index, new_sum_squares, new_last_4, new_next, new_dictionary, expected_sum)
    end
  end

  def update_data(index, sum, last_4, next)  do
    case point_type(index, sum, next) do
      :at_corner -> {index + 1, get_new_sum(sum, next), last_4 + 2, get_new_next(next)}
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
                    words = String.split(line, " ")
                    has_unique_words(words)
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
      Enum.sort(String.to_charlist(word)) end
  end
end

defmodule Santa.Day5 do

  @doc """
  iex> Santa.Day5.part_one
  396086
  """
  def part_one() do
    walk(make_map(String.split(Santa.Day5.Input.input(), "\n"), 0, %{}), 0, 0,
    fn (value) -> value + 1 end)
  end

  @doc """
  iex> Santa.Day5.part_two
  28675390
  """
  def part_two() do
    walk(make_map(String.split(Santa.Day5.Input.input(), "\n"), 0, %{}), 0, 0,
      fn (value) when value >= 3 -> value - 1
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
        walk(Map.replace!(map, start, next_evaluator.(element)),
          start + element, step + 1, next_evaluator)
    end
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
