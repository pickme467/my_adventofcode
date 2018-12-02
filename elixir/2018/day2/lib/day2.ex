defmodule Day2 do
  @moduledoc """
  Challenge for Day2 of Advent of Code contest
  """

  @doc """
  iex> Day2.solution_1
  8296
  """
  def solution_1 do
    Input.get_input()
    |> Enum.map(fn x ->
      x
      |> Enum.group_by(fn x -> x end)
      |> Map.values()
      |> Enum.map(&length/1)
      |> Enum.filter(fn x -> x >= 2 end)
      |> Enum.uniq()
    end)
    |> List.flatten()
    |> Enum.group_by(fn x -> x end)
    |> Map.values()
    |> Enum.map(&Enum.count/1)
    |> Enum.reduce(1, fn x, acc -> x * acc end)
  end

  @doc """
  iex> Day2.solution_2
  'pazvmqbftrbeosiecxlghkwud'
  """
  def solution_2 do
    list = Input.get_input()
    find_match(list, list, list, [])
  end

  defp find_match(_, _, _, [matching]) do
    matching
  end

  defp find_match([_current | rest], [], list, matching) do
    find_match(rest, list, list, matching)
  end

  defp find_match([current | rest], [matcher | rest_matchers], list, matching) do
    find_match([current | rest], rest_matchers, list, update_matching(matching, current, matcher))
  end

  defp update_matching(matching, x, y) do
    differ_by = match(x, y, [])

    case length(differ_by) do
      1 -> [find_common(x, y, [])] ++ matching
      _ -> matching
    end
  end

  defp match([], [], diff) do
    diff
  end

  defp match([x | rx], [x | ry], diff) do
    match(rx, ry, diff)
  end

  defp match([x | rx], [_y | ry], diff) do
    match(rx, ry, [x] ++ diff)
  end

  defp find_common([], [], common) do
    Enum.reverse(common)
  end

  defp find_common([x | rx], [x | ry], common) do
    find_common(rx, ry, [x | common])
  end

  defp find_common([_x | rx], [_y | ry], common) do
    find_common(rx, ry, common)
  end
end

defmodule Input do
  def get_input() do
    input()
    |> String.split()
    |> Enum.map(&:binary.bin_to_list/1)
  end

  defp input() do
    "bazvmqthjtrnlosiecxyghkwud
pazvmqbijirzlosiecxyghkwud
pazvtqbmjtrnlosiecxyghkwzd
pazvmqbfjtrjlosnlcxyghkwud
pazvkqbfjtrtlosiecjyghkwud
paztmqbfjtrnbosiecxyglkwud
pazvmqbfjtunlosievxmghkwud
pazvmqbfjtmngosiecyyghkwud
jazvmqbfjtrnlosiecxygikpud
pazvqqbfctrnlosimcxyghkwud
pazvmqbfjtrnwogiecxyihkwud
pazvmqbfjtrqlojiecxeghkwud
payvmqbfjtrzlosiecxyghkwuk
pkzvmqnfjtrnlosieciyghkwud
pazvmqqfjtrnldsiecxyghkwui
pazvmqbfttrqlosiecxywhkwud
gazvmybfjthnlosiecxyghkwud
pazvmqbfjtrnlasiecxygptwud
pktvmqbfjtrnwosiecxyghkwud
pazvmqwfjtrnlosiecxgghkkud
pazvmzkbjtrnlosiecxyghkwud
pazvmqbfjtrnloslecxyghuwui
pezvmqbfjtrnlesieyxyghkwud
cazvmqbfjrrnlosiecxyghkmud
pazvmqrfjjrnlosiecxyghkwnd
pazvmqbgjtrnlosiecxyphtwud
pazvmqbvmtrnlosiecxyghkpud
pazdmqbfjtrnlosiecxyuhkpud
pazvmqbflnrnloshecxyghkwud
pazvvqbfjprilosiecxyghkwud
pazvwqbfjtrllosiecxyghknud
pazvmqbfjtrnloniecxdghkaud
pazvmqbfjtrnlvsuecxynhkwud
ptzvmqwfjtrnlosieccyghkwud
pkzvmqbjjtrnlosiecryghkwud
pazvmqqfjtrexosiecxyghkwud
pazgmqbfjtrneoyiecxyghkwud
paznmqbfjtrnlosiecxydhkwzd
pazvmqbfjtrnaosiwcxsghkwud
pazomqbfjxrnlosiewxyghkwud
pazsmqbfjprnlosiecxrghkwud
pazvmqbfqtrnoosiecxygmkwud
aazvmqbfjtrnlosiacxyghjwud
pazviqbfjtrnlobiecxygrkwud
qazwmqbfjhrnlosiecxyghkwud
pazvmqbfftrnlosiqcxygfkwud
patvmqbfjtonlosircxyghkwud
pazvmqbfjtrnlomaecxyghkpud
paztmqbfjtrulossecxyghkwud
pazvmqbijtrnlobiecxyghkwkd
pazvsqbfjtrnlospecxyghkqud
pbzmmqbfjtrnlosiecxyghkwhd
pezvkqbfjtenlosiecxyghkwud
razvmqbfjkrnlosiecxeghkwud
pazcmqbfjtrnloriecxyghkgud
pazvmqbfftfnlosiecvyghkwud
pazvmqpujtrnlosiepxyghkwud
patvgqbfjtrnloslecxyghkwud
pazvmqbfltrnlosibcxyghswud
pazvmebfjtrnlosaecxyehkwud
pazdmqbejtrnlosiecxyghrwud
pazvmcbfntrplosiecxyghkwud
pszvmqbfjtrnlosivcfyghkwud
puzvmqbfjtrnloeiecxyxhkwud
pazvmqbfjtrivooiecxyghkwud
pazvyqbfjtrngosiwcxyghkwud
pauvmqbfjtrnlosimexyghkwud
pazvmqbfjtrnwoshecxeghkwud
dazvmqbfjtrnloshecxygxkwud
pazvmqbfjtrtdosiecxyghvwud
pazxmqbfjtrnlosieceyghjwud
pazvmqbfjtrnlosihexjghkwud
pazvmqbfjsrnlosiecxughiwud
phzvcqbfjtrqlosiecxyghkwud
pazvmibfjtrnlosjecxxghkwud
pazvmqbfjtrbeosiecxlghkwud
pazvmqyfjttolosiecxyghkwud
fawvmqbfjtrnlosiecxyghkwhd
pazvmqbfjprnxosiecxyghkbud
macvmqbfjtrnlosiesxyghkwud
pazsmqbfjtrflouiecxyghkwud
pacvmqbfjtrnltsiecxyghcwud
pazvmqbfjtymlosiecxygykwud
pazvmqbfjtrclosiecxygukwmd
pazvmqbfjtrnlobiecxphhkwud
mazvmqbhitrnlosiecxyghkwud
pazvmqdtjtrnlrsiecxyghkwud
pazvmqbfjgrnllsieczyghkwud
pazvmqbfjtrilosiecxxgikwud
pazvmqbjjtrnlosreceyghkwud
paxvmmbfjtrilosiecxyghkwud
pazqmwbfjtrnlowiecxyghkwud
pazvmqbfjfrnqosiecxyghkwui
pazvmqbfjtrrgosiecxyghswud
pazvmqnfjtrnlosiecsyghkwmd
paiemqbmjtrnlosiecxyghkwud
pazvmqbfdtqnlosiecxyjhkwud
pazvmxbfjthndosiecxyghkwud
pqzvmqbfjtrnlosiecxbghkzud
pagrmqbfjtrnlosiecxygskwud
pazamqtfjtrnsosiecxyghkwud
pazvmqbfjtrnldshecxyzhkwud
pazvmnbfjtrllosieclyghkwud
snzvmqbfjnrnlosiecxyghkwud
pazvsqbfjdrnlosiecxyghswud
pazvmqnfjfrnlosiecsyghkwud
pazvmqbfjtrnlosiecxjghowum
pazvmqbfjtjnlosieczygfkwud
pazvmqbsjtrnloziecxyghkeud
pazvxqbgjtrnlooiecxyghkwud
pazvmqbfjtrnlooiecxmyhkwud
pazvmqbyftrnlosgecxyghkwud
pazvmqbfjtrnlosiwcxyqhksud
pazvmqkyjtrnlokiecxyghkwud
pazfmqbfjtrnlosijcxyohkwud
pazvmqbfjtrnlociecxygikcud
fazvmqbfjtrnlosiecxyjhkuud
pazvmqbojtknlohiecxyghkwud
pazvmqbfjtgnlosbecxyghkwux
pazvmqbfjtrnlocieckoghkwud
pazvdqbfjtrlltsiecxyghkwud
pazvmqbfjtsnlfsiecxyglkwud
przvpqbfjtrnyosiecxyghkwud
pazvmbrfjtrnlosiecxmghkwud
dazvmqbfttrnlostecxyghkwud
pazvmqbfttdnlosiecxygwkwud
pazvmqbvitrnlosieexyghkwud
pazvmqbfjhrnlosjecxyvhkwud
pazvmqbfstrnlosiecxyggkwpd
bazvmqbfjtrnlmsiecxyohkwud
patmmqbfjtrnlosizcxyghkwud
pazvmqbfwtrglosieqxyghkwud
pazvmqbfjtrnlosiecxdhhkwmd
pazvmqbfjdrnlosnexxyghkwud
oazrrqbfjtrnlosiecxyghkwud
pazvmqbfjcrnlosiecxygakwjd
pazvmqbfjtrnlosifcxfghkwyd
pazvmnbfjtrnlosiecxyahzwud
pazvmqbfgtrnlojiecxyghkgud
pazvmqbfjtrnlaliecxyghkwuy
pazvmqbfjtrnlfsiecrtghkwud
pazvmqbkjtrnloswecxdghkwud
pazvtqbfjtdnlosiecxyghkwuu
pozvmqbfrtrnlosiesxyghkwud
payvmqbfjornlossecxyghkwud
pazvuqbfjtrnlosiscxyghkpud
pgzcmqbfjtrnlotiecxyghkwud
pazvvqbfjtrnlobieyxyghkwud
pazycqbfjtrnlosiecxyzhkwud
pizvdqbfjtrnlosiecxbghkwud
pazvmqbfjtrnloqiecxmgtkwud
gazvmqbfjtrnlusiecxpghkwud
pazvmqdfjtralosiecxyghkwmd
pazvmqbfjtmnlosiecxywhawud
pazvlqbfjtrnlosqecxyghqwud
pazvmqbfjtrnlhsneixyghkwud
kazvmqbfjtrqlosimcxyghkwud
pazvmwbfjtrclosiecxyghkuud
pazvmqjfjtrnlosieckyghpwud
pezvmqbgjtrnloseecxyghkwud
pazvqqbfjtfnlosvecxyghkwud
oazvmqbfjtunlosiecxyghkwad
pazvmqbfjtrncoswecxyghfwud
pazvyqbfjtrnlosqecxygtkwud
pazvmqbfjtrvlzsiecxygwkwud
pazvmqbfjjrnlosiekxylhkwud
madvmqbfjtrnlosircxyghkwud
pazvmybfjtrnlisiecxyghkwbd
pazvmqbjjixnlosiecxyghkwud
pazvmqefjtrnloqiecxyghhwud
pazveqbfjtrnlosiecgygzkwud
pazvmqbfjtrxlosiecxmgwkwud
uazvmqufjtrnlosiecxyghkwuo
pasymqbfjtrnlosiecxyghowud
pazvmqbfjtlnlpsiecxyghswud
pnzvmqbfjprnloszecxyghkwud
pafjmqcfjtrnlosiecxyghkwud
pazvmqxfbtrnloqiecxyghkwud
pazvmzbfjtrnposiccxyghkwud
pazvmqbfjotulosiecxyghkwud
pazvmqbfotrnlosgecxykhkwud
przvmqbfjtrnlosiecxyqhkwcd
pazvmqbfjtsnlogiecxyyhkwud
pazvmqbfrtrnlzsiecxyghkwug
pazvmqbfjtrnlosiecxzgukwuo
pqzvmqbqjtrnlosdecxyghkwud
pazvmqbfjtqqlosiecxughkwud
pazvmqbfjtrnlosiedhyphkwud
pazsmqbcutrnlosiecxyghkwud
pazvmqbgrtrnlosiecxyghpwud
pazemqbfjtznlosiecxyghkvud
pazvkqbfjtrilosiecxyghkwod
pfzvmqbfjtrnlopiecxygjkwud
pazvmqvfjtreloniecxyghkwud
pazvmqbfjernljsiecxgghkwud
pazvmqikjtrnlosiecxyghqwud
pazvmqbfjtrnpesoecxyghkwud
fazvmqbfjtrnlosihchyghkwud
pazvmqbfjtgnloanecxyghkwud
pazvmqsfjqrnlosiecxychkwud
parvmqbfjtrnlosiecxygfuwud
przvmqbfjtrhlosihcxyghkwud
pazvmqbcjtrnlosimcxgghkwud
pazvmqbfjtrnlosceciyjhkwud
pazvkqbfjtrylosivcxyghkwud
pazvmqbfjtrnlgsieoxyghdwud
pazvmqnfstrnlowiecxyghkwud
pazvmqbfdtrnlosieumyghkwud
pazvmqbfjtrnlosyecxfghkwul
pazvmqbfjtrclosivcxyghkcud
pazjmqbfjtrnlosiecxygokwkd
hazvmqbfjtrflosiecxzghkwud
wazvmqbfjtrnlomiecxyphkwud
yazvmqbfjirnkosiecxyghkwud
pczvmqbfjtrnlohiecxyghkwpd
pazvmqbfotrbeosiecxlghkwud
pazvmqbfjtrplosiecxynhzwud
paxvbqbwjtrnlosiecxyghkwud
pazvmqvfjtrnlosiecbyghqwud
pazjmqbfjtrnlosiecxoghkwed
pazvmqbfjtreljsitcxyghkwud
mazamqbfjtrnlosiecxoghkwud
pazvmqbfjjrnposiscxyghkwud
pbrvmqbfjtrnloliecxyghkwud
pazvmqbfjtrnlosiecxgghkyyd
pmzvmqbfntrnlosiecxyghkwuw
pazvzqbfjtrnlosienxyghzwud
pazvmqifjtvnlosrecxyghkwud
tazvmqbhjtjnlosiecxyghkwud
pazvmqbfjtlnxosiecxyghkwuo
pazvmqbfjennlosiecxyghkwxd
pahvmqbfjhrnlosiecxythkwud
pazvmlkfjtrnlxsiecxyghkwud
pfzvmqbojtrnlosieciyghkwud
pazvbqbfjtrollsiecxyghkwud
eazvmqbfjtrnlosiecayghkoud
pazvmqbfjtjnlvsiecxyghkwsd
pazvoqbojtrnlosiecfyghkwud
pazvmqbfjtuslosiecxyghksud
pazvmqbfjnrnlosiedxyghkwup
pazvmqbjjtrnlosieaxyghdwud
pazccqbfjtrhlosiecxyghkwud
pbzvmqkfjtrnlosievxyghkwud
pazvmqrljtrnlosiscxyghkwud
pazvmqbfjfoqlosiecxyghkwud
pazcmqbfjtrnlosiecxyihkwuf
pszvmqbfjtrnnosiacxyghkwud
aazvmqbfjtrnlosieyxyghkwld
pazvrqbfntrnlosiycxyghkwud
pkzvoqbfjtrnlosiecxyghxwud"
  end
end
