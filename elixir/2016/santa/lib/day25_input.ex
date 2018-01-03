defmodule Santa.Day25.Input do
  @doc false
  def input() do
    "cpy a d\n" <> #
"cpy 15 c\n" <> #
"cpy 170 b\n" <> #   <----+ This loop multipliest 170 by 15
"inc d\n" <> #       <--+ | and adds register a putting all
"dec b\n" <> #          | | in register d
"jnz b -2\n" <> #    >--+ |
"dec c\n" <> #            |
"jnz c -5\n" <> #    >----+
"cpy d a\n" <> #     <-----------------------+ to generate 0101 infinitely
"jnz 0 0\n" <> #     <--------------------+  | value of d shall be just
"cpy a b\n" <> #                          |  | ...010101010, so a is smallest
"cpy 0 a\n" <> #                          |  | number giving ..10101010 when
"cpy 2 c\n" <> #     <---------------+    |  | added to 170 * 15
"jnz b 2\n" <> #     >--+  <--+      |    |  |
"jnz 1 6\n" <> #     >--|-----|---+  |    |  |
"dec b\n" <> #       <--+     |   |  |    |  |
"dec c\n" <> #                |   |  |    |  |
"jnz c -4\n" <> #    >--------+   |  |    |  | This inner loop calculates
"inc a\n" <> #                    |  |    |  | integer value of a / 2
"jnz 1 -7\n" <> #    >------------|--+    |  |
"cpy 2 b\n" <> #     <------------+       |  |
"jnz c 2\n" <> #     >-----+ <-+          |  |
"jnz 1 4\n" <> #     >--+  |   |          |  | This loop calculates
"dec b\n" <> #       <--|--+   |          |  | a mod 2
"dec c\n" <> #          |      |          |  |
"jnz 1 -4\n" <> #    >--|------+          |  |
"jnz 0 0\n" <> #     <--+                 |  |
"out b\n" <> #                            |  | b is just value of a mod 2
"jnz a -19\n" <> #    >-------------------+  |
"jnz 1 -21"      #    >----------------------+
  end
end
