-module(bot_tests).

-include_lib("eunit/include/eunit.hrl").

bot_test_() ->
  {setup,
   fun() ->
       bot_sup:start_link(), ok end,
   fun(ok) ->
       bot_sup:stop() end,
   fun(ok) ->
       [
        {"Making bot",
         fun() ->
             {ok, _Bot} = bot_sup:make_bot({{bot, 99998},
                                            {bot, 99999}, {output, 1}})
         end}
       , {"hand piece to output outputs to service",
          fun() ->
              ok = bot:hand_piece(5, {output, 99997}),
              #{99997 := [5]} = output_bot:get_outputs()
          end}
       , {"hand piece to another bot",
          fun() ->
              bot_sup:make_bot({{bot, 99998}, {bot, 99999}, {output, 99997}}),
              register(bot:make_bot_name({bot, 99999}), self()),
              bot:hand_piece(7, {bot, 99998}),
              bot:hand_piece(8, {bot, 99998}),
              {value, 7} = receive
                     {_GenSrv, Msg} -> Msg
                   after
                     500 -> timeout
                   end,
              {99998, [{7, 8}]} = bot:history({bot, 99998}),
              bot:hand_piece(2, {bot, 99998}),
              bot:hand_piece(1, {bot, 99998}),
              receive _Any -> ok after 500 -> ok end,
              {99998, [{1, 2}, {7, 8}]} = bot:history({bot, 99998})
          end}
       ]
    end}.
