--funcao score que recebe int(do score), int(do frame), lista de pontuacoes, retorna uma string - resultado final
--      score frame   l.pont 
score :: Int -> Int -> [Int] -> String

-- será lido nums por vez, p1 = pontuacao1

-- verifica se tem apenas duas jogadas no ultimo frame
score r_score 10 (p1:p2:[]) = show(p1) ++ " " ++ show(p2) ++ " | " ++ show(r_score + p1 + p2)

-- diferentes possiveis pontuacoes do ultimo frame (com tres pontuacoes)
score r_score 10 (10:10:10:[]) = "X X X | " ++ show(r_score + 30) -- as tres jogadas sao 10 cada, somando 30
 
score r_score 10 (10:10:p3:[]) = "X X " ++ show(p3) ++ " | " ++ show(r_score + 20 + p3) -- as duas primeiras jogadas sao strike

score r_score 10 (10:p1:p2:[]) = -- se a primeira jogada for strike
  if (p1 + p2) == 10 then "X " ++ show(p1) ++ " / | " ++ show(r_score + 10 + p1 + p2) -- se forem spare
  else "X " ++ show(p1) ++ " " ++ show(p2) ++ " | " ++ show(r_score + 10 + p1 + p2) -- se nao forem spare
score r_score 10 (p1:p2:p3:[]) = -- caso sem strike, entao é spare
  if (p3) == 10 then show(p1) ++ " / X | " ++ show(r_score + p1 + p2 + p3) -- strike na ultima jogada
  else show(p1) ++ " / " ++ show(p3) ++ " | " ++ show(r_score + p1 + p2 + p3) -- se nao for strike na ultima jogada


-- outras jogadas, normal, strike, spare (sem casos especiais)
score r_score r_frame (p1:p2:p3:resto) =
  if p1 == 10 then "X _ | " ++ score (r_score + p1 + p2 + p3) (r_frame+1) (p2:p3:resto) -- strike(primeira jogada ==10), soma score + bonus de duas prox jogadas
  else if (p1 + p2) == 10 then show (p1) ++ " / | " ++ score (r_score + p1 + p2 + p3) (r_frame+1) (p3:resto) -- spare, soma + bonus da prox jogada
  else show(p1) ++ " " ++ show(p2) ++ " | " ++ score (r_score + p1 + p2) (r_frame+1) (p3:resto) -- jogada normal, sem ser strike ou spare


main :: IO ()
main = do
  -- putStrLn "Digite a quantidade de pinos derrubados em cada jogada: "
  input <- getLine -- leitura da entrada
  -- divide a entrada em substrings
  let game = map read (words input) :: [Int]
  putStrLn(score 0 1 game) -- chamando a funcao com seus parametros