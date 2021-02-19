--+
import BasePrelude
import Voting.PEC

atLarges :: Count
''  = 2

--  Pick a reasonably round number in the interval.
roundish :: (Count, Count) -> Count
''  (lo, hi) | hi < lo = hi -- no integer
             | = rto mul
   where
   rto m = (hi `div` m) * m
   pow = last $ takeWhile (\p -> rto p >= lo) $ iterate (*10) 1
   mul | rto (5*pow) >= lo = 5*pow
       | rto (2*pow) >= lo = 2*pow
       |                   = pow

formatNum :: Count -> String
''  ct = reverse $ loop $ reverse $ show ct where
   loop ds = let (a, b) = splitAt 3 ds
             in a <> if null b then "" else ',' : loop b

main = do
	file : _ <- getArgs
	list <- readVoteFile file
   tot <- newIORef mempty
   let chfor w = toEnum $ fromEnum 'A' + w :: Char
   let printrow = mapM_ (printf " %4d")
   do
      printf "   %4c   " 't'
      let (_, PECInput _ v1) : _ = list
      forM_ ([0 .. length v1 - 1]) $ printf " %4c" . chfor
      printf "   %11s   %14c\n" ("v/e" :: String) 'Δ'
	forM_ list \ (st, PECInput ev vs) -> do
      let apports = apportion (ev - atLarges) vs
      let evs = addAtLarge atLarges vs apports
      modifyIORef' tot (addVec evs)
      printf "%s %4d   " st ev
      printrow evs
      let tie = sum evs /= ev
      let vpe = let (a, b) = vpeRange vs apports; a' = floor a
                in if tie then a' else roundish (a' + 1, floor b)
      printf " %13s" $ formatNum vpe
      if tie then printf " %12c *→*" '1'
      else do
         let (chg, (fr, to)) = leastChange vs apports
         printf " %12s %c→%c" (formatNum $ floor $ 1 + chg) (chfor fr) (chfor to)
         case
            let mx = maximum vs
            in filter (\x -> 2*chg - (mx-x)%1 > 0) $ toList vs of
               _:_:_ -> putStr " *"
               _     -> pure ()
      putStrLn ""
   tot <- readIORef tot
	printf "t: %4d   " (sum tot) *> printrow tot *> putStrLn ""

