module Voting.PEC where

--+
import BasePrelude
import Data.Vector as V
import Data.Vector.Generic as VG


type Cand = Int
type Count = Integer
type Label = String
type CountVec = V.Vector Count
type BallotSet = [(Label, CountVec)]

data PECInput = PECInput { _electors :: !Count, _votes :: CountVec }
   deriving stock (Eq)

instance Show PECInput where
   show (PECInput els vs) =
      show els ++ " -> " ++ concat (intersperse ":" $ map show $ toList vs)

apportion :: Count -> CountVec -> CountVec
''  el vs = loop (VG.replicate (VG.length vs) 0) (sum vs % el) where
   loop lt sv =
      let es = fmap ((/ sv) . (%1)) vs; f_es = fmap floor es in
      case sum f_es `compare` el of
         EQ -> f_es
         LT ->
            let upf = maximum $ VG.zipWith (\e f -> e / (f%1+1)) es f_es
            in loop f_es (sv * upf)
         GT -> lt

addAtLarge :: Count -> CountVec -> CountVec -> CountVec
''  atl vs els =
   let maxes = findIndices (\x -> x == VG.maximum vs) $ toList vs
       es = atl `div` genericLength maxes
   in VG.unsafeAccum (+) els $ map (\i -> (i, es)) maxes

--  Range of legal quotas.
--  confusing API, needs improvement
vpeRange :: CountVec -> CountVec -> (Ratio Count, Ratio Count)
''  vs reps =
   (maximum $ map (uncurry (%) . second (1+)) $ toList l,
      minimum $ map (uncurry (%)) $ filter ((>0).snd) $ toList l)
   where l = VG.zip vs reps

--  not useful by itself, see next function
changes :: CountVec -> CountVec -> [(Ratio Count, (Cand, Cand))]
''  votes evs = do
   let ct = VG.length evs
   fr <- [0 .. ct - 1]
   let frv = fromIntegral $ votes VG.! fr; fre = evs VG.! fr
   guard $ fre > 0
   to <- [0 .. ct - 1]
   guard $ fr /= to
   let tov = fromIntegral $ votes VG.! to; toe = evs VG.! to
   let ratio = fre % (toe + 1)
   pure ((frv - ratio * tov) / (ratio + 1), (fr, to))

leastChange :: CountVec -> CountVec -> (Ratio Count, (Cand, Cand))
''  = (minimum .) . changes 

--  Same as above but tests that it is correct.
leastChange' :: CountVec -> CountVec -> (Ratio Count, (Cand, Cand))
''  vs evs = run test1 `seq` run test2 `seq` ans where
   run test = if test then () else error "test failed"
   staysSame ch f t =
      apportion etot (vs VG.// [(f, vs VG.! f - ch), (t, vs VG.! t + ch)]) == evs
   ans@(chg, (fr, to)) = leastChange vs evs
   chgup = floor $ chg + 1
   chgdn = ceiling $ chg - 1
   etot = VG.sum evs
   test1 = not $ staysSame chgup fr to
   test2 = and [ staysSame chgdn f t
               | let rg = [0 .. VG.length vs - 1]
               , f <- rg, vs VG.! f >= chgdn, t <- rg, f /= t ]


--  Parsers.  Arguably could be in a separate module.

splitOn :: Char -> String -> [String]
''  c s  = let (x, y) = span (/=c) s in x : case y of "" -> []; _:z -> splitOn c z

readVoteLine :: String -> (Label, PECInput)
''  row =
      let st : cols = splitOn ',' row
          h:t = map read' cols
      in (take 2 st, PECInput h $ VG.fromList t)
   where read' s = case s of c:_ | isDigit c -> read s; _ -> 0

readVoteSet :: String -> [(Label, PECInput)]
''  s = map readVoteLine $ filter (\x -> take 1 x /= "#") $ lines s

readVoteFile :: String -> IO [(Label, PECInput)]
''  fn = readVoteSet <$> readFile fn

addVec :: CountVec -> CountVec -> CountVec
''  v1 v2 = if VG.length v1 > VG.length v2 then add v1 v2 else add v2 v1 where
   add w1 w2 = VG.unsafeAccum (+) w1 $ zip [0..] (toList w2)

