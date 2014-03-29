{-# OPTIONS_GHC -O2 -optc-O3 -fno-cse #-}

import Data.List
import Control.Monad
import Data.Ord (comparing)
import System.Random (randomRIO)
import Debug.Trace

data Move = LEFT | RIGHT | UP | DOWN deriving (Eq, Show, Ord, Enum)
type Table = [[Int]]

printTable :: Table -> IO ()
printTable = putStrLn . (++"\n") 
           . intercalate "\n" 
           . map (intercalate "\t" . map show)

-- LEFT
squashLeft :: Table -> Table
squashLeft table = [squashLine l 4 | l <- table]
        where squashLine [] n = replicate n 0
              squashLine (0:xs) n = squashLine xs n
              squashLine [x] n = x: replicate (n-1) 0
              squashLine (x:y:xs) n
                | x== y     = (x+y): squashLine xs (n-1)
                | otherwise = x: squashLine (y:xs) (n-1)

move :: Table -> Move -> Table
move table m = case m of
                 LEFT  -> squashLeft table
                 RIGHT -> let t' = map reverse table
                              t'' = squashLeft t'
                          in  map reverse t''
                 UP    -> let t' = transpose table
                              t'' = squashLeft t'
                          in  transpose t''
                 DOWN  -> let t' = (map reverse . transpose) table
                              t'' = squashLeft t'
                          in  (transpose . map reverse) t''

genNext ::  Table -> [(Double, Table)]
genNext table = concat $ do
        i <- [0..3]
        j <- [0..3]
        guard (table !! i !! j == 0)
        let l = table !! i
        return [(prob * 0.75, take i table ++
                              [take j l ++ [2] ++ drop (j+1) l] ++
                              drop (i+1) table),
                (prob * 0.25, take i table ++
                              [take j l ++ [4] ++ drop (j+1) l] ++
                              drop (i+1) table)]
    where prob = 1 / fromIntegral (emptyCount table)

emptyCount :: Table -> Int
emptyCount = sum . map (length . filter (==0))

twoBase :: Int -> Int
twoBase pow = let Just i = lookup pow table in i
    where table = (0, 0): [(2^i, i) | i <- [0..20]]

-- how many columns and rows are monotonous
monoCount :: Table -> Int
monoCount table = sum (map isMono table) + sum (map isMono (transpose table))
    where isMono l = if and [a <= b | (a,b) <- pairs] || and [a >= b | (a,b) <- pairs]
                        then (twoBase . maximum $ l) else 0
            where pairs = zip l (tail l)


totalValue :: Table -> Double
totalValue table = sum [val i | l <- table, i <- l]
        where -- empty block
              val 0 = 30
              -- block with number
              val n = fromIntegral n ** 1.2

-- evaluation function
evaluate :: Table -> Double
evaluate t = (fromIntegral . monoCount $ t) * 30.0
           + totalValue t * 1.0

bestMove :: Int -> Table -> Maybe (Double, Move)
bestMove deepth t = let -- possible moves and corresponding table
                        mts = [(t', m) | m <- [LEFT .. DOWN]
                                       , let t' = move t m
                                       , t' /= t]
                    in case length mts of
                           0 -> Nothing
                           1 -> let (t', m) = head mts  -- only one, don't lookforward
                                in  Just (evaluate t', m)
                           _ -> let posib = [(expectVal deepth t', m) | (t', m) <- mts]
                                in  Just (maximumBy (comparing fst) posib)

estiDeepth :: Table -> Int
estiDeepth t 
        | ec >= 12 = 1
        {-| ec >= 8 = 1-}
        | ec >= 4 = 2
        {-| ec >= 2 = 3-}
        | otherwise = 3
    where ec = emptyCount t

expectVal :: Int -> Table -> Double
expectVal 0 t = evaluate t
expectVal deepth t =
        -- if too many empty block, do not look forwards
        let d = min (deepth-1) (estiDeepth t)
            -- expection of evaluation of next table
        in  sum $ do (p', t') <- ts
                     case bestMove d t' of
                         Just (v, _) -> return (p' * v)
                         Nothing -> return (p' * evaluate t')
    where ts = genNext t

playForever :: Table -> IO Table
playForever t =
        let emptyPos = [(i,j) | i <- [0..3] , j <- [0..3] , t !! i !! j == 0]
        in  if null emptyPos
                then do  -- game over
                    putStrLn "No empty blocks. Game over."
                    return t
                else do
                    r1 <- randomRIO (0, length emptyPos - 1)
                    r2 <- randomRIO (1, 100)
                    let -- random select empty position
                        (i,j) = emptyPos !! (r1 :: Int)
                        -- random fill 2 or 4
                        block = if 75 > (r2 :: Int) then 2 else 4
                        l = t !! i
                        -- new table
                        t' = take i t ++
                                [take j l ++ [block] ++ drop (j+1) l] ++
                                drop (i+1) t
                    putStrLn $ "New " ++ show block ++ " at " ++ show (i+1,j+1)
                    printTable t'
                    case bestMove (estiDeepth t') t' of  -- lookforward
                        Nothing -> do  -- no more moves, game over
                                    putStrLn "No moves. Game over."
                                    return t'
                        Just (_, m) -> do
                                    print m
                                    let t'' = move t' m
                                    printTable t''
                                    playForever t''

main = do
        -- for submitting to hackerrank 2048 contest
        {-table <- replicateM 4 readLnInts-}
        {-let Just (_, m)  = bestMove (estiDeepth table) table [> lookforward X steps <]-}
        {-print m-}

        playForever (replicate 4 (replicate 4 0))

    where readLnInts = liftM (map (read::String->Int) . words) getLine
