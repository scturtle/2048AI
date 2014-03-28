{-# OPTIONS_GHC -O2 -optc-O2 #-}

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
              squashLine (x:y:xs) n =
                if x==y then (x+y): squashLine xs (n-1)
                        else x: squashLine (y:xs) (n-1)

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

-- how many columns and rows are monotonous
monoCount :: Table -> Int
monoCount table = (sum . map isMono) table + (sum . map isMono . transpose) table
    where isMono l = if and (zipWith (<=) l (tail l)) ||
                        and (zipWith (>=) l (tail l))
                        then 1 else 0

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
bestMove deepth t = let ms = [(v, m) | m <- [LEFT, RIGHT, UP, DOWN]
                             , let t' = move t m
                             , let v = if deepth <= 1 then evaluate t'
                                                      else expectVal deepth t'
                             , emptyCount t' > 0]
                    in  if null ms then Nothing else Just (maximum ms)

expectVal :: Int -> Table -> Double
expectVal deepth t =
        -- if too many empty block, do not look forwards
        if emptyCount t >= 4
            then evaluate t
            -- expection of evaluation of next table
            else sum $ do (p', t') <- ts
                          case bestMove (deepth-1) t' of
                              Just (v, _) -> return (p' * v)
                              Nothing -> return (p' * evaluate t')
    where ts = genNext t

playForever :: Table -> IO Table
playForever t = case bestMove 3 t of -- lookforward X steps
                    Nothing -> return t  -- no more moves, game over
                    Just (_, m) ->
                        let t' = move t m
                            emptyPos = [(i,j) | i <- [0..3] , j <- [0..3]
                                              , t' !! i !! j == 0]
                        in do
                            r1 <- randomRIO (0, length emptyPos - 1)
                            r2 <- randomRIO (1, 100)
                            let -- random select empty position
                                (i,j) = emptyPos !! (r1 :: Int)
                                -- random fill 2 or 4
                                block = if 75 > (r2 :: Int) then 2 else 4
                                l = t' !! i
                                -- new table
                                t'' = take i t' ++
                                        [take j l ++ [block] ++ drop (j+1) l] ++
                                        drop (i+1) t'
                            -- show
                            print m
                            printTable t''
                            -- play more
                            playForever t''

main = do
        -- for submit to hackerrank 2048 contest
        {-table <- replicateM 4 readLnInts
        let Just (_, m)  = bestMove 3 table [> lookforward X steps <]
        print m-}

        playForever (replicate 4 (replicate 4 0))

    where readLnInt = readLn :: IO Int
          readLnInts = liftM (map (read::String->Int) . words) getLine
