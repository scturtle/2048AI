{-# OPTIONS_GHC -O2 -optc-O3 #-}

import Data.List
import Control.Monad
import Data.Ord (comparing)
import System.Random (randomRIO)
import qualified Data.Map.Strict as M
import Control.Monad.State
import Debug.Trace
import Text.Printf (printf)
import Data.Time

data Move = LEFT | RIGHT | UP | DOWN deriving (Eq, Show, Ord, Enum, Bounded)
type Table = [[Int]]

probOfFour = 0.25

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
        return [(prob * p2, take i table ++
                            [take j l ++ [2] ++ drop (j+1) l] ++
                            drop (i+1) table),
                (prob * p4, take i table ++
                            [take j l ++ [4] ++ drop (j+1) l] ++
                            drop (i+1) table)]
    where prob = 1 / fromIntegral (emptyCount table)
          p4 = probOfFour
          p2 = 1 - probOfFour

emptyCount :: Table -> Int
emptyCount = sum . map (length . filter (==0))

-- from Data.List.Ordered
isSortedBy :: (Int -> Int -> Bool) -> [Int] -> Bool
isSortedBy lte [a,b,c,d] = lte a b && lte b c && lte c d

-- how many columns and rows are monotonous
monoCount :: Table -> Int
monoCount table = sum (map isMono (table ++ transpose table))
    where isMono l = if isSortedBy (<=) l || isSortedBy (>=) l
                        then ((baseTwoTable M.!) . maximum $ l)
                        else 0
            where baseTwoTable = M.fromList $ (0,0): [(2^i, i) | i <- [0..15]]

totalValue :: Table -> Double
totalValue t = sum [val i | l <- t, i <- l]
        where -- empty block
              val 0 = 30
              -- block with number
              val n = table M.! n
              table = M.fromList [(2^i, (fromIntegral (2^i :: Int)) ** 1.2)
                                 | i <- [0..15 :: Int]]

-- evaluation function
evaluate :: Table -> Double
evaluate t = totalValue t * 1.0
           + (fromIntegral . monoCount $ t) * 30.0

bestMove :: Int -> Table -> State Double (Maybe (Double, Move))
bestMove depth t = do
        {-bd <- get-}
        {-case (trace (printf "%d %f" depth bd) length mts) of-}
        case length mts of
                0 -> return Nothing  -- no more moves, game over
                1 -> do              -- only one move, take it, no more search
                    let (t', m) = head mts
                        v = evaluate t'
                    get >>= put . (max v)  -- update bound
                    return $ Just (v, m)
                _ -> do
                    vs <- sequence $ [liftM (flip (,) m) $ boundedEval t' | (t', m) <- mts]
                    return $ Just (maximumBy (comparing fst) vs)

    where mts = [(t', m) | m <- [minBound ..] , let t' = move t m , t' /= t]  -- possible moves
          boundedEval t' = do
              bd <- get
              let v' = evaluate t'
              put (max v' bd)  -- update bound
              if depth <= 0  -- reach depth limit
                  then return v'
                  else if v' < bd * (0.9 - fromIntegral depth * 0.1)  -- adoptable pruning threshold
                           {-then (trace (printf "pruning %d %f %f" depth bd v') v')-}
                           then return v'
                           else do let d = min depth (estiDepth t')
                                   v'' <- expectVal d t'
                                   get >>= put . (max v'')  -- update bound
                                   return v''

expectVal :: Int -> Table -> State Double Double
expectVal depth t = do
        res <- sequence [liftM ((,) p) $ bestMove (depth - 1) t'  -- [(p, Maybe (v, m))]
                        | (p, t') <- genNext t]
        return . sum . map (\(p, r) -> maybe 0 ((*p) . fst) r) $ res  -- sum { p * v }

estiDepth :: Table -> Int
estiDepth t
        | ec >= 12 = 1
        {-| ec >= 8 = 2-}
        | ec >= 3 = 2
        | ec >= 2 = 3
        | ec >= 1 = 4
        | otherwise = 5
    where ec = emptyCount t

playForever :: Table -> IO Table
playForever t =
        let emptyPos = [(i,j) | i <- [0..3] , j <- [0..3] , t !! i !! j == 0]
        in  if null emptyPos
                then do  -- game over
                    putStrLn "No empty blocks. Game over."
                    return t
                else do
                    r1 <- randomRIO (0, length emptyPos - 1)
                    r2 <- randomRIO (0, 1) :: IO Double
                    let -- random select empty position
                        (i,j) = emptyPos !! r1
                        -- random fill 2 or 4
                        block = if probOfFour < r2 then 2 else 4
                        l = t !! i
                        -- new table
                        t' = take i t ++
                                [take j l ++ [block] ++ drop (j+1) l] ++
                                drop (i+1) t
                    putStrLn $ "New " ++ show block ++ " at " ++ show (i+1,j+1)
                    printTable t'
                    {-putStrLn "Enter"; getLine  -- pause-}
                    start <- getCurrentTime
                    case (evalState (bestMove (estiDepth t') t') 0) of  -- lookforward
                        Nothing -> do  -- no more moves, game over
                                    putStrLn "No moves. Game over."
                                    return t'
                        Just (_, m) -> do
                                        print m
                                        stop <- getCurrentTime
                                        print $ diffUTCTime stop start
                                        let t'' = move t' m
                                        printTable t''
                                        playForever t''

main = do
        -- for submitting to hackerrank 2048 contest
        {-table <- replicateM 4 readLnInts
        let Just (_, m)  = (evalState (bestMove (estiDepth table) table) 0)  [> lookforward X steps <]
        print m-}

        playForever (replicate 4 (replicate 4 0))

    where readLnInts = liftM (map (read::String->Int) . words) getLine
