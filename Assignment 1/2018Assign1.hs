-- CS205 Assignment 1
-- ample solutions, typically several solutions are possible


-- Question 1: average

import Test.QuickCheck

average :: Float -> Float -> Float -> Float
average x y z = (x + y + z) / 3

howManyAboveAverage1 :: Float -> Float -> Float -> Int
howManyAboveAverage1 x y z = f x + f y + f z
     where av = average x y z
           f x = if (x > av) then 1 else 0
                                  
howManyAboveAverage2 :: Float -> Float -> Float -> Int
howManyAboveAverage2 x y z = if ((x > av && y > av) ||
                                 (x > av && z > av) ||
                                 (y > av && z > av))
                            then 2
                            else if x == y && y == z then 0 else 1
                   where av = average x y z
                        
checkcorrectness x y z =  
     howManyAboveAverage1 x y z  == howManyAboveAverage2 x y z


-- *Main> howManyBelowAverage 123456 222222 654321
-- 1
-- quickCheck checkcorrectness
-- OK, passed 100 tests

-- Note: in this solution we compared two different solutions, to increase the confidence in our implementation, however quickcheck could also be used to compare against the specification. 



alfredo :: Int -> Int -> Float
alfredo numOfTop diameter = overall * 1.5
                where radius = (fromIntegral diameter)/2.0
                      area = pi*radius*radius
                      base = 0.001*area
                      topCost = 0.002 * area * (fromIntegral numOfTop)
                      overall = topCost + base

-- note it would even be better to avoid the hardcoded numbers.
-- Pizza Famiglia is approx twice as expensive than Pizza Bambini:
-- alfredo 2 32 
-- 6.03
-- alfredo 6 16
-- 3.92

comparePizzas = alfredo 6 14  > alfredo 2 32 
-- False

-- Question 3.

divides :: Integer -> Integer -> Bool
divides x y = y `mod` x == 0

prime :: Integer -> Bool
prime n = n > 1 &&  and [not(divides x n) | x <- [2..(n-1)]]

allprimes :: [Integer]
allprimes = [x | x<- [2..], prime x]

allprimesBetween :: Integer -> Integer -> [Integer]
allprimesBetween x y = [p | p<- [x,y], prime p]

primeTest :: [Bool]
primeTest = map prime [1..]

-- alternatively
-- primeTest = [prime x | x <- [1..]]

primeTest2 :: [(Integer, Bool)]
primeTest2 = zip [1..] primeTest

{-
take 100 primeTest2
[(1,False),(2,True),(3,True),(4,False),(5,True),(6,False),(7,True),(8,False),(9,False),(10,False),(11,True),(12,False),(13,True),(14,False),(15,False),(16,False),(17,True),(18,False),(19,True),(20,False),(21,False),(22,False),(23,True),(24,False),(25,False),(26,False),(27,False),(28,False),(29,True),(30,False),(31,True),(32,False),(33,False),(34,False),(35,False),(36,False),(37,True),(38,False),(39,False),(40,False),(41,True),(42,False),(43,True),(44,False),(45,False),(46,False),(47,True),(48,False),(49,False),(50,False),(51,False),(52,False),(53,True),(54,False),(55,False),(56,False),(57,False),(58,False),(59,True),(60,False),(61,True),(62,False),(63,False),(64,False),(65,False),(66,False),(67,True),(68,False),(69,False),(70,False),(71,True),(72,False),(73,True),(74,False),(75,False),(76,False),(77,False),(78,False),(79,True),(80,False),(81,False),(82,False),(83,True),(84,False),(85,False),(86,False),(87,False),(88,False),(89,True),(90,False),(91,False),(92,False),(93,False),(94,False),(95,False),(96,False),(97,True),(98,False),(99,False),(100,False)]
-}

primeTwins :: Int -> Int 
primeTwins n =  length [(x,x-2) | x <- (take n allprimes), prime(x-2)]
-- note this solution is short, but not very efficient.

-- run: primeTwins 2000
-- 302

-- do you want to see the primetwins?
-- then call eg: (primeTwins2 4) to see the primeTwins amongst the first four
-- prime numbers
-- primeTwins2 :: Int -> [(Integer,Integer)] 
-- primeTwins2 n =  [(x,x-2) | x <- (take n allprimes), prime(x-2)]



-- Question 4
expmod :: Integer -> Integer -> Integer -> Integer
expmod m e n | e == 1    = m`mod` n
             | otherwise = ((expmod m (e-1) n)*m)`mod`n
             
expmod2 :: Integer -> Integer -> Integer -> Integer
expmod2 m e n | e == 1          = m`mod` n
              | even e          = ((expmod2 m (e`div`2) n)^2)`mod`n
              | otherwise       = ((expmod2 m (e-1) n)*m)`mod`n


-- *Main> expmod 5000 654321 14
-- 8
-- *Main> expmod2 5000 654321 14
-- 8 -- much faster 


-- example grid for question 5
grid = [[True,True,True],[False,True,True],[True,True,True]]

-- Question 5
-- a)
valid :: [[Bool]] -> Bool
valid grid = length grid >= 3 && max == min && max >= 3
  where max = maximum (map length grid)
        min = minimum (map length grid) 

-- b)
showGrid :: [[Bool]] -> String
showGrid grid = inter "\n" $ map (map ch) grid
  where ch True = 'O'
        ch False = '.'
        inter _ [] = []
        inter sep (x:xs) = x ++ foldl (\x rest -> x ++ sep ++ rest) [] xs 

printGrid :: [[Bool]] -> IO ()
printGrid g = putStrLn $ showGrid g


-- c)
readGrid :: String -> [[Bool]]
readGrid = map (map cell) . lines . strip
  where cell 'O' = True
        cell '.' = False
        strip s = dropWhile isWhiteSpace (dropWhileEnd isWhiteSpace s)
        isWhiteSpace c = c `elem` " \n\r\t"
        dropWhileEnd pred = reverse . dropWhile pred . reverse

-- d)
neighbours :: [Bool] -> Int
neighbours nb = length (filter id nb)

newCell :: Int -> Bool -> Bool
newCell count c | c && (count < 2 || count > 3) = False
                | not c && count == 3 = True
                | otherwise = c


-- This step function is very explicit - it proceeds by _pattern-matching_ on
-- 3x3 regions of the grid and deals with the edges by adding a dummy row and 
-- column. Many student submissions had more succint, and often elegant, 
-- solutions.
step :: [[Bool]] -> [[Bool]]
step gd | not $ valid gd = error "Invalid grid"
        | otherwise = step' (extendGridLeft (extendGridTop gd))
  where step' :: [[Bool]] -> [[Bool]]
        step' (top : gd@(cur : bot : _)) = row top cur bot : step' gd
        step' (top : cur : []) = [row top cur []]

        row :: [Bool] -> [Bool] -> [Bool] -> [Bool]
        row (tl : t@(tc : tr : _)) (cl : c@(cc : cr : _)) (bl : b@(bc : br : _)) =
          newCell (neighbours [tl, tc, tr, cl, cr, bl, bc, br]) cc : row t c b
        row (tl : t@(tc : tr : _)) (cl : c@(cc : cr : _)) [] =
          newCell (neighbours [tl, tc, tr, cl, cr]) cc : row t c []
        row (tl : tc : []) (cl : cc : []) (bl : bc : []) =
          [newCell (neighbours [tl, tc, cl, bl, bc]) cc]
        row (tl : tc : []) (cl : cc : []) [] =
          [newCell (neighbours [tl, tc, cl]) cc]
        row _ _ _ = error "Invalid region"

        extendGridLeft :: [[Bool]] -> [[Bool]]
        extendGridLeft gd = map (False :) gd

        extendGridTop :: [[Bool]] -> [[Bool]]
        extendGridTop gd = newRow : gd
          where newRow = take (rowLength gd) $ repeat False
                rowLength [] = 
                  error "Invalid grid" -- if valid gd, this won't happen
                rowLength (xs : _) = length xs

-- e)
loadGridFile :: String -> IO [[Bool]]
loadGridFile f = do
  contents <- readFile f
  let grid = readGrid contents
  if valid grid 
     then return grid
     else error "Invalid grid"

-- f)
interactive :: IO ()
interactive = do 
    putStr "Please enter a file name: "
    fname <- getLine
    grid <- loadGridFile fname
    loop grid
  where loop grid = do
          printGrid grid
          putStr "\nEnter continues, \"exit\" aborts > "
          l <- getLine
          if l `elem` ["exit", "x", "quit", "q"] then
            return ()
          else
            loop (step grid)






{-
Question 1: 8 marks

Question 2: 6 marks

Question 3: 8 marks

Question : 6 marks

Question 5:  a-f) 17 marks

Question 5: g) maximally 5 marks depending on which challenge you choose, they increase in difficulty.

Overall: rules, presentation, code quality, demonstrated understanding: 10 marks
-}

