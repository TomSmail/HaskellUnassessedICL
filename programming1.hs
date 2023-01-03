import Data.Char (ord, chr, intToDigit, isSpace)
import Data.List
import qualified Data.Map as Map
import Distribution.Simple (VersionRange)

func :: Int -> Bool
func x = ('a', x, (3, 'b')) < ('b', 5, (3, 'c'))

-- 1.5
timeConverter :: Int -> (Int, Int, Int)
timeConverter s 
    = (s `div` 3600, (s `mod` 3600) `div` 60, (s  `mod` 3600 ) `mod` 60)

-- 1.6 
polarConverter :: (Float, Float) -> (Float, Float)
polarConverter (r, theta) =
    let x = r * sin theta
        y = r * cos theta
    in (x, y)

-- 1.8
doubleDigits :: Int -> Int
doubleDigits n = (a * 10 + a ) * 100 + (b * 10 + b)
    where (a,b) = quotRem n 10

-- 1.9a
-- NOT WORKING ?? - might be an issue with my installation 
ordComp :: String -> String
ordComp = filter (\x -> ord x > 106)

-- 1.9b
thirteenTable :: Int -> [Int]
thirteenTable n 
    = [a | x <- [1..n],
           let a = 13 * x, a < n,
           last (foldr (:) [] (show a)) == '3']

-- 2.1
addDigit :: Int -> Int -> Int
addDigit y xs = xs * 10 + y

-- 2.3
type Vertex = (Float, Float)
pythagVert :: Vertex -> Vertex -> Float
pythagVert (x,y) (x',y') =  sqrt $ (x - x') ^ 2 + (y - y') ^ 2

-- 2.4
triangleArea :: Vertex -> Vertex -> Vertex -> Float
triangleArea (x,y) (d,e) (i,j) = sqrt $ s * (s-a) * (s-b) * (s-c)
    where a = pythagVert (x,y) (d,e)
          b = pythagVert (d,e) (i,j)
          c = pythagVert (x,y) (i,j)
          s = (a + b + c ) / 2

-- 2.5
isPrime :: Int -> Bool
isPrime x
    | x > 1 = not (any (\y -> x `mod` y == 0) [2..x-1])
    | otherwise = False

-- 2.6 
fact :: Int -> Int
fact 0 = 1
fact x = x * fact (x - 1)

-- 2.7
perm :: Int -> Int -> Int
perm n r = fact n `div` fact (n - r)

-- 2.9
remainder :: Int -> Int -> Int
remainder x y
    | x < 0 = y + x
    | otherwise = remainder (x-y) y

-- 2.11
-- Need help with this func
{-binary :: Int -> Int
binary x y 
    | x == 0 = y : 0
    | x `div` largestTwo x = 
    | otherwise = 
    where largestTwo x = 
-}

-- 2.12a
add :: Int -> Int -> Int
add x y
    | x == 0 = y
    | otherwise = add (pred x) (succ y)

-- 2.12b
-- not sure best way to do this 
larger :: Int -> Int -> String
larger x y
    | x == 0 = "second"
    | y == 0 = "first"
    | otherwise = larger (pred x) (pred y)

-- 2.15a
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x - 1) + fib (x -2)

-- 2.15b
fib' :: Int -> Int -> Int -> Int -> Int
fib' k k' acc i
    | i == 0 = acc
    | otherwise = fib' k' (k + k') (k + k') (i - 1)

-- 2.16
goldenRatio :: Int -> Int -> Int -> Int -> Int -> Float
goldenRatio k k' k'' acc i
    = fromIntegral (fib' k' k'' acc i) / fromIntegral (fib' k k' acc i)

-- 3.2
precedes :: Ord a => a -> a -> Bool
precedes x y = x <= y 

-- 3.3
pos :: Int -> [Int] -> Int ->  Int
pos _ [] _ = 0
pos x (y:ys) i
    | x == y = i
    | otherwise = pos x ys (i+1)

-- 3.4 This could be done much more efficiently with a hash map
-- due to immutable data types am not sure how to achieve this
--twoSame :: [Int] -> Bool
--twoSame xs =
-- foldl (\x -> Map.insertWith (+) x 1 [(5, 1), [6, 0]]) Map.empty xs
twoSame :: [Int] -> Bool
twoSame [] = False
twoSame (x:xs)
    | x `elem` xs = True
    | otherwise = twoSame xs

-- 3.5 
rev :: [a] -> [a] 
rev [] = []
rev (x:xs) 
    = rev xs ++ [x]

-- 3.6? 

-- 3.8
removeWhitespace :: String -> String
removeWhitespace str = filter (\x -> not $ isSpace x) str 

-- 3.11
primeFactors :: Int -> [Int]
primeFactors num = factors num starting
     where starting = [2..]
           factors :: Int -> [Int] -> [Int]
           factors 1 _ = []
           factors num rem@(r:rs)
            | num `mod` r == 0 = r : factors (num `div` r) rem
            | otherwise = factors num rs

-- 3.2

-- 3.2.1
-- findAll :: [(Int,Int)]
findAll a t = [y | (x, y) <- t, x == a]

-- 3.2.2
remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove key table = [(x,y) | (x,y) <- table, key /= x]


remove' :: Eq a => a -> [(a, b)] -> [(a, b)]
remove' key  = filter (\(x,y) -> x /= key) 

-- 4.2
allSame :: [Int] -> Bool
allSame xs
 | length (nub xs) == 1 = True
 | otherwise = False

allSame' :: [Int] -> Bool
allSame' xs = foldl (&&) True $ zipWith (==) xs (tail xs)

-- 4.3 a
infFactorials :: [Int]
infFactorials = take 10 $ scanl (*) 1 [2..]

-- 4.3 b
e :: Double
e = sum $ map (\x -> 1.0 / fromIntegral x) (1:infFactorials)

-- 4.4.1
squash :: (a -> a -> b) -> [a] -> [b]
squash f (x1: x2: xs) = (f x1 x2 ) : squash f (x2: xs)
squash f (x1: x2) = []

-- 4.4.2
squash' :: (a -> a -> b) -> [a] -> [b]
squash' f xs = zipWith f xs (tail xs)

-- 4.5
converge ::  (a -> a -> Bool) -> [a] -> a
converge f (x1: x2: xs)
  | f x1 x2 = x1
  | otherwise = converge f (x2: xs)
converge f (x1: x2) = x1

e' :: Double
e' = undefined 
--converge (==) $ scanl (\n -> sum $ map (\x -> 1.0 / fromIntegral x) (1:(take n $ scanl (*) 1.0 [2..]))) 1.0 [1..]

-- 4.11
pipeline :: [a -> a] -> [a] -> [a]
pipeline fs xs = map (foldr (.) id fs) xs


-- 5.1
data Shape = Triangle Side Side Side | Square Side | Circle Radius
type Radius = Float
type Side = Float

area :: Shape -> Float
area (Triangle x y z )
    = sqrt ( s
    * ( s - x) 
    * ( s - y) 
    * ( s - z )
    )
    where s = (x + y + z) / 2
area (Square x) 
    = x * x
area (Circle r )
    = pi * r * r

-- 5.2
type Shape' = [Coord]
type Coord = (Float, Float)

area' :: Shape' -> Float
area' ((x1,y1):(x2,y2):(x3,y3):[]) 
    = sqrt ( s
    * ( s - x) 
    * ( s - y) 
    * ( s - z )
    ) 
    where x = sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2
          y = sqrt $ (x2 - x3) ** 2 + (y2 - y3) ** 2
          z = sqrt $ (x3 - x1) ** 2 + (y3 - y1) ** 2
          s = (x + y + z) / 2
area' ((x1,y1):coord@(x2,y2):coords)
    = area' (coord:coords) + (sqrt $ (x1 - x2) ** 2 + (y1 - y2))

-- 5.3
data Date = Date Day Month Year
type Day = Int
type Month = Int
type Year = Int
-- age :: Date -> Date -> Int
-- age dob@(d1, m1, y1) current@(d2, m2, y2) 
--     | m1 < m2 && d1 < d2 = y2 - y1
--     | m1 < m2 = d1

