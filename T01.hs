--http://learnyouahaskell.com/introduction
--Up to Higher order functions


doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                     then x else x*2

doubleSmallNumber' x = (if x > 100 then x else 2*x) + 1

boomBangs xs = [if x < 10 then "BOOMS!" else "BANG!" | x <- xs, odd x]

length' list = sum [1| _ <- list]

removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

removeNonUppercase_wrong st = [c | c <- st, c <- ['A'..'Z']]


lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"  


sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"  



factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)  


length'' :: (Num b) => [a] -> b  
length'' [] = 0  
length'' (_:xs) = 1 + length' xs 


capital :: String -> String  
-- |capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  

mytake :: (Integral i) => i -> [b] -> [b]
mytake _ [] = error "list does not have enough elements!"
mytake 1 (x:xs) = [x]
mytake n (x:xs) = x : (mytake (n-1) xs ) 

take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs  


multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z

myMinus :: (Num a) => a -> a -> a
myMinus x y = x - y

myMinusTen :: (Num a) => a -> a
myMinusTen x = myMinus x 10

applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)  

applyTwice' :: a -> (a -> a) -> a  
applyTwice' x f = f (f x)  

filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []  
  
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  
  
last' :: [a] -> a  
last' = foldl1 (\_ x -> x)


oddSquareSum :: Integer  
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]  

-- oddSquareSum :: Integer  
-- oddSquareSum =   
    -- let oddSquares = filter odd $ map (^2) [1..]  
        -- belowLimit = takeWhile (<10000) oddSquares  
    -- in  sum belowLimit  