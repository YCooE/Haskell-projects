module DescendingOrder where
    import Data.List (sort)
    import Data-Char (intToDigit)

digs ::Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

joiner :: [Integer] -> Integer
joiner = read. concatMap show

reverseL :: [Integer] -> [Integer]
reverseL (x:xs) = reverseL xs ++ [x]

joinnnn :: [Integer] -> Integer
joinnnn (x:xs) = read(concat(map show (x:xs))) :: Integer

test :: IO()

test = putStrLn $ show $ digs 123131

descendingOrder :: Integer -> Integer
descendingOrder a = joinnnn (reverse(sort(digs a)))

-- Much Smarter Way on Intenet haha -- 
descendingOrder2 :: Integer -> Integer
descendingOrder2 :: read . reverse . sort . show