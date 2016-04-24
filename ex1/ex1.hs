toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]

toDigits x
    | x > 0 = [(x `mod` 10)]++toDigits (x `div` 10)
    | x <= 0 = []

toDigitsRev x
    | x > 0 = toDigitsRev (x `div` 10)++[(x `mod` 10)]
    | x <= 0 = []

doubleEveryOther::[Integer] -> [Integer]

doubleEveryOther [] = []
doubleEveryOther [x]=[x]
doubleEveryOther (x:y:xs) =x:2*y:doubleEveryOther xs

sumDigits ::[Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum(toDigitsRev x) + sumDigits xs

validate :: Integer -> Bool
validate x = if sumDigits(doubleEveryOther(toDigits x)) `mod` 10 ==0 then True else False

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a,c)]
hanoi n a b c = (hanoi (n - 1) a b c) ++ [(a,b)] ++ (hanoi (n - 1) c a b)

