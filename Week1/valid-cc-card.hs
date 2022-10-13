toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev x 
    | x < 0 = []
    | x < 10    = [x]
    | otherwise = remainder:toDigitsRev ((x - remainder) `div` 10)
    where remainder = x `mod` 10


toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev  

doubleOddIndesis :: (Integral a) => [a] -> [a]
doubleOddIndesis xs = zipWith (\i el -> if odd i then el * 2 else el) [l,l-1..0] xs
                         where l = (length xs) - 1

doubleAndSum :: (Integral a) => [a] -> a 
doubleAndSum = sum . doubleOddIndesis

isValid :: (Integral a) => a -> Bool
isValid x 
   | x `mod` 10 == 0 = True 
   | otherwise     = False

computeAndValidate :: (Integral a) => [a] -> Bool
computeAndValidate digits = isValid (digitsSum + doubledDigitsSum)
   where 
      digitsSum = sum digits
      doubledDigitsSum = doubleAndSum digits

digits = [4, 0, 1, 2, 8, 8, 8, 8, 8, 8, 8, 8, 1, 8, 8, 1]


{-
In this section, you will implement the validation algorithm for
credit cards. It follows these steps:

• Double the value of every second digit beginning from the right.
That is, the last digit is unchanged; the second-to-last digit is doubled; the third-to-last digit is unchanged; and so on. For example,
[1,3,8,6] becomes [2,3,16,6].

• Add the digits of the doubled values and the undoubled digits from the original number. For example, [2,3,16,6] becomes
2+3+1+6+6 = 18

• Calculate the remainder when the sum is divided by 10. For the
above example, the remainder would be 8.
If the result equals 0, then the number is valid.
-}