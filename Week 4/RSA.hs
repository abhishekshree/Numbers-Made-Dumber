module RSA where
import Data.Char ( chr )
import System.IO

toIntList :: String -> [Integer]
toIntList s = map (toInteger . fromEnum ) s
 
toString :: [Integer] -> String
toString intArr = map ( chr . fromInteger ) intArr 

-- Gets divisors
divisors :: Integer -> [Integer]
divisors n = [m | m <- [1..n] , mod n m == 0 ]

-- Prime check
isPrime :: Integer -> Bool
isPrime n = divisors n == [1,n]

-- Euler Phi Function
phi :: Integer -> Integer -> Integer
phi prime1 prime2 = (prime1 - 1 ) * ( prime2 - 1 ) 

-- Breaks after finding the first such e, thanks to laziness.
calculateE :: Integer -> Integer
calculateE tot = head [n | n <- [2..tot - 1] , gcd n tot == 1]

-- Breaks after finding the first such d, thanks to laziness.
calculateD :: Integer -> Integer -> Integer  -> Integer
calculateD e n phi = head [d | d <- [1..n] , mod ( d * e ) phi == 1]

-- Encode logic
rsaEncode :: Integer -> Integer -> [Integer] -> [Integer]
rsaEncode n e numbers = map (\num -> mod ( num ^ e ) n ) numbers

-- Decode logic
rsaDecode :: Integer -> Integer -> [Integer] -> [Integer]
rsaDecode d n ciphers = map (\c -> mod ( c ^ d ) n ) ciphers

main :: IO ()
main = do
  putStr "The phrase is: "
  hFlush stdout -- To flush newline
  text <- getLine
  let primes       = take 50 $ filter isPrime [1..]
      p1           = last primes
      p2           = last $ init primes
      totient      = phi p1 p2
      e            = calculateE totient
      n            = p1  * p2 
      encoded      = rsaEncode n e $ toIntList text
      d            = calculateD e n totient
      encrypted    = concatMap show encoded
      decrypted    = toString $ rsaDecode d n encoded
  putStrLn ("Encrypted: " ++ encrypted) 
  putStrLn ("Decrypted: " ++ decrypted)
