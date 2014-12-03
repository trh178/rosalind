module Primes where

primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:ps) = p : sieve [x | x <- ps, x `mod` p /= 0]
