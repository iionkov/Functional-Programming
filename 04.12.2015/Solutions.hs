count_digits :: Int -> Int
count_digits n = if (n < 10) then 1 else 1+count_digits(n `div` 10)

sum_digits :: Int -> Int
sum_digits n = if(n<10)then n else ( n ` mod` 10)+sum_digits(n `div` 10)

pow :: Double -> Int -> Double
pow number k = if(k == 0) then 1 else number * pow number ( k - 1)

help_iter :: Int -> Int -> Int
help_iter x res = if(x == 0) then res else help_iter ( x `div` 10) ( res + (x `mod` 10))


sum_digits_iter :: Int -> Int
sum_digits_iter n = help_iter n 0

help_rev :: Int -> Int -> Int
help_rev 0 res = res
help_rev n res = help_rev ( n `div` 10) ( 10 * res + ( n `mod` 10) )

rev :: Int -> Int
rev n = help_rev n 0

help_prime :: Int-> Int->Bool
help_prime n k  = if(( k + 1 ) == n)then True else if( (n `mod` k) == 0 ) then False else help_prime n ( k + 1)

prime :: Int -> Bool
prime 2 = True
prime 1 = False
prime n = help_prime n 2

main::IO()
main = do
  print(prime 13)
