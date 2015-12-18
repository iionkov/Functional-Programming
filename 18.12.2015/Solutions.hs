difference::(Float->Float)->Float->Float->Float
difference f a b = f a - f b
define::(Float->Float)
define = (\ x -> 2*x)
composition::(Int->Int)->(Int->Int)->(Int->Int)
composition f g = (\ x -> f (g x))
derive::(Float->Float)->Float->(Float->Float)
derive f eps = (\ x -> (f x+eps - f x)/eps)
derive_n::(Float->Float)->Int->Float->(Float->Float)
derive_n f n eps
    |n==0 = f
    |otherwise = derive_n (derive f eps) (n-1) eps
isOdd::Int->Bool
isOdd x = mod x 2 == 1
mysum::[Int]->Int
mysum l=sum [x*x|x<-l, isOdd x]
scons::Int->[[Int]]->[[Int]]
scons x (y:ys)=[x:y|y<-(y:ys)]
repeated::(Int->Int)->Int->(Int->Int)
repeated f 1=(\ x -> f x)
repeated f n=(\ x -> f (repeated f (n-1) x))
repeated_with::(Int->Int)->Int->(Int->Int)
repeated_with f 1=(\ x -> f x)
repeated_with f 2=(\ x->composition f f x)
repeated_with f n=(\ x -> f (repeated_with f (n-1) x))
isPrime::Int->Int->Bool
isPrime x i
    |x==1 = False
    |i==x = True
    |mod x i==0 = False
    |otherwise = isPrime x (i+1)
helper::Int->Int->(Int,Int)
helper x i
    |(isPrime i 2) && (isPrime (x - i) 2) = (i,x-i)
    |otherwise = helper x (i+1)
list2primes::[Int]->[(Int,Int)]
list2primes lst=[helper x 2|x<-lst,even x]
main::IO()
main = do
    print(difference (\ x -> x*x) 4 3)
    print(define 2)
    print(composition (\ x -> x+2) (\ x -> 2*x) 2)
    print(derive (\ x -> x*x*x) 0.000001 1)
    print(derive_n (\ x -> x*x*x) 2 0.0001 1)
    print(mysum [1,2,4,5,6])
    print(scons 0 [[1,2],[3,4],[5]])
    print(repeated_with (\ x -> x+1) 6 4)
    print(helper 18 2)
    print(list2primes [23,24,25,26,27,28,30])
