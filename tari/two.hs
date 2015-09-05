module Two where

import Data.List

gaya m a = a*m

take' 0 _=[]
take' _ []=[]
take' n (x:xs)= x : take' (n-1) (xs)

drop' 0 (x:xs) = (x:xs)
drop' _ [] = []
drop' n (x:xs) = drop' (n-1) (xs)

fst' (a,b) = a

snd' (a,b) = b

delete' _ [] = []
delete' 

sqr x= x^2

map' f (x:xs)= (f x) : map' f (xs)
map' f[]= []
