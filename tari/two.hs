module Two where

import Data.List

--1.a

take' 0 _=[]
take' _ []=[]
take' n (x:xs)= x : take' (n-1) (xs)

--pembatas

drop' 0 (x:xs) = (x:xs)
drop' _ [] = []
drop' n (x:xs) = drop' (n-1) (xs)

--pembatas

fst' (a,b) = a

--pembatas

snd' (a,b) = b

--pembatas

sqr x= x^2

--pembatas

map' f (x:xs)= (f x) : map' f (xs)
map' f[]= []

--pembatas

delete' n (x:xs)
  |n==x = xs
  |n/=x = x : delete' n(xs)
    |n/=x = (x:xs)
