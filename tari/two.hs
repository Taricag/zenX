module Two where

import Data.List

--1.a

null' [] ="true"
null' [_]="false"

--pembatas

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

sqr  x = x*x
kuadrat a b = a^b
kurang a b = a-b


--pembatas

map' f (x:xs)= (f x) : map' f (xs)
map' f[]= []

--pembatas




delete' n (x:xs)
  |n==x = xs
  |n/=x = x : delete' n(xs)
  |n/=x = (x:xs)

--pembatas

--delete all'

--pembatas

--foldl'

--pembatas

--foldl1'

--pembatas

--zip'

--pembatas

--zipWith'

--pembatas

--nth'

--pembatas

--scanl1

--pembatas

elem' n[]= False
elem' n (x:xs)
  |n /= x = elem' n (xs)
  |n==x = True

--pembatas

notElem' n[]= True
notElem' n (x:xs)
  |n /= x = notElem' n (xs)
  |n==x = False


--pembatas

head' (x:xs) = x

length' [] = 0
length' (x:xs) = 1 + length' (xs)

--pembatas

--reverse'

--pembatas

last' [x] = x
last' (x:xs) = last' (xs)
--pembatas

tail' (x:xs) = xs

--pembatas

init' [x] = []
init' (x:xs)= x : init' (xs)

--pembatas

min' a b
  |a<b = a
  |b<a = b

--pembatas

max' a b
  |a<b = b
  |b<a = a

--pembatas

--concat'

--pembatas

intersperse' n (x:xs) = n : x : intersperse' n(xs)
intersperse' n [] = []

--pembatas

intercalate' (x:xs) [(x:xs)] = x : xs:x: intercalate' (x:xs) [(xs)]
intercalate' (x:xs) [[]] =[]
noooo!!!!

--pembatas

--and'

--pembatas

--or'

--pembatas

--zip3'

--pembats

sum' [] = 0
sum' (x:xs) = x + sum' xs

-- pembatas

filter' (f) (x:xs)
  |f x == True = x : filter' f (xs)
  |f x == False = filter' f (xs)
filter' (f) [] = []

--pembatas
