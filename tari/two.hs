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

map' f (x:xs)= (f x) : map' f (xs)
map' f[]= []

--pembatas

filter' (f) [] = []
filter' (f) (x:xs)
  |f x == True = x : filter' f (xs)
  |f x == False = filter' f (xs)

--pembatas

delete' n (x:xs)
  |n==x = xs
  |n/=x = x : delete' n(xs)
  |n/=x = (x:xs)

--pembatas

deleteall' n (x:xs)
  |n==x = deleteall' n (xs)
  |n/=x = x : deleteall' n (xs)
deleteall' n [] = []

--pembatas

foldl'' f n (x:xs) = f n (product'(x:xs))

--pembatas

foldl1'' f [x] = x
foldl1'' f (x:xs)=  f x (foldl1' f (xs))


-- foldl1 (-) [2,2,2,2] = - 2 ffoldl fx

--pembatas

zip' (x':xs') (x:xs) = (x',x) : zip' (xs') (xs)
zip' [] (x:xs) = []
zip' (x:xs) [] = []
zip' [] [] = []


--pembatas

zipWith' f [] (x:xs) = []
zipWith' f (x':xs') [] = []
zipWith' f [] [] = []
zipWith' f (x':xs') (x:xs)= f x' x : zipWith' f (xs') (xs)

--pembatas

nth  (x:xs) 0 = (x)
nth (x:xs) n = nth (xs) (n-1)

--pembatas

--scanl' f n (x:xs)= n : f x : f xs
--f xs =
--
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

--pembatas

length' [] = 0
length' (x:xs) = 1 + length' (xs)

--pembatas

reverse' [] = []
reverse' (x:xs)= reverse' (xs) ++ x :[]


--pembatas

last' [x] = x
last' (x:xs) = last' (xs)

--pembatas

tail' (x:xs) = (xs)

--pembatas

init' [x] = []
init' (x:xs)= x : init' (xs)

--pembatas

max' a b
  |a<b = b
  |otherwise = a

--pembatas

min' a b
  |a<b = a
  |otherwise = b

--pembatas

concat' [x] = x
concat' (x:xs) = x ++ concat' xs

--pembatas

intersperse' n (x:xs) = n : x : intersperse' n(xs)
intersperse' n [] = []

--pembatas

--intercalate' [] (x':xs') = x':xs'
intercalate' (x:xs) [[]] =[]
--intercalate' (x:xs) [[x']]= [x']
intercalate' (x:xs)(x':xs') =  x' ++ (x:xs) ++ intercalate' (x:xs) xs'

--intrcalate [1,1] [[2,2],[3,3],[4,4]] = [2,2] ++ [1,1]++ [3,3] ++[1,1] ++
 --pembatas

and' [] =True
and' (fx:fxs)
  |fx== False= False
  |fx == True = and' (fxs)

--pembata

or' [] =False
or' (fx:fxs)
  |fx == True = True
  |fx == False = or' (fxs)

--pembatas

zip3' (x':xs') (x'':xs'')(x:xs) = (x',x'',x) : zip3' (xs') (xs'') (xs)
zip3' [] (x'':xs'')(x:xs) =[]
zip3' (x':xs') [](x:xs)=[]
zip3' (x':xs') (x'':xs'')[] =[]
zip3' [] [] (x:xs)=[]
zip3' [] (x'':xs'')[] = []
zip3' (x':xs') [][] = []
zip3' [] [] [] =[]


--pembatas

sum' [] = 0
sum' (x:xs) = x + sum' xs

-- pembatas

product' [] = 1
product' (x:xs) = x*product' (xs)

--pembatas

--words'

--pembatas

--words' "" = [x]
--words' "" = x : words' xs


--pembatas

lines' "a" = [" a "]

unlines' ["x"] = "x \n"

--pembatas

--unwords

--pembatas

takeWhile' f [] = []
takeWhile' f (x:xs)
  | (f x)== True =  x : takeWhile' f (xs)
  | (f x)== False = takeWhile' f (xs)

--pembatas

dropWhile' f [] = []
dropWhile' f (x:xs)
  | (f x) == True = dropWhile' f (xs)
  | (f x) == False = x : dropWhile' f (xs)

--pembatas

--concatMap

--pembatas

all' f [] = True
all' f (x:xs)
  | (f x) == False = False
  | (f x) == True = all' f (xs)

--pembatas

any' f [] = False
any' f (x:xs)
  | (f x) == True =True
  | (f x) == False = any' f (xs)

--pembatas

insert' n [] = [n]
insert' n (x:xs)
  |n <= x = n : (x:xs)
  |n > x = x : insert' n (xs)

--pembatas

zipWith3' f (x':xs') (x'':xs'')(x:xs) =(f x' x'' x) : zipWith3' f (xs') (xs'')(xs)

--fungsi
krg a b = a-b
sqr a b = a^b
