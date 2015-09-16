module Two where

import Data.List

--1.a

null' [] = True
null' [_]= False

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
  |otherwise = filter' f (xs)

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

foldl'' f n [] = n
foldl'' f n (x:xs) = f (foldl'' f n xs) x

--pembatas

foldl1'' f [x] = x
foldl1'' f (x:xs)= f x  (foldl1'' f (xs))

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

scanl'' f n [] =[n]
scanl'' f n (x:xs)= [n] ++ scanl'' f (f n x) xs

--pembatas

--scanl1'' f []= []
--scanl1'' f [x]= [x]
-- canl1'' f (x:xs) =


--pembatas

elem' n[]= False
elem' n (x:xs)
  |n /= x = elem' n (xs)
  |otherwise= True

--pembatas

notElem' n[]= True
notElem' n (x:xs)
  |n /= x = notElem' n (xs)
  |otherwise =  False

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

intercalate' [] [x'] = x'
intercalate' [] (x':xs') = x' ++ intercalate' [] xs'
intercalate' (x:xs) [x']= x'
intercalate' (x:xs)(x':xs') =  x' ++ (x:xs) ++ intercalate' (x:xs) xs'

 --pembatas

and' [] =True
and' (fx:fxs)
  |fx== False= False
  |otherwise = and' (fxs)

--pembata

or' [] =False
or' (fx:fxs)
  |fx == True = True
  |otherwise = or' (fxs)

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

words' [x] = [[x]]
words' (x:xs)
  |x == ' ' = words' (xs)
  |x == '\n' =  words' (xs)
  |otherwise = [x] : words' (xs)

  --(masih salah)  --words' "we d a \n g ja he"
  --["w","e","d","a","g","j","a","h","e"]
  --malah gini

--pembatas

lines' []= [[]]
lines' (x:xs)
  |x == '\n' = [] : lines' (xs)
  |otherwise = x : lines' (xs)



--pembatas

unlines' [] = ""
unlines' (x:xs) = x ++ "\n" ++ (unlines' xs)

--pembatas

unwords' [] = ""
unwords' (x:xs) = x ++ unwords (xs)

--pembatas

takeWhile' f [] = []
takeWhile' f (x:xs)
  | (f x)== True =  x : takeWhile' f (xs)
  | (f x)== False = []

--pembatas

dropWhile' f [] = []
dropWhile' f (x:xs)
  | (f x) == True = dropWhile' f (xs)
  | (f x) == False = (x:xs)

--pembatas

concatMap' f [x] = f x ++ []
concatMap' f (x:xs) = f x ++  concatMap' f (xs)

--pembatas

all' f [] = True
all' f (x:xs)
  | (f x) == False = False
  | otherwise = all' f (xs)

--pembatas

any' f [] = False
any' f (x:xs)
  | (f x) == True =True
  | otherwise = any' f (xs)

--pembatas

insert' n [] = [n]
insert' n (x:xs)
  |n <= x = n : (x:xs)
  |n > x = x : insert' n (xs)

--pembatas

zipWith3' f (x':xs') (x'':xs'')(x:xs) =(f x' x'' x) : zipWith3' f (xs') (xs'')(xs)

--pembatas

nub' []=[]
nub' (x:xs) = [x] ++ nub' (deleteall' x (xs))

--pembatas

sort' [] =[]
sort' (x:xs) =minimum (x:xs) : sort' (delete' (minimum (x:xs)) (x:xs))

--pembatas

minimum' [x] = x
minimum' (x:xs) = minimum' ((min' x (head' xs)) : tail' xs)

--pembatas

maximum' [x] = x
maximum' (x:xs) = maximum' ((max' x(head' xs )) : tail' xs)

--pembatas

inits' [] = [[]]
inits' (x:xs) =  inits' (init' (x:xs)) ++ [x:xs]

--pembatas

tails' [] = [[]]
tails' (x:xs)= (x:xs) : tails' (xs)

--pembatas

union' (x:xs) (x':xs')= nub' ((x:xs) ++ (x':xs'))

--pembatas

intersect' [] (x':xs') = []
intersect' (x:xs) (x':xs')= same x (x':xs') ++ intersect' (xs) (x':xs')
  where same n [] = []
        same x (x':xs')
          |x == x' = [x']
          |x /= x' = same x (xs')

--pembatas

group' [x] = [[x]]
group' (x:xs) = [x] : group' (xs)

--pembatas

splitAt' n (x:xs) =(take' n (x:xs) ,  drop' n (x:xs))

--pembatas

partition' f (x:xs)= (filter' f (x:xs) , unfilter f (x:xs))
  where unfilter (f) [] = []
        unfilter (f) (x:xs)
          |f x == True = unfilter f (xs)
          |f x == False = x : unfilter f (xs)

--pembatas

replicate' 0 b = []
replicate' a b = b : replicate' (a-1) b

--pembatas

--fungsi
