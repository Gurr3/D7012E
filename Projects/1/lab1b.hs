import Data.List (findIndex)
import Maybe (fromJust)

xs = [1,1,8,2,2,3,3,4,5,6,4,5,7]--13
ys = [1,2,3,4,5,6,7,8]--8
zs = [1,-3,2,-2,1,-1]--6
us = [1,-3,2,-2,3,-1,1,-3,2,-2,3,-1]--12
ks = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]--20
ps = [1,-1,40,-39, -5, 30]--6
exs= [1,4,2,-1,3]


--call doit with a unique list
kmaxsubunique list k = doit k (makeuniq list)


--       take the k first elements
--       of an insertionsorted
--       among all subsets of the given list structured in the desired way
doit k = take k . iSort . rec_findsubsets 0 
--doit k ys= take k (iSort (fixy (rec_findsubsets 0 ys))))

--from lecture 2
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

ins x [] = [x]
ins x (y:ys)
	| x >= y = x:(y:ys)
	| otherwise = y : ins x ys

--from part 1a
makeuniq list = [n | (n,index) <- zip list [0..], (fromJust(findIndex (==n) list)) == index]

--find all subsets
rec_findsubsets index [] = [] 
rec_findsubsets index ls = rec_findsubsets (index+1) (tail ls) ++ rec_findsubsets' index ls

rec_findsubsets' index [] = []
rec_findsubsets' index ls = dostructure index ls ++ rec_findsubsets' index (init ls)

--find all subsets in a slow fashion
--rec_findsubsets index ls = dostructure index ls ++ rec_findsubsets (index+1) (tail ls) ++ rec_findsubsets (index) (init ls) 


--make the structure (sum, startindex, endindex)
dostructure startindex list = [(s,start,stop)| (s,start,stop) <- zip3 [(sum list)] [startindex+1] [startindex + (length list) ]]
