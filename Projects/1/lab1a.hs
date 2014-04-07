import Data.List (findIndex, findIndices)
import Maybe (fromJust)

xs :: [Int]
xs = [1,1,8,2,2,3,3,4,5,6,4,5,7]
ys = [1,2,3,4,5,6,7,8]

-- BEGIN PART 1

--v1
makeuniq xs = [n | (n,m) <- zip xs [0..], (head(findIndices (==n) xs)) == m]

--v2
makeuniq' list = [n | (n,index) <- zip list [0..], (fromJust(findIndex (==n) list)) == index]



{--   Number the elements of a list (so I can process each one differently according to its position). 

	                 zip xs [0..]
     For example,    zip ['a','b','c'] [0..]
     gives              [('a',0),('b',1),('c',2)]

http://www.haskell.org/haskellwiki/How_to_work_on_lists#Modifying_the_list_or_its_elements
--}
