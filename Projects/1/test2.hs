import Data.List
r index [] = []
r index ls = (show ls) ++ r (index+1) (tail ls) ++ r (index) (init ls) 
a = "a"
b = "b"
c = "c"

resort k = reverse . sort $ k