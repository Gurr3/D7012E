module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] 
	
instance Parse T where
  parse =  iter Statement.parse >-> Program 
  toString = toString2

toString2 :: T -> String
toString2 (Program []) = []
toString2 (Program (stmt:stmts)) = toString stmt ++ toString2 (Program stmts)

--toString :: T -> String
  
exec :: T -> [Integer] -> [Integer]
exec (Program p) input = Statement.exec p Dictionary.empty input 