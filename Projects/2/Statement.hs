module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
	Skip |
	Begin [Statement] |
    If Expr.T Statement Statement |
	While Expr.T Statement|
	Read String|
	Write Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip;" >-> buildSkip
buildSkip _= Skip

begin = (accept "begin" -# statements) #- require "end" >-> buildBegin
buildBegin s = Begin s 

iff = (accept "if" -# Expr.parse ) # (require "then" -# statement) # (require "else" -# statement) >-> buildIff
buildIff ((q, b), c) = If q b c

while = (accept "while" -# Expr.parse ) # (require "do" -# statement) >-> buildWhile
buildWhile (a,b) = While a b 

read1 = accept "read" -# word #- require ";" >-> buildRead
buildRead a = Read a

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite a= Write a

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] dict input =
	[]
exec (Assignment var expr : stmts) dict input =
	exec stmts (Dictionary.insert (var, Expr.value expr dict) dict) input
exec (Skip : stmts) dict input= 
	exec stmts dict input
exec (Begin listofstmts : stmts) dict input = 
	exec (listofstmts++stmts) dict input
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input	
exec (While cond stmt:stmts) dict input=
	if (Expr.value cond dict)>0
	then exec (stmt:While cond stmt:stmts) dict input 
	else exec stmts dict input
exec (Read var : stmts) dict (useinput:input) =
	exec stmts (Dictionary.insert (var,useinput) dict) input
exec (Write var : stmts) dict input = 
	Expr.value var dict : exec stmts dict input  


statement = assignment !begin !iff !while !read1 !write !skip
statements = iter statement


shaw :: T -> String
shaw (Assignment var expr) =
	var ++ " := " ++ toString expr ++ ";\n"
shaw (Begin stmts) = 
	"begin\n" ++ shawbegin stmts ++ "end\n"
shaw (If cond thenStmts elseStmts) =
	"if " ++ toString cond ++" then\n"++ shaw thenStmts ++"else\n"++ shaw elseStmts
shaw (While cond stmt) =
	"While " ++ toString cond++ " do\n" ++ shaw stmt
shaw (Read var)  =
	"read " ++ var ++";\n"
shaw (Write var) =
	"write " ++ toString var ++";\n"
shaw (Skip) = "skip;\n"

shawbegin :: [Statement] -> String
shawbegin [] = []
shawbegin (stmt:stmts) = toString stmt ++ shawbegin stmts


instance Parse Statement where
  parse = statement
  toString = shaw  --error "Statement.toString not implemented"
  
  
{--
 statement ::= 
		variable ':=' expr ';'
	   | 'skip' ';'
	   | 'begin' statements 'end'
	   | 'if' expr 'then' statement 'else' statement
	   | 'while' expr 'do' statement
	   | 'read' variable ';'
	   | 'write' expr ';'
--}