{
module Language.Malgo.Parser where

import Prelude hiding (EQ, LT, GT)
import Language.Malgo.Lexer
import Language.Malgo.Syntax
import Language.Malgo.Utils
}

%name parse
%tokentype { Token }
%error { parseError }

%token
let   { Token (_, LET) }
in    { Token (_, IN) }
end   { Token (_, END) }
val   { Token (_, VAL) }
fun   { Token (_, FUN) }
if    { Token (_, IF) }
then  { Token (_, THEN) }
else  { Token (_, ELSE) }
'('   { Token (_, LPAREN) }
')'   { Token (_, RPAREN) }
':'   { Token (_, COLON) }
';'   { Token (_, SEMICOLON) }
','   { Token (_, COMMA) }
'=='  { Token (_, EQ) }
'='   { Token (_, EQUAL) }
'<>'  { Token (_, NEQ) }
'<'   { Token (_, LT) }
'>'   { Token (_, GT) }
'<='  { Token (_, LE) }
'>='  { Token (_, GE) }
'+'   { Token (_, PLUS) }
'-'   { Token (_, MINUS) }
'*'   { Token (_, ASTERISK) }
'/'   { Token (_, SLASH) }
'%'   { Token (_, PERCENT) }
'&&'  { Token (_, AND) }
'||'  { Token (_, OR) }
id    { Token (_, ID _) }
float { Token (_, FLOAT _) }
int   { Token (_, INT _) }
char  { Token (_, CHAR _) }
bool  { Token (_, BOOL _) }
str   { Token (_, STRING _) }

%right ';'
%right then else
%nonassoc '==' '<>'
%nonassoc '<' '>' '<=' '>='
%left '&&' '||'
%left '+' '-'
%left '*' '/' '%'
%nonassoc NEG
%%

exp: exp '+' exp { BinOp (_info $2) Add $1 $3 }
   | exp '-' exp { BinOp (_info $2) Sub $1 $3 }
   | exp '*' exp { BinOp (_info $2) Mul $1 $3 }
   | exp '/' exp { BinOp (_info $2) Div $1 $3 }
   | exp '%' exp { BinOp (_info $2) Mod $1 $3 }
   | exp '==' exp { BinOp (_info $2) Eq $1 $3 }
   | exp '<>' exp { BinOp (_info $2) Neq $1 $3 }
   | exp '<' exp { BinOp (_info $2) Lt $1 $3 }
   | exp '>' exp { BinOp (_info $2) Gt $1 $3 }
   | exp '<=' exp { BinOp (_info $2) Le $1 $3 }
   | exp '>=' exp { BinOp (_info $2) Ge $1 $3 }
   | exp '&&' exp { BinOp (_info $2) And $1 $3 }
   | exp '||' exp { BinOp (_info $2) Or $1 $3 }
   | let decls in exp end { Let (_info $1) (reverse $2) $4 }
   | if exp then exp else exp { If (_info $1) $2 $4 $6 }
   | exp ';' exp { Seq (_info $2) $1 $3 }
   | id { Var (_info $1) (_id . _tag $ $1) }
   | int { Int (_info $1) (_int . _tag $ $1) }
   | '-' int %prec NEG { BinOp (_info $1) Sub
                           (Int (_info $1) 0)
                           (Int (_info $2) (_int . _tag $ $2))
                       }
   | float { Float (_info $1) (_float . _tag $ $1) }
   | '-' float %prec NEG { BinOp (_info $1) Sub
                             (Float (_info $1) 0)
                             (Float (_info $2) (_float . _tag $ $2))
                         }
   | bool { Bool (_info $1) (_bool . _tag $ $1) }
   | char { Char (_info $1) (_char . _tag $ $1) }
   | str  { String (_info $1) (_str . _tag $ $1) }
   | id '(' args ')' { Call (_info $1) (Var (_info $1) . _id . _tag $ $1) (reverse $3) }
   | '(' ')' { Unit (_info $1) }
   | '(' exp ')' { $2 }

args : args ',' exp { $3 : $1 }
     | exp { [$1] }


decls : decls decl { $2 : $1 }
      | decl { [$1] }

decl :: { Decl }
decl : val id ':' id '=' exp { ValDec (_info $1) (_id . _tag $ $2)
                                 (NameTy (_id . _tag $ $4))
                                 $6
                             }
     | fun id '(' params ')' ':' id '=' exp {
         FunDec (_info $1) (_id . _tag $ $2)
           (reverse $4)
           (NameTy (_id . _tag $ $7))
           $9
       }

params :: { [(Name, Type)] }
params : params ',' param { $3 : $1 }
       | param { [$1] }

param :: { (Name, Type) }
param : id ':' id { (_id . _tag $ $1, NameTy (_id . _tag $ $3)) }

{
parseError :: [Token] -> a
parseError [] = error "Parse error at EOF"
parseError (t:ts) = error $ "Parse error:" ++ show t
}
