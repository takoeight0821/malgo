module Language.Malgo.LetLang where

import           Control.Monad.State
import qualified Data.Map              as Map
import           Language.Malgo.Syntax

type Env = Map.Map Name AST

emptyEnv :: Env
emptyEnv = Map.empty

extendEnv :: Name -> AST -> Env -> Env
extendEnv var val env = Map.insert var val env

applyEnv :: Env -> Name -> Maybe AST
applyEnv env var = Map.lookup var env

valueOf :: AST -> StateT Env (Either String) AST
valueOf (Int i) = return (Int i)
valueOf (Tree [Symbol "-", lhs, rhs]) =
  do Int lhs' <- valueOf lhs
     Int rhs' <- valueOf rhs
     return (Int (lhs' - rhs'))
valueOf (Tree [Symbol "zero?", x]) =
  do Int x' <- valueOf x
     return (Bool (x' == 0))
valueOf (Tree [Symbol "if", c, t, e]) =
  do (Bool b) <- valueOf c
     if b then valueOf t else valueOf e
valueOf (Symbol a) =
  do env <- get
     case applyEnv env a of
       Just ast -> return ast
       Nothing  -> fail $ a ++ " is not found"
valueOf (Tree [Symbol "let", Symbol var, val, body]) =
  do val' <- valueOf val
     env <- get
     put $ extendEnv var val' env
     valueOf body

runValueOf ast = runStateT (valueOf ast) emptyEnv