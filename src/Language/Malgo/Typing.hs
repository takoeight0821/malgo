module Language.Malgo.Typing where

import           Control.Monad.State
import           Data.Either
import           Language.Malgo.Syntax

type Env = [(Name, Type)]

initEnv = [ ("print", FunTy UnitTy [StringTy])
          , ("println", FunTy UnitTy [StringTy])
          , ("print_int", FunTy UnitTy [IntTy])
          ]

addBind :: Name -> Type -> StateT Env (Either String) ()
addBind n t = do
  ctx <- get
  put $ (n, t):ctx

getType :: Name -> StateT Env (Either String) Type
getType n = do
  ctx <- get
  case lookup n ctx of
    Just ty -> return ty
    Nothing -> lift . Left $ "error: " ++ n ++ " is not defined.\nEnv: " ++ show ctx

typeEq :: Type -> Type -> Bool
typeEq = (==)

typeof :: Expr -> StateT Env (Either String) Type
typeof (Var name) = getType name
typeof (Int _)    = return IntTy
typeof (Float _)  = return FloatTy
typeof (Bool _)   = return BoolTy
typeof (Char _)   = return CharTy
typeof (String _) = return StringTy
typeof Unit = return UnitTy
typeof (Call name args) = do
  argsTy <- mapM typeof args
  funty <- getType name
  case funty of
    FunTy retTy paramsTy -> if and $ zipWith typeEq argsTy paramsTy
                            then return retTy
                            else lift . Left $ ("error: Expected -> " ++ show paramsTy ++
                                                 "; Actual -> " ++ show argsTy)
typeof (Seq e1 e2) = do
  ty1 <- typeof e1
  if typeEq ty1 UnitTy
    then typeof e2
    else lift . Left $ "error: Expected -> " ++ show UnitTy ++ "; Actual -> " ++ show ty1
typeof _ = lift . Left $ "TODO: implement typeof"

typeCheckExpr expr = runStateT (typeof expr) initEnv
