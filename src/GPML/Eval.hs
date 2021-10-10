{-#LANGUAGE TemplateHaskell#-}
module GPML.Eval where

import GPML.Prelude
import GPML.Types.AST

import Data.Text qualified as T

import Data.Map qualified as M

data EvalError = VariableOutOfScope Name 
               | CannotUnquote Value
               | CannotCall Value [Value]
               | WrongNumberOfArgs [Name] [Value]
               deriving (Show, Eq)

data EvalFrame = EvalFrame {
    _frameVars :: Map Name Value
}
makeLenses ''EvalFrame

eval :: Members '[State (NonEmpty EvalFrame), Error EvalError] r 
     => Expr 
     -> Sem r Value
eval (IntLit n)    = pure $ VInt n
eval (DoubleLit x) = pure $ VDouble x
eval (Lambda vs es) = pure $ VLambda vs es

eval (Var x) = gets (viaNonEmpty head . mapMaybe (lookup x . view frameVars) . toList) >>= \case
    Nothing -> throw $ VariableOutOfScope x
    Just v -> pure v

eval (DefineVar x e)     = eval e >>= \v -> modify (_head1 . frameVars %~ insert x v) $> VString "" 
eval (DefineFun f xs es) = eval (DefineVar f (Lambda xs es))

eval (SourceCode segs) = VString . T.concat <$> forM segs \case
        Quote t   -> pure t
        UnQuote es -> valueToText . last =<< traverse eval es

eval (App f as) = do
    f' <- eval f
    as' <- traverse eval as 
    case f' of
        VLambda ps es
            | length ps == length as -> do
                modify (EvalFrame (M.fromList (zip ps as')) <|)
                last <$> traverse eval es 
                <* modify (unsafeTail)
            | otherwise -> throw $ WrongNumberOfArgs ps as'
        v -> throw $ CannotCall v as'


valueToText :: Members '[Error EvalError] r 
     => Value
     -> Sem r Text
valueToText (VString t) = pure t
valueToText (VInt n) = pure (show n)
valueToText (VDouble x) = pure (show x)
valueToText v@(VList _) = throw $ CannotUnquote v
valueToText v@(VLambda _ _) = throw $ CannotUnquote v

runWithInitialFrames :: Sem (State (NonEmpty EvalFrame) : r) a -> Sem r a
runWithInitialFrames = evalState (one (EvalFrame mempty))
