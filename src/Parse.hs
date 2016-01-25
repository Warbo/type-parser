module Parse where

import           Data.List
import qualified Language.Haskell.Exts.Parser as Parser
import qualified Language.Haskell.Exts.Pretty as Pretty
import           Language.Haskell.Exts.Syntax

-- | Parses a String as a type, returning component parts of any function
--   argument types. For example, given "(Int, Char) -> Bool -> Float" will
--   return a list like [(Int, Char), Bool, Int, Char]. Given, e.g. "Int", will
--   return []
parseArgs :: String -> [Type]
parseArgs s = case parseType s of
  Just t  -> argTypes t
  Nothing -> []

parseType :: String -> Maybe Type
parseType x = case Parser.parseType x of
  Parser.ParseFailed _ _ -> Nothing
  Parser.ParseOk t       -> Just t

-- | Get all components of the argument parts of a type, e.g. for
--   Int -> [Bool] -> String will return a list like [Int, [Bool], Bool]
argTypes :: Type -> [Type]
argTypes t = if "->" `isInfixOf` (pp t)
                then let topArgs = init (splitType t)
                      in topArgs ++ concatMap bitsOf topArgs
                else []

-- | Split up a function type Foo -> Bar -> Baz into its components
--   [Foo, Bar, Baz]
splitType (TyForall _ _ x) = splitType x
splitType (TyFun x y)      = x : splitType y
splitType t                = [t]

-- | Recursively break apart a type, to get all of the components it's made of,
--   e.g. for "Either [Bool] Int" will return a list like [Either [Bool] Int,
--   [Bool], Int, Bool]
bitsOf :: Type -> [Type]
bitsOf t = t : case t of
  TyForall   _ _ x -> bitsOf x
  TyList     x     -> bitsOf x
  TyParArray x     -> bitsOf x
  TyParen    x     -> bitsOf x
  TyKind     x _   -> bitsOf x
  TyBang     _ x   -> bitsOf x
  TyFun      x y   -> bitsOf x ++ bitsOf y
  TyApp      x y   -> bitsOf x ++ bitsOf y
  TyInfix    x _ y -> bitsOf x ++ bitsOf y
  TyEquals   x y   -> bitsOf x ++ bitsOf y
  TyTuple    _ xs  -> concatMap bitsOf xs
  TyPromoted x     -> []
  _                -> []

pp = Pretty.prettyPrintStyleMode (Pretty.style {
                                     Pretty.mode = Pretty.OneLineMode
                                  })
                                 Pretty.defaultMode
