module Parse where

import           Data.List
import qualified Language.Haskell.Exts.Parser as Parser
import qualified Language.Haskell.Exts.Pretty as Pretty
import           Language.Haskell.Exts.Syntax

data RoseTree = Leaf String | Node [RoseTree] deriving (Eq)

instance Show RoseTree where
  show (Leaf x) = show x
  show (Node xs) = "(" ++ intercalate " " (map show xs) ++ ")"

parseType :: String -> Maybe RoseTree
parseType x = case Parser.parseType x of
  Parser.ParseFailed _ _ -> Nothing
  Parser.ParseOk t       -> Just (parseType' t)

parseType' :: Type -> RoseTree
parseType' t = case t of
  TyCon name -> Leaf (pp name)

pp = Pretty.prettyPrint
