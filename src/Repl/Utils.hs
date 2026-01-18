module Repl.Utils (
  moduleHeader,
  buildModule,
  ensureTrailingNewline,
  indent,
  dropWhileEnd,
  isws,
  allwsLine
) where

import qualified Prelude ()
import MHSPrelude
import Data.List (reverse, dropWhile)

moduleHeader :: String
moduleHeader = unlines
  [ "module Inline where"
  , "import Prelude"
  , "import System.IO.PrintOrRun"
  , "import Data.Typeable"
  , "import Numeric"
  ]

buildModule :: String -> String
buildModule defs = moduleHeader ++ defs

ensureTrailingNewline :: String -> String
ensureTrailingNewline s
  | null s         = "\n"
  | last s == '\n' = s
  | otherwise      = s ++ "\n"

indent :: String -> String
indent = unlines . map ("  " ++) . lines

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse

isws :: Char -> Bool
isws c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

allwsLine :: String -> Bool
allwsLine = all isws
