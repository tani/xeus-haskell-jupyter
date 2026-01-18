module Repl.Context (
  ReplCtx(..),
  StoredDef(..),
  initialCtx,
  appendDefinition,
  currentDefsSource
) where

import qualified Prelude ()
import MHSPrelude

import System.Environment (lookupEnv)
import Data.List (nub)
import MicroHs.Compile (Cache, emptyCache)
import MicroHs.SymTab (stEmpty)
import MicroHs.TypeCheck (Symbols)
import MicroHs.Flags (Flags(..), defaultFlags)
import MicroHs.Ident (Ident)

import Repl.Error (ReplError(..))
import Repl.Analysis (extractDefinitionNames)

data StoredDef = StoredDef
  { sdCode :: String
  , sdNames :: [Ident]
  }

data ReplCtx = ReplCtx
  { rcFlags :: Flags
  , rcCache :: Cache
  , rcDefs  :: [StoredDef]
  , rcSyms  :: Symbols
  }

initialCtx :: String -> IO ReplCtx
initialCtx dir = do
  let flags = defaultFlags dir
  mpath <- lookupEnv "MHS_LIBRARY_PATH"
  let rpath = maybe "." id mpath
      extra = splitColon rpath
      rcFlags = flags { paths = paths flags ++ extra }
  pure ReplCtx { rcFlags = rcFlags, rcCache = emptyCache, rcDefs = [], rcSyms = (stEmpty, stEmpty) }

splitColon :: String -> [String]
splitColon s = case break (== ':') s of
  (a, [])     -> [a]
  (a, _:rest) -> a : splitColon rest

defsSource :: [StoredDef] -> String
defsSource = concatMap sdCode

currentDefsSource :: ReplCtx -> String
currentDefsSource = defsSource . rcDefs

stripRedefined :: [StoredDef] -> [Ident] -> [StoredDef]
stripRedefined defs [] = defs
stripRedefined defs names = filter noOverlap defs
  where
    noOverlap def = all (`notElem` names) (sdNames def)

appendDefinition :: ReplCtx -> String -> Either ReplError [StoredDef]
appendDefinition ctx snippet =
    case extractDefinitionNames snippet of
        Left err -> Left err
        Right names ->
            let uniqueNames = nub names
                retainedDefs = stripRedefined (rcDefs ctx) uniqueNames
            in Right (retainedDefs ++ [StoredDef snippet uniqueNames])
