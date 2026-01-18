module Repl.Compiler (
  compileModule,
  runAction
) where

import qualified Prelude ()
import MHSPrelude

import Repl.Context (ReplCtx(..))
import Repl.Error (ReplError(..))

import MicroHs.Compile (Cache, compileModuleP, compileToCombinators)
import MicroHs.CompileCache (cachedModules)
import MicroHs.TypeCheck (TModule(..), tBindingsOf, Symbols)
import MicroHs.Translate (TranslateMap, translateMap, translateWithMap)
import MicroHs.Ident (Ident, qualIdent, unQualIdent)
import MicroHs.Builtin (builtinMdl)
import MicroHs.Exp (Exp(Var))
import MicroHs.Desugar (LDef)
import MicroHs.Expr (ImpType(..))
import MicroHs.Parse (parse, pTopModule)
import MicroHs.StateIO (runStateIO)
import qualified MicroHs.IdentMap as IMap
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (isJust)
import Data.List (foldl')

compileModule :: ReplCtx -> String -> IO (Either ReplError (TModule [LDef], Cache, Symbols))
compileModule ctx src =
  case parse pTopModule "<repl>" src of
    Left perr -> pure (Left (ReplParseError perr))
    Right mdl -> do
      r <- runStateIO (compileModuleP (rcFlags ctx) ImpNormal mdl) (rcCache ctx)
      let (((dmdl, syms, _, _, _), _), cache') = unsafeCoerce r
          cmdl = compileToCombinators dmdl
      pure (Right (cmdl, cache', syms))

runAction :: Cache -> TModule [LDef] -> Ident -> IO (Either ReplError ())
runAction cache cmdl ident = do
  let defs      = tBindingsOf cmdl
      baseMap   = withBuiltinAliases $ translateMap $ concatMap tBindingsOf (cachedModules cache)
      actionAny = translateWithMap baseMap (defs, Var (qualIdent (tModuleName cmdl) ident))
      action    = unsafeCoerce actionAny :: IO ()
  action
  pure (Right ())

withBuiltinAliases :: TranslateMap -> TranslateMap
withBuiltinAliases mp = foldl' addAlias mp (IMap.toList mp)
  where
    addAlias acc (ident, val) =
      let aliasIdent = qualIdent builtinMdl (unQualIdent ident)
      in if aliasIdent == ident || isJust (IMap.lookup aliasIdent acc)
           then acc
           else IMap.insert aliasIdent val acc
