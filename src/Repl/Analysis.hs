module Repl.Analysis (
  canParseDefinition,
  canParseExpression,
  extractDefinitionNames,
  completionCandidates,
  isIncomplete
) where

import qualified Prelude ()
import MHSPrelude

import Repl.Error (ReplError(..))
import Repl.Utils (buildModule, ensureTrailingNewline)

import MicroHs.Parse (parse, pExprTop, pTopModule)
import MicroHs.Expr (EModule(..), EDef(..), patVars)
import MicroHs.Ident (Ident, SLoc(..))
import MicroHs.Lex (Token(..))
import qualified MicroHs.Lex as Lex
import Data.List (nub)

canParseDefinition :: String -> Bool
canParseDefinition snippet =
  case parse pTopModule "<xhaskell-define>" (buildModule (ensureTrailingNewline snippet)) of
    Right _ -> True
    Left _  -> False

canParseExpression :: String -> Bool
canParseExpression snippet =
  case parse pExprTop "<xhaskell-expr>" snippet of
    Right _ -> True
    Left _  -> False

extractDefinitionNames :: String -> Either ReplError [Ident]
extractDefinitionNames snippet =
  case parse pTopModule "<xhaskell-define-names>" (buildModule snippet) of
    Left err -> Left (ReplParseError err)
    Right mdl -> Right (definitionNamesFromModule mdl)

definitionNamesFromModule :: EModule -> [Ident]
definitionNamesFromModule (EModule _ _ defs) = concatMap definitionNames defs
  where
    definitionNames def =
      case def of
        Data lhs _ _        -> [lhsIdent lhs]
        Newtype lhs _ _     -> [lhsIdent lhs]
        Type lhs _          -> [lhsIdent lhs]
        Fcn name _          -> [name]
        PatBind pat _       -> patVars pat
        Sign names _        -> names
        KindSign name _     -> [name]
        Pattern lhs _ _     -> [lhsIdent lhs]
        Class _ lhs _ _     -> [lhsIdent lhs]
        DfltSign name _     -> [name]
        ForImp _ _ name _   -> [name]
        Infix _ names       -> names
        _                   -> []

    lhsIdent (name, _) = name

tokenize :: String -> [Token]
tokenize = Lex.lex (SLoc "" 1 1)

extractIdents :: [Token] -> [String]
extractIdents ts = [s | TIdent _ _ s <- ts]

completionCandidates :: String -> [String]
completionCandidates = extractIdents . tokenize

isIncomplete :: String -> Bool
isIncomplete s = go [] s
  where
    go st [] = not (null st)
    go st (c:cs)
      | c `elem` "([{" = go (c:st) cs
      | c `elem` ")]}" = case st of
          [] -> False
          (x:xs) -> if match x c then go xs cs else False
      | c == '"' = goString st cs
      | c == '\'' = goChar st cs
      | otherwise = go st cs

    goString st [] = True
    goString st ('\\':'"':cs) = goString st cs
    goString st ('"':cs) = go st cs
    goString st (_:cs) = goString st cs

    goChar st [] = True
    goChar st ('\\':'\'':cs) = goChar st cs
    goChar st ('\'':cs) = go st cs
    goChar st (_:cs) = goChar st cs

    match '(' ')' = True
    match '[' ']' = True
    match '{' '}' = True
    match _ _ = False
