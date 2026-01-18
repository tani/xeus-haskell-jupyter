{-# LANGUAGE ForeignFunctionInterface #-}

module Repl (
  mhsReplNew,
  mhsReplFree,
  mhsReplDefine,
  mhsReplRun,
  mhsReplFreeCString,
  mhsReplCanParseDefinition,
  mhsReplCanParseExpression,
  mhsReplCompletionCandidates,
  mhsReplInspect,
  mhsReplIsComplete
) where

import qualified Prelude ()
import MHSPrelude

import System.Environment (lookupEnv)
import System.IO (putStrLn)
import Control.Exception (try, SomeException, displayException)
import Data.IORef
import Data.List (foldl', intercalate, nub, reverse, dropWhile)
import Data.Maybe (isJust, fromMaybe, maybeToList)
import Data.Text (pack, unpack)

import Foreign.C.String (CString, peekCString, peekCStringLen, newCString)
import Foreign.C.Types (CInt, CSize)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.StablePtr (StablePtr, newStablePtr, freeStablePtr, deRefStablePtr)
import Foreign.Storable (poke)
import MicroHs.Builtin (builtinMdl)
import MicroHs.Compile (Cache, compileModuleP, compileToCombinators, emptyCache)
import MicroHs.CompileCache (cachedModules)
import MicroHs.SymTab (SymTab, Entry(..), stLookup, stEmpty)
import MicroHs.Desugar (LDef)
import MicroHs.Exp (Exp(Var))
import MicroHs.Expr (EModule(..), EDef(..), ImpType(..), patVars, showExpr)
import MicroHs.Flags (Flags, defaultFlags, paths)
import MicroHs.Ident (Ident, mkIdent, qualIdent, unQualIdent, SLoc(..))
import qualified MicroHs.IdentMap as IMap
import MicroHs.Parse (parse, pExprTop, pTopModule)
import MicroHs.StateIO (runStateIO)
import MicroHs.TypeCheck (TModule(..), tBindingsOf, Symbols)
import MicroHs.Translate (TranslateMap, translateMap, translateWithMap)
import Unsafe.Coerce (unsafeCoerce)
import MicroHs.Lex

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

c_OK, c_ERR :: CInt
c_OK  = 0
c_ERR = -1

writeErrorCString :: Ptr CString -> String -> IO ()
writeErrorCString errPtr msg =
  if errPtr == nullPtr then pure ()
  else newCString msg >>= poke errPtr

--------------------------------------------------------------------------------
-- Context
--------------------------------------------------------------------------------

type ReplHandle = StablePtr (IORef ReplCtx)

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

moduleSourceWith :: ReplCtx -> String -> String
moduleSourceWith ctx extra = buildModule (currentDefsSource ctx ++ extra)

moduleFromDefs :: [StoredDef] -> String
moduleFromDefs defs = buildModule (defsSource defs)

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

--------------------------------------------------------------------------------
-- Error
--------------------------------------------------------------------------------

data ReplError
  = ReplParseError String
  | ReplCompileError String
  | ReplRuntimeError String
  deriving (Eq, Show)

prettyReplError :: ReplError -> String
prettyReplError e =
  case e of
    ReplParseError s   -> "Parse error: "   ++ s
    ReplCompileError s -> "Compile error: " ++ s
    ReplRuntimeError s -> "Runtime error: " ++ s

--------------------------------------------------------------------------------
-- FFI exports
--------------------------------------------------------------------------------

foreign export ccall "mhs_repl_new"         mhsReplNew          :: CString -> CSize -> IO ReplHandle
foreign export ccall "mhs_repl_free"        mhsReplFree         :: ReplHandle -> IO ()
foreign export ccall "mhs_repl_define"      mhsReplDefine       :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
foreign export ccall "mhs_repl_run"         mhsReplRun          :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
foreign export ccall "mhs_repl_execute"     mhsReplExecute      :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
foreign export ccall "mhs_repl_is_complete" mhsReplIsComplete :: ReplHandle -> CString -> CSize -> IO CString
foreign export ccall "mhs_repl_free_cstr"   mhsReplFreeCString  :: CString -> IO ()
foreign export ccall "mhs_repl_can_parse_definition" mhsReplCanParseDefinition :: CString -> CSize -> IO CInt
foreign export ccall "mhs_repl_can_parse_expression" mhsReplCanParseExpression :: CString -> CSize -> IO CInt
foreign export ccall "mhs_repl_completion_candidates" mhsReplCompletionCandidates :: ReplHandle -> IO ()
foreign export ccall "mhs_repl_inspect" mhsReplInspect :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt

mhsReplNew :: CString -> CSize -> IO ReplHandle
mhsReplNew cstr csize = do
  str <- peekSource cstr csize
  ctx <- initialCtx str
  ref <- newIORef ctx
  newStablePtr ref

mhsReplFree :: ReplHandle -> IO ()
mhsReplFree = freeStablePtr

mhsReplFreeCString :: CString -> IO ()
mhsReplFreeCString = free

--------------------------------------------------------------------------------
-- Unified runner
--------------------------------------------------------------------------------

runReplAction
  :: (ReplCtx -> String -> IO (Either ReplError ReplCtx))
  -> ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
runReplAction act h srcPtr srcLen errPtr = do
  ref <- deRefStablePtr h
  src <- peekSource srcPtr srcLen
  ctx <- readIORef ref
  result <- try (act ctx src) :: IO (Either SomeException (Either ReplError ReplCtx))
  let normalized = case result of
        Left ex -> Left (ReplRuntimeError (displayException ex))
        Right r -> r
  case normalized of
    Left e -> writeErrorCString errPtr (prettyReplError e) >> pure c_ERR
    Right ctx' -> writeIORef ref ctx' >> pure c_OK

--------------------------------------------------------------------------------
-- Compilation / execution
--------------------------------------------------------------------------------

moduleHeader :: String
moduleHeader = unlines
  [ "module Inline where"
  , "import Prelude"
  , "import System.IO.PrintOrRun"
  , "import Data.Typeable"
  , "import Numeric"
  ]

runResultName :: String
runResultName = "runResult"

runResultIdent :: Ident
runResultIdent = mkIdent runResultName

buildModule :: String -> String
buildModule defs = moduleHeader ++ defs

indent :: String -> String
indent = unlines . map ("  " ++) . lines

runBlock :: String -> String
runBlock stmt = unlines
  [ "_myPrintOrRun :: (Typeable a, PrintOrRun a) => a -> IO ()"
  , "_myPrintOrRun x ="
  , "  case cast x of"
  , "    Just (io :: IO ()) -> io"
  , "    Nothing -> do"
  , "      putStr \"\\x02\\&application/vnd.xeus.haskell.value\\x1F\""
  , "      _printOrRun x"
  , "      putStr \"\\x03\""
  , ""
  , runResultName ++ " :: IO ()"
  , runResultName ++ " = _myPrintOrRun ("
  , indent stmt
  , "  )"
  ]

ensureTrailingNewline :: String -> String
ensureTrailingNewline s
  | null s         = "\n"
  | last s == '\n' = s
  | otherwise      = s ++ "\n"


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

--------------------------------------------------------------------------------
-- REPL logic
--------------------------------------------------------------------------------

replDefine :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replDefine ctx snippet = do
  let snippet' = ensureTrailingNewline snippet
  case appendDefinition ctx snippet' of
    Left err -> pure (Left err)
    Right defsWithNew -> do
      cm <- compileModule ctx (moduleFromDefs defsWithNew)
      case cm of
        Left err'          -> pure (Left err')
        Right (_, cache', syms') -> pure (Right ctx{ rcDefs = defsWithNew, rcCache = cache', rcSyms = syms' })

replRun :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replRun ctx stmt = do
  let block = runBlock stmt
      src = moduleSourceWith ctx block
  cm <- compileModule ctx src
  case cm of
    Left err -> pure (Left err)
    Right (cmdl, cache', syms') -> do
      r <- runAction cache' cmdl runResultIdent
      case r of
        Left e  -> pure (Left e)
        Right _ -> pure (Right ctx{ rcCache = cache', rcSyms = syms' })

--------------------------------------------------------------------------------
-- Public FFI API
--------------------------------------------------------------------------------

mhsReplDefine :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
mhsReplDefine = runReplAction replDefine

mhsReplRun :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
mhsReplRun = runReplAction replRun

mhsReplExecute :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
mhsReplExecute = runReplAction replExecute
mhsReplIsComplete :: ReplHandle -> CString -> CSize -> IO CString
mhsReplIsComplete h srcPtr srcLen = do
  ref <- deRefStablePtr h
  src <- peekSource srcPtr srcLen
  ctx <- readIORef ref
  status <- replIsComplete ctx src
  newCString status

replIsComplete :: ReplCtx -> String -> IO String
replIsComplete ctx snippet = do
  if all isws snippet then pure "complete" else do
    let ls = lines (ensureTrailingNewline snippet)
        go n
          | n < 0 = pure "invalid"
          | otherwise = do
              let (defLines, runLines) = splitAt n ls
                  defPart = unlines defLines
                  runPart = unlines (dropWhileEnd allwsLine runLines)
                  candidateDefs = currentDefsSource ctx ++ defPart
              if canParseDefinition candidateDefs
                then if all allwsLine runLines
                     then pure "complete"
                     else if canParseExpression runPart || canParseExpression ("do\n" ++ indent runPart)
                          then pure "complete"
                          else go (n - 1)
                else go (n - 1)
    res <- go (length ls)
    if res == "invalid" && isIncomplete snippet
      then pure "incomplete"
      else pure res

mhsReplInspect :: ReplHandle -> CString -> CSize -> Ptr CString -> IO CInt
mhsReplInspect h srcPtr srcLen resPtr = do
  ref <- deRefStablePtr h
  name <- peekSource srcPtr srcLen
  ctx <- readIORef ref
  result <- replInspect ctx name
  case result of
    Left e -> writeErrorCString resPtr (prettyReplError e) >> pure c_ERR
    Right info -> newCString info >>= poke resPtr >> pure c_OK

replInspect :: ReplCtx -> String -> IO (Either ReplError String)
replInspect ctx name = do
  let ident = mkIdent name
      (typeTable, valueTable) = rcSyms ctx
  case stLookup "value" ident valueTable of
    Right (Entry _ sigma) -> return (Right $ name ++ " :: " ++ showExpr sigma)
    Left _ -> case stLookup "type" ident typeTable of
      Right (Entry _ kind) -> return (Right $ name ++ " :: " ++ showExpr kind)
      Left _ -> return (Left $ ReplRuntimeError $ "Identifier not found: " ++ name)

isws :: Char -> Bool
isws c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

allwsLine :: String -> Bool
allwsLine = all isws

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse

replExecute :: ReplCtx -> String -> IO (Either ReplError ReplCtx)
replExecute ctx snippet = do
  let ls = lines (ensureTrailingNewline snippet)
      go n
        | n < 0 = pure $ Left (ReplParseError "unable to parse snippet")
        | otherwise = do
            let (defLines, runLines) = splitAt n ls
                defPart = unlines defLines
                runPart = unlines (dropWhileEnd allwsLine runLines)
                candidateDefs = currentDefsSource ctx ++ defPart
            if canParseDefinition candidateDefs
              then if all allwsLine runLines
                   then replDefine ctx defPart
                   else if canParseExpression runPart
                        then do
                          eCtx' <- replDefine ctx defPart
                          case eCtx' of
                            Left err -> pure (Left err)
                            Right ctx' -> replRun ctx' runPart
                        else
                          let runBlock = "do\n" ++ indent runPart
                          in if canParseExpression runBlock
                             then do
                               eCtx' <- replDefine ctx defPart
                               case eCtx' of
                                 Left err -> pure (Left err)
                                 Right ctx' -> replRun ctx' runBlock
                             else go (n - 1)
              else go (n - 1)
  go (length ls)

--------------------------------------------------------------------------------
-- CString utilities
--------------------------------------------------------------------------------

peekSource :: CString -> CSize -> IO String
peekSource ptr len
  | ptr == nullPtr = pure ""
  | len == 0       = peekCString ptr
  | otherwise      = peekCStringLen (ptr, fromIntegral len)

--------------------------------------------------------------------------------
-- Parser helpers
--------------------------------------------------------------------------------

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

mhsReplCanParseDefinition :: CString -> CSize -> IO CInt
mhsReplCanParseDefinition ptr len = do
  code <- peekSource ptr len
  pure $ if canParseDefinition code then 1 else 0

mhsReplCanParseExpression :: CString -> CSize -> IO CInt
mhsReplCanParseExpression ptr len = do
  code <- peekSource ptr len
  pure $ if canParseExpression code then 1 else 0

tokenize :: String -> [Token]
tokenize = lex (SLoc "" 1 1)

extractIdents :: [Token] -> [String]
extractIdents ts = [s | TIdent _ _ s <- ts]

completionCandidates :: String -> [String]
completionCandidates = extractIdents . tokenize

reservedIds :: [String]
reservedIds =
  [ "case", "class", "data", "default", "deriving", "do", "else"
  , "foreign", "if", "import", "in", "infix", "infixl", "infixr"
  , "instance", "let", "module", "newtype", "of", "then", "type"
  , "where", "_"
  ]

mhsReplCompletionCandidates :: ReplHandle -> IO ()
mhsReplCompletionCandidates h = do
  ref <- deRefStablePtr h
  ctx <- readIORef ref
  let source = currentDefsSource ctx
  let localIdents = completionCandidates source
  let allCandidates = nub (reservedIds ++ localIdents)
  mapM_ putStrLn allCandidates

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
