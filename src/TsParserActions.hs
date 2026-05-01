module TsParserActions where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
import Ast
import Location
import qualified Token
import qualified Common

-- *******************
-- *                 *
-- * general imports *
-- *                 *
-- *******************
import Data.Maybe ( fromMaybe )
import Data.List ( map, stripPrefix, isPrefixOf )

-- ***********
-- *         *
-- * stmt if *
-- *         *
-- ***********
stmtIf :: Location -> Ast.Exp -> [Ast.Stmt] -> Maybe [Ast.Stmt] -> Ast.Stmt
stmtIf loc cond body elsePart = Ast.StmtIf $ Ast.StmtIfContent
    {
        Ast.stmtIfCond = cond,
        Ast.stmtIfBody = body,
        Ast.stmtElseBody = fromMaybe [] elsePart,
        Ast.stmtIfLocation = loc
    }

-- ************
-- *          *
-- * exp call *
-- *          *
-- ************
expCall :: Location -> Ast.Exp -> [Ast.Exp] -> Ast.Exp
expCall loc funcExp callArgs = Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = funcExp,
        Ast.args = callArgs,
        Ast.expCallLocation = loc
    }

-- ***************
-- *             *
-- * stmt import *
-- *             *
-- ***************
normalizeImportPath :: String -> String
normalizeImportPath i = case Data.List.stripPrefix "./" i of { Just p -> p; _ -> i }

resolvePathAlias :: [ Common.PathMapping ] -> String -> String
resolvePathAlias [] imported = imported
resolvePathAlias (m:rest) imported =
    case Data.List.stripPrefix (Common.path_mapping_from m) imported of
        Just suffix -> (Common.path_mapping_to m) ++ suffix
        Nothing -> resolvePathAlias rest imported

isKnownFilename :: [ String ] -> String -> Bool
isKnownFilename filenames imported = (imported ++ ".ts") `elem` filenames

resolveImportSource :: Common.AdditionalRepoInfo -> String -> Ast.ImportSource
resolveImportSource repoInfo imported = let
    aliases = Common.path_mappings repoInfo
    knownFilenames = Common.filenames repoInfo
    importedNorm = normalizeImportPath imported
    resolved = resolvePathAlias aliases importedNorm
    in resolveImportSource' knownFilenames importedNorm resolved

resolveImportSource' :: [ String ] -> String -> String -> Ast.ImportSource
resolveImportSource' knownFilenames importedNorm resolved = case Data.List.isPrefixOf "@" importedNorm of
    False -> if isKnownFilename knownFilenames importedNorm
        then ImportLocal (ImportLocalFile (importedNorm ++ ".ts"))
        else ImportThirdParty (ImportThirdPartyContent importedNorm)
    True -> if (resolved /= importedNorm) && (isKnownFilename knownFilenames resolved)
        then ImportLocal (ImportLocalFile (resolved ++ ".ts"))
        else ImportThirdParty (ImportThirdPartyContent importedNorm)

importify' :: Common.AdditionalRepoInfo -> Token.ConstStr -> Token.Named -> Ast.Stmt
importify' repoInfo importSource importFromSource = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = resolveImportSource repoInfo (Token.constStrValue importSource),
    Ast.stmtImportSpecific = Just (Ast.ImportSpecific (Token.content importFromSource)),
    Ast.stmtImportAlias = Nothing,
    Ast.stmtImportLocation = Token.location importFromSource
}

importify :: Common.AdditionalRepoInfo -> Token.ConstStr -> [ Token.Named ] -> [ Ast.Stmt ]
importify repoInfo = Data.List.map . (importify' repoInfo)

stmtImport :: Common.AdditionalRepoInfo -> Location -> Maybe [Token.Named] -> Token.ConstStr -> Ast.Stmt
stmtImport repoInfo loc maybeImports importSource = Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = importify repoInfo importSource (fromMaybe [] maybeImports),
        Ast.stmtBlockLocation = loc
    }
