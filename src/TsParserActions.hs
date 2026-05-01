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

-- *****************
-- *               *
-- * stmt function *
-- *               *
-- *****************
stmtFunction :: Location -> Token.Named -> [Ast.Param] -> Maybe [Ast.Stmt] -> Ast.Stmt
stmtFunction loc fname params body = Ast.StmtFunc $ Ast.StmtFuncContent
    {
        Ast.stmtFuncReturnType = Just (varify (Token.Named "any" loc)),
        Ast.stmtFuncName = Token.FuncName fname,
        Ast.stmtFuncParams = params,
        Ast.stmtFuncBody = fromMaybe [] body,
        Ast.stmtFuncAnnotations = [],
        Ast.stmtFuncLocation = loc
    }

-- ***************
-- *             *
-- * stmt decvar *
-- *             *
-- ***************
varify :: Token.Named -> Ast.Var
varify = Ast.VarSimple . Ast.VarSimpleContent . Token.VarName

assignify :: [Ast.Var] -> Ast.Exp -> [Ast.Stmt]
assignify vars e = Data.List.map (\v -> Ast.StmtAssign (Ast.StmtAssignContent v e)) vars

normalizeVardec :: Token.VarName -> Ast.Exp -> Ast.Stmt
normalizeVardec v (Ast.ExpLambda lambda) = Ast.StmtFunc $ Ast.StmtFuncContent
    {
        Ast.stmtFuncReturnType = Just (varify (Token.Named "any" (Token.getVarNameLocation v))),
        Ast.stmtFuncName = Token.FuncName (Token.getVarNameToken v),
        Ast.stmtFuncParams = Ast.expLambdaParams lambda,
        Ast.stmtFuncBody = Ast.expLambdaBody lambda,
        Ast.stmtFuncAnnotations = [],
        Ast.stmtFuncLocation = Ast.expLambdaLocation lambda
    }
normalizeVardec v initValue = Ast.StmtVardec $ Ast.StmtVardecContent
    {
        Ast.stmtVardecName = v,
        Ast.stmtVardecNominalType = Just (varify (Token.Named "any" (Token.getVarNameLocation v))),
        Ast.stmtVardecInitValue = Just initValue,
        Ast.stmtVardecLocation = Token.getVarNameLocation v
    }

stmtDecvar :: Location -> [Ast.Var] -> Ast.Exp -> Ast.Stmt
stmtDecvar _   [Ast.VarSimple (Ast.VarSimpleContent v)] initExp = normalizeVardec v initExp
stmtDecvar loc vars                                     initExp = Ast.StmtBlock $ Ast.StmtBlockContent (assignify vars initExp) loc

-- *************
-- *           *
-- * exp binop *
-- *           *
-- *************
expBinop :: Location -> Ast.Exp -> Ast.Exp -> Ast.Exp
expBinop loc left right = Ast.ExpBinop $ Ast.ExpBinopContent
    {
        Ast.expBinopLeft = left,
        Ast.expBinopRight = right,
        Ast.expBinopOperator = Ast.PLUS,
        Ast.expBinopLocation = loc
    }

-- ************
-- *          *
-- * exp bool *
-- *          *
-- ************
expBool :: Bool -> Location -> Ast.Exp
expBool value loc = Ast.ExpBool $ Ast.ExpBoolContent $ Token.ConstBool
    {
        Token.constBoolValue = value,
        Token.constBoolLocation = loc
    }

-- ***************
-- *             *
-- * stmt return *
-- *             *
-- ***************
stmtReturn :: Location -> Maybe Ast.Exp -> Ast.Stmt
stmtReturn loc value = Ast.StmtReturn $ Ast.StmtReturnContent
    {
        Ast.stmtReturnValue = value,
        Ast.stmtReturnLocation = loc
    }

-- ******************
-- *                *
-- * exp arrow func *
-- *                *
-- ******************
expArrowFunction :: Location -> [Ast.Param] -> [Ast.Stmt] -> Ast.Exp
expArrowFunction loc params body = Ast.ExpLambda $ Ast.ExpLambdaContent
    {
        Ast.expLambdaParams = params,
        Ast.expLambdaBody = body,
        Ast.expLambdaLocation = loc
    }

-- ***************
-- *             *
-- * stmt assign *
-- *             *
-- ***************
stmtAssign :: Ast.Var -> Ast.Exp -> Ast.Stmt
stmtAssign lhs rhs = Ast.StmtAssign $ Ast.StmtAssignContent
    {
        Ast.stmtAssignLhs = lhs,
        Ast.stmtAssignRhs = rhs
    }

-- ************
-- *          *
-- * exp null *
-- *          *
-- ************
expNull :: Location -> Ast.Exp
expNull loc = Ast.ExpNull $ Ast.ExpNullContent
    {
        Ast.expNullValue = Token.ConstNull
            {
                Token.constNullLocation = loc
            }
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
