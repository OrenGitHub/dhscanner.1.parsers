{
{-# OPTIONS -Werror=missing-fields #-}

module PyParser( parseProgram ) where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
import Ast
import PyLexer
import Location
import qualified Token

-- *******************
-- *                 *
-- * general imports *
-- *                 *
-- *******************
import Data.Maybe
import Data.Either
import Data.List ( map )
import Data.Map ( fromList )

}

-- ***********************
-- *                     *
-- * API function: parse *
-- *                     *
-- ***********************
%name parse

-- *************
-- *           *
-- * tokentype *
-- *           *
-- *************
%tokentype { AlexTokenTag }

-- *********
-- *       *
-- * monad *
-- *       *
-- *********
%monad { Alex }

-- *********
-- *       *
-- * lexer *
-- *       *
-- *********
%lexer { lexwrap } { AlexTokenTag TokenEOF _ }

-- ***************************************************
-- * Call this function when an error is encountered *
-- ***************************************************
%error { parseError }

%token 

-- ***************
-- *             *
-- * parentheses *
-- *             *
-- ***************

'('    { AlexTokenTag AlexRawToken_LPAREN _ }
')'    { AlexTokenTag AlexRawToken_RPAREN _ }
'['    { AlexTokenTag AlexRawToken_LBRACK _ }
']'    { AlexTokenTag AlexRawToken_RBRACK _ }
'{'    { AlexTokenTag AlexRawToken_LBRACE _ }
'}'    { AlexTokenTag AlexRawToken_RBRACE _ }

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************

':'    { AlexTokenTag AlexRawToken_COLON  _ }
','    { AlexTokenTag AlexRawToken_COMMA  _ }
'-'    { AlexTokenTag AlexRawToken_HYPHEN _ }

-- *********************
-- *                   *
-- * reserved keywords *
-- *                   *
-- *********************

'body'                      { AlexTokenTag AlexRawToken_BODY            _ }
'level'                     { AlexTokenTag AlexRawToken_LEVEL           _ }
'name'                      { AlexTokenTag AlexRawToken_NAME            _ }
'asname'                    { AlexTokenTag AlexRawToken_ASNAME          _ }
'names'                     { AlexTokenTag AlexRawToken_NAMES           _ }
'alias'                     { AlexTokenTag AlexRawToken_ALIAS           _ }
'Import'                    { AlexTokenTag AlexRawToken_IMPORT          _ }
'ImportFrom'                { AlexTokenTag AlexRawToken_IMPORTF         _ }
'Module'                    { AlexTokenTag AlexRawToken_MODULE          _ }
'module'                    { AlexTokenTag AlexRawToken_MODULE2         _ }

-- ************
-- *          *
-- * location *
-- *          *
-- ************

'lineno'                    { AlexTokenTag AlexRawToken_LINE            _ }
'col_offset'                { AlexTokenTag AlexRawToken_COL             _ }
'end_lineno'                { AlexTokenTag AlexRawToken_ELINE           _ }
'end_col_offset'            { AlexTokenTag AlexRawToken_ECOL            _ }

-- ***************
-- *             *
-- * expressions *
-- *             *
-- ***************

-- ***************
-- *             *
-- * expressions *
-- *             *
-- ***************

-- **************
-- *            *
-- * statements *
-- *            *
-- **************

'IfStatement'         { AlexTokenTag AlexRawToken_STMT_IF     _ }

-- *************
-- *           *
-- * operators *
-- *           *
-- *************

'<'  { AlexTokenTag AlexRawToken_OP_LT       _ }
'==' { AlexTokenTag AlexRawToken_OP_EQ       _ }
'='  { AlexTokenTag AlexRawToken_OP_ASSIGN   _ }
'*'  { AlexTokenTag AlexRawToken_OP_TIMES    _ }

-- ****************************
-- *                          *
-- * integers and identifiers *
-- *                          *
-- ****************************

INT    { AlexTokenTag (AlexRawToken_INT  i) _ }
ID     { AlexTokenTag (AlexRawToken_ID  id) _ }

-- *************************
-- *                       *
-- * grammar specification *
-- *                       *
-- *************************
%%

-- ************
-- *          *
-- * optional *
-- *          *
-- ************
opt(a): { Nothing } | a { Just $1 }

-- **********************
-- *                    *
-- * parametrized lists *
-- *                    *
-- **********************
listof(a):      a { [$1] } | a          listof(a) { $1:$2 }
commalistof(a): a { [$1] } | a ',' commalistof(a) { $1:$3 }

-- *********************
-- *                   *
-- * Ast root: program *
-- *                   *
-- *********************
program: 'Module' '(' stmts ')'
{
    Ast.Root
    {
        Ast.filename = "DDD",
        decs = [],
        stmts = []
    }
}

-- *********
-- *       *
-- * stmts *
-- *       *
-- *********
stmts: 'body' '=' '[' commalistof(stmt) ']' { $4 }

-- ********
-- *      *
-- * stmt *
-- *      *
-- ********
stmt:
stmt_import      { $1 } |
stmt_import_from { $1 }

-- ***************
-- *             *
-- * stmt_import *
-- *             *
-- ***************
stmt_import:
'Import'
'('
    'names' '=' '[' commalistof(alias) ']' ','
    loc
')'
{
    Nothing
}

-- ********************
-- *                  *
-- * stmt_import_from *
-- *                  *
-- ********************
stmt_import_from:
'ImportFrom'
'('
    'module' '=' ID ','
    'names' '=' '[' commalistof(alias) ']' ','
    'level' '=' INT ','
    loc
')'
{
    Nothing
}

-- *********
-- *       *
-- * alias *
-- *       *
-- *********
alias:
'alias'
'('
    'name' '=' ID ','
    opt(asname)
    loc
')'
{
    Nothing
}

-- **********
-- *        *
-- * asname *
-- *        *
-- **********
asname: 'asname' '=' ID ',' { Nothing }

-- ************
-- *          *
-- * location *
-- *          *
-- ************
loc:
'lineno' '=' INT ','
'col_offset' '=' INT ','
'end_lineno' '=' INT ','
'end_col_offset' '=' INT
{
    Location
    {
        Location.filename = "",
        lineStart = tokIntValue $3,
        colStart = tokIntValue $7,
        lineEnd = tokIntValue $11,
        colEnd = tokIntValue $15
    }
}

{

unquote :: String -> String
unquote s = let n = length s in take (n-2) (drop 1 s)

extractParamSingleName' :: [ Token.ParamName ] -> Maybe Token.ParamName
extractParamSingleName' ps = case ps of { [p] -> Just p; _ -> Nothing }
 
extractParamSingleName :: [ Either Token.ParamName Token.NominalTy ] -> Maybe Token.ParamName
extractParamSingleName = extractParamSingleName' . lefts  

extractParamNominalType' :: [ Token.NominalTy ] -> Maybe Token.NominalTy
extractParamNominalType' ts = case ts of { [t] -> Just t; _ -> Nothing }
 
extractParamNominalType :: [ Either Token.ParamName Token.NominalTy ] -> Maybe Token.NominalTy
extractParamNominalType = extractParamNominalType' . rights 

paramify :: [ Either Token.ParamName Token.NominalTy ] -> Location -> Maybe Ast.Param
paramify attrs l = let
    name = extractParamSingleName attrs
    nominalType = extractParamNominalType attrs
    in case (name, nominalType) of { (Just n, Just t) -> Just $ Ast.Param n t 0; _ -> Nothing }

getFuncNameAttr :: [ Either (Either Token.FuncName [ Ast.Param ] ) (Either Token.NominalTy [ Ast.Stmt ] ) ] -> Maybe Token.FuncName
getFuncNameAttr = undefined

getFuncReturnType :: [ Either (Either Token.FuncName [ Ast.Param ] ) (Either Token.NominalTy [ Ast.Stmt ] ) ] -> Maybe Token.NominalTy
getFuncReturnType = undefined

getFuncBody :: [ Either (Either Token.FuncName [ Ast.Param ] ) (Either Token.NominalTy [ Ast.Stmt ] ) ] -> Maybe [ Ast.Stmt ]
getFuncBody = undefined

getFuncParams :: [ Either (Either Token.FuncName [ Ast.Param ] ) (Either Token.NominalTy [ Ast.Stmt ] ) ] -> Maybe [ Ast.Param ]
getFuncParams = undefined

-- getFilename :: (Either Location Ast.Dec) -> FilePath
-- getFilename x = case x of { Left l -> Location.filename l; Right dec -> Location.filename $ Ast.locationDec dec }

-- add the /real/ serial index of the param
-- the parser just puts an arbitrary value
-- there because it lacks context
enumerateParams :: (Word,[Param]) -> [Param]
enumerateParams (_,[    ]) = []
enumerateParams (i,(p:ps)) =
    let
        n = (paramName        p)
        t = (paramNominalType p)
        head = Param { paramName = n, paramNominalType = t, paramSerialIdx = i }
        tail = (enumerateParams (i+1,ps))
    in
        head:tail

-- ***********
-- *         *
-- * lexwrap *
-- *         *
-- ***********
lexwrap :: (AlexTokenTag -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

-- **************
-- *            *
-- * parseError *
-- *            *
-- **************
parseError :: AlexTokenTag -> Alex a
parseError t = alexError' (tokenLoc t)

-- ****************
-- *              *
-- * parseProgram *
-- *              *
-- ****************
parseProgram :: FilePath -> String -> Either String Ast.Root
parseProgram = runAlex' parse
}

