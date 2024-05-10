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

'id'                        { AlexTokenTag AlexRawToken_KWID            _ }
'op'                        { AlexTokenTag AlexRawToken_OPERATOR        _ }
'operand'                   { AlexTokenTag AlexRawToken_OPERAND         _ }
'Return'                    { AlexTokenTag AlexRawToken_STMT_RETURN     _ }
'Constant'                  { AlexTokenTag AlexRawToken_EXPR_CONST      _ }
'Not'                       { AlexTokenTag AlexRawToken_NOT             _ }
'Add'                       { AlexTokenTag AlexRawToken_ADD             _ }
'ctx'                       { AlexTokenTag AlexRawToken_CTX             _ }
'kwonlyargs'                { AlexTokenTag AlexRawToken_ARGS4           _ }
'posonlyargs'               { AlexTokenTag AlexRawToken_ARGS3           _ }
'arguments'                 { AlexTokenTag AlexRawToken_ARGS2           _ }
'arg'                       { AlexTokenTag AlexRawToken_ARG             _ }
'args'                      { AlexTokenTag AlexRawToken_ARGS            _ }
'attr'                      { AlexTokenTag AlexRawToken_ATTR            _ }
'Attribute'                 { AlexTokenTag AlexRawToken_ATTR2           _ }
'func'                      { AlexTokenTag AlexRawToken_FUNC            _ }
'body'                      { AlexTokenTag AlexRawToken_BODY            _ }
'test'                      { AlexTokenTag AlexRawToken_TEST            _ }
'Name'                      { AlexTokenTag AlexRawToken_NAME2           _ }
'Call'                      { AlexTokenTag AlexRawToken_CALL            _ }
'Expr'                      { AlexTokenTag AlexRawToken_STMT_EXPR       _ }
'level'                     { AlexTokenTag AlexRawToken_LEVEL           _ }
'value'                     { AlexTokenTag AlexRawToken_VALUE           _ }
'values'                    { AlexTokenTag AlexRawToken_VALUES          _ }
'name'                      { AlexTokenTag AlexRawToken_NAME            _ }
'asname'                    { AlexTokenTag AlexRawToken_ASNAME          _ }
'orelse'                    { AlexTokenTag AlexRawToken_ORELSE          _ }
'defaults'                  { AlexTokenTag AlexRawToken_DEFAULTS        _ }
'kw_defaults'               { AlexTokenTag AlexRawToken_KW_DEFAULTS     _ }
'target'                    { AlexTokenTag AlexRawToken_TARGET          _ }
'targets'                   { AlexTokenTag AlexRawToken_TARGETS         _ }
'names'                     { AlexTokenTag AlexRawToken_NAMES           _ }
'alias'                     { AlexTokenTag AlexRawToken_ALIAS           _ }
'keywords'                  { AlexTokenTag AlexRawToken_KEYWORDS        _ }
'Import'                    { AlexTokenTag AlexRawToken_IMPORT          _ }
'conversion'                { AlexTokenTag AlexRawToken_CONVERSION      _ }
'JoinedStr'                 { AlexTokenTag AlexRawToken_FSTRING         _ }
'ImportFrom'                { AlexTokenTag AlexRawToken_IMPORTF         _ }
'FormattedValue'            { AlexTokenTag AlexRawToken_FORMATTED_VAL   _ }
'Load'                      { AlexTokenTag AlexRawToken_LOAD            _ }
'Store'                     { AlexTokenTag AlexRawToken_STORE           _ }
'Assign'                    { AlexTokenTag AlexRawToken_ASSIGN          _ }
'AugAssign'                 { AlexTokenTag AlexRawToken_ASSIGN2         _ }
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
'UnaryOp'                   { AlexTokenTag AlexRawToken_EXPR_UNOP       _ }


-- **************
-- *            *
-- * statements *
-- *            *
-- **************

'If'                        { AlexTokenTag AlexRawToken_STMT_IF         _ }
'FunctionDef'               { AlexTokenTag AlexRawToken_STMT_FUNCTION   _ }

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
program: 'Module' '(' 'body' '=' stmts ')'
{
    Ast.Root
    {
        Ast.filename = "DDD",
        decs = [],
        stmts = []
    }
}

-- ********
-- *      *
-- * args *
-- *      *
-- ********
args:
'[' ']'                  { Nothing } |
'[' commalistof(arg) ']' { Nothing }

-- *******
-- *     *
-- * arg *
-- *     *
-- *******
arg: exp { $1 }

-- ******
-- *    *
-- * op *
-- *    *
-- ******
op:
'Not' '(' ')' { Nothing } |
'Add' '(' ')' { Nothing }

-- ************
-- *          *
-- * exp_unop *
-- *          *
-- ************
exp_unop:
'UnaryOp'
'('
    'op' '=' op ','
    'operand' '=' exp ','
    loc
')'
{
    Nothing
}

-- **************
-- *            *
-- * var_simple *
-- *            *
-- **************
var_simple: name { $1 }

-- *************
-- *           *
-- * var_field *
-- *           *
-- *************
var_field:
'Attribute'
'('
    'value' '=' exp_var ','
    'attr' '=' ID ','
    'ctx' '=' ctx ','
    loc
')'
{
    Nothing
}

-- *******
-- *     *
-- * var *
-- *     *
-- *******
var:
var_simple { $1 } |
var_field  { $1 }

-- ***********
-- *         *
-- * exp_var *
-- *         *
-- ***********
exp_var: var { $1 }

-- ***********
-- *         *
-- * exp_str *
-- *         *
-- ***********
exp_str:
'Constant'
'('
    'value' '=' ID ','
    loc
')'
{
    Nothing
}

-- **************
-- *            *
-- * conversion *
-- *            *
-- **************
conversion: '-' INT { Nothing }

-- *******************
-- *                 *
-- * formatted_value *
-- *                 *
-- *******************
formatted_value:
'FormattedValue'
'('
    'value' '=' exp ','
    'conversion' '=' conversion ','
    loc
')'
{
    Nothing
}

-- ****************
-- *              *
-- * fstring elem *
-- *              *
-- ****************
fstring_elem:
exp_str         { $1 } |
formatted_value { $1 }

-- ***************
-- *             *
-- * exp_fstring *
-- *             *
-- ***************
exp_fstring:
'JoinedStr'
'('
    'values' '=' '[' commalistof(fstring_elem) ']' ','
    loc
')'
{
    Nothing
}

-- *******
-- *     *
-- * exp *
-- *     *
-- *******
exp:
exp_str     { $1 } |
exp_var     { $1 } |
exp_unop    { $1 } |
exp_call    { $1 } |
exp_fstring { $1 }

-- ************
-- *          *
-- * keywords *
-- *          *
-- ************
keywords: '[' ']' { Nothing }

-- ************
-- *          *
-- * exp_call *
-- *          *
-- ************
exp_call:
'Call'
'('
    'func' '=' exp ','
    'args' '=' args ','
    'keywords' '=' keywords ','
    loc
')'
{
    Nothing
}

-- *********
-- *       *
-- * stmts *
-- *       *
-- *********
stmts: '[' ']' { [] } | '[' commalistof(stmt) ']' { $2 }

-- ***********
-- *         *
-- * stmt_if *
-- *         *
-- ***********
stmt_if:
'If'
'('
    'test' '=' exp ','
    'body' '=' stmts ','
    'orelse' '=' stmts ','
    loc 
')'
{
    Nothing
}

-- *************
-- *           *
-- * stmt_call *
-- *           *
-- *************
stmt_call:
'Expr'
'('
    'value' '=' exp_call ','
    loc
')'
{
    $5
}

-- ***************
-- *             *
-- * stmt_return *
-- *             *
-- ***************
stmt_return:
'Return' '(' loc ')'  { Nothing }

-- *******************
-- *                 *
-- * stmt_aug_assign *
-- *                 *
-- *******************
stmt_aug_assign:
'AugAssign'
'('
    'target' '=' var ','
    'op' '=' op ','
    'value' '=' exp ','
    loc
')'
{
    Nothing
}

-- ********
-- *      *
-- * stmt *
-- *      *
-- ********
stmt:
stmt_if          { $1 } |
stmt_call        { $1 } |
stmt_import      { $1 } |
stmt_return      { $1 } |
stmt_assign      { $1 } |
stmt_function    { $1 } |
stmt_aug_assign  { $1 } |
stmt_import_from { $1 }

-- *********
-- *       *
-- * param *
-- *       *
-- *********
param:
'arg'
'('
    'arg' '=' ID ',' loc
')'
{
    Nothing
}

-- **********
-- *        *
-- * params *
-- *        *
-- **********
params:
'arguments'
'('
    'posonlyargs' '=' '[' ']' ','
    'args' '=' '[' commalistof(param) ']' ','
    'kwonlyargs' '=' '[' ']' ','
    'kw_defaults' '=' '[' ']' ','
    'defaults' '=' '[' ']'
')'
{
    Nothing
}

-- *****************
-- *               *
-- * stmt_function *
-- *               *
-- *****************
stmt_function:
'FunctionDef'
'('
    'name' '=' ID ','
    'args' '=' params ','
    'body' '=' stmts
')'
{
    Nothing
}

-- *******
-- *     *
-- * ctx *
-- *     *
-- *******
ctx:
'Store' '(' ')' { Nothing } |
'Load'  '(' ')' { Nothing }

-- ********
-- *      *
-- * name *
-- *      *
-- ********
name:
'Name'
'('
    'id' '=' ID ','
    'ctx' '=' ctx ','
    loc
')'
{
    Nothing
}

-- ***************
-- *             *
-- * stmt_assign *
-- *             *
-- ***************
stmt_assign:
'Assign'
'('
    'targets' '=' '[' listof(name) ']' ','
    'value' '=' exp ','
    loc
')'
{
    Nothing
}

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

