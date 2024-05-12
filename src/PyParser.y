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
'left'                      { AlexTokenTag AlexRawToken_LEFT            _ }
'op'                        { AlexTokenTag AlexRawToken_OPERATOR        _ }
'ops'                       { AlexTokenTag AlexRawToken_OPERATOR2       _ }
'decorator_list'            { AlexTokenTag AlexRawToken_DECORATORS      _ }
'type_params'               { AlexTokenTag AlexRawToken_TYPE_PARAMS     _ }
'type_ignores'              { AlexTokenTag AlexRawToken_TYPE_IGNORES    _ }
'comparators'               { AlexTokenTag AlexRawToken_COMPARE2        _ }
'BoolOp'                    { AlexTokenTag AlexRawToken_COMPARE3        _ }
'Compare'                   { AlexTokenTag AlexRawToken_COMPARE         _ }
'operand'                   { AlexTokenTag AlexRawToken_OPERAND         _ }
'Return'                    { AlexTokenTag AlexRawToken_STMT_RETURN     _ }
'With'                      { AlexTokenTag AlexRawToken_WITH            _ }
'withitem'                  { AlexTokenTag AlexRawToken_WITH2           _ }
'context_expr'              { AlexTokenTag AlexRawToken_CTX_MANAGER     _ }
'items'                     { AlexTokenTag AlexRawToken_ITEMS           _ }
'List'                      { AlexTokenTag AlexRawToken_LIST            _ }
'elts'                      { AlexTokenTag AlexRawToken_ELTS            _ }
'False'                     { AlexTokenTag AlexRawToken_FALSE           _ }
'True'                      { AlexTokenTag AlexRawToken_TRUE            _ }
'Constant'                  { AlexTokenTag AlexRawToken_EXPR_CONST      _ }
'Not'                       { AlexTokenTag AlexRawToken_NOT             _ }
'Add'                       { AlexTokenTag AlexRawToken_ADD             _ }
'Eq'                        { AlexTokenTag AlexRawToken_EQ              _ }
'Or'                        { AlexTokenTag AlexRawToken_OR              _ }
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
'keyword'                   { AlexTokenTag AlexRawToken_KEYWORD         _ }
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
program:
'Module'
'('
    'body' '=' stmts ','
    'type_ignores' '=' '[' ']'
')'
{
    Ast.Root
    {
        Ast.filename = "DDD",
        decs = [],
        stmts = $5
    }
}

-- ********
-- *      *
-- * args *
-- *      *
-- ********
args:
'[' ']'                  { [] } |
'[' commalistof(arg) ']' { $2 }

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
'Add' '(' ')' { Nothing } |
'Eq'  '(' ')' { Nothing } |
'Or'  '(' ')' { Nothing }

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
    $9
}

-- **************
-- *            *
-- * var_simple *
-- *            *
-- **************
var_simple:
name
{
    Ast.VarSimple $ Ast.VarSimpleContent
    {
        Ast.varName = Token.VarName $1
    }
}

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
    Ast.VarField $ Ast.VarFieldContent
    {
        Ast.varFieldLhs = $5,
        Ast.varFieldName = Token.FieldName $ Token.Named
        {
            Token.content = unquote (tokIDValue $9),
            Token.location = $15
        },
        Ast.varFieldLocation = $15
    }
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
exp_var: var { Ast.ExpVarContent $1 }

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
    Ast.ExpStr $ Ast.ExpStrContent
    {
        Ast.expStrValue = Token.ConstStr
        {
            Token.constStrValue = tokIDValue $5,
            Token.constStrLocation = $7
        }
    }
}

-- ******************
-- *                *
-- * exp_bool_false *
-- *                *
-- ******************
exp_bool_false:
'Constant'
'('
    'value' '=' 'False' ','
    loc
')'
{
    Ast.ExpBool $ Ast.ExpBoolContent
    {
        Ast.expBoolValue = Token.ConstBool
        {
            Token.constBoolValue = False,
            Token.constBoolLocation = $7
        }
    }
}

-- *****************
-- *               *
-- * exp_bool_true *
-- *               *
-- *****************
exp_bool_true:
'Constant'
'('
    'value' '=' 'True' ','
    loc
')'
{
    Ast.ExpBool $ Ast.ExpBoolContent
    {
        Ast.expBoolValue = Token.ConstBool
        {
            Token.constBoolValue = True,
            Token.constBoolLocation = $7
        }
    }
}

-- ************
-- *          *
-- * exp_bool *
-- *          *
-- ************
exp_bool:
exp_bool_true  { $1 } |
exp_bool_false { $1 }

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
    $5
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
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent
        {
            Ast.actualExpVar = Ast.VarSimple $ Ast.VarSimpleContent
            {
                Ast.varName = Token.VarName $ Token.Named
                {
                    Token.content = "fstring",
                    Token.location = $9
                }
            }
        },
        Ast.args = $6,
        Ast.expCallLocation = $9
    }
}

-- *****************
-- *               *
-- * exp_binop_cmp *
-- *               *
-- *****************
exp_binop_cmp:
'Compare'
'('
    'left' '=' exp ','
    'ops' '=' '[' op ']' ','
    'comparators' '=' '[' commalistof(exp) ']' ','
    loc
')'
{
    Ast.ExpBinop $ Ast.ExpBinopContent
    {
        Ast.expBinopLeft = $5,
        Ast.expBinopRight = case $16 of { [] -> $5; (rhs:_) -> rhs },
        Ast.expBinopOperator = Ast.PLUS, -- FIXME
        Ast.expBinopLocation = $19
    }
}

-- ******************
-- *                *
-- * exp_binop_cmp2 *
-- *                *
-- ******************
exp_binop_cmp2:
'BoolOp'
'('
    'op' '=' op ','
    'values' '=' '[' commalistof(exp) ']' ','
    loc
')'
{
    Ast.ExpBinop $ Ast.ExpBinopContent
    {
        Ast.expBinopLeft = case $10 of
        {
            [] -> dummyExp $13;
            [e] -> e;
            [lhs,rhs] -> lhs;
            (lhs:rhs:_) -> lhs
        },
        Ast.expBinopRight = case $10 of
        {
            [] -> dummyExp $13;
            [e] -> e;
            [lhs,rhs] -> rhs;
            (lhs:rhs:_) -> rhs
        },
        Ast.expBinopOperator = Ast.PLUS,
        Ast.expBinopLocation = $13
    }  
}

-- *************
-- *           *
-- * exp_binop *
-- *           *
-- *************
exp_binop:
exp_binop_cmp  { $1 } |
exp_binop_cmp2 { $1 }

-- ************
-- *          *
-- * exp_list *
-- *          *
-- ************
exp_list:
'List'
'('
    'elts' '=' '[' commalistof(exp) ']' ','
    'ctx' '=' ctx ','
    loc
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent
        {
            Ast.actualExpVar = Ast.VarSimple $ Ast.VarSimpleContent
            {
                Ast.varName = Token.VarName $ Token.Named
                {
                    Token.content = "listify",
                    Token.location = $13
                }
            }
        },
        Ast.args = $6,
        Ast.expCallLocation = $13
    }
}

-- *******
-- *     *
-- * exp *
-- *     *
-- *******
exp:
exp_str     { $1 } |
exp_var     { Ast.ExpVar $1 } |
exp_bool    { $1 } |
exp_list    { $1 } |
exp_unop    { $1 } |
exp_binop   { $1 } |
exp_call    { Ast.ExpCall $1 } |
exp_fstring { $1 }

-- ***********
-- *         *
-- * keyword *
-- *         *
-- ***********
keyword:
'keyword'
'('
    'arg' '=' ID ','
    'value' '=' exp ','
    loc
')'
{
    Nothing
}

-- ************
-- *          *
-- * keywords *
-- *          *
-- ************
keywords:
'[' ']'                      { Nothing } | 
'[' commalistof(keyword) ']' { Nothing }

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
    Ast.ExpCallContent
    {
        Ast.callee = $5,
        Ast.args = $9,
        Ast.expCallLocation = $15
    }
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
    Ast.StmtIf $ Ast.StmtIfContent
    {
        Ast.stmtIfCond = $5,
        Ast.stmtIfBody = $9,
        Ast.stmtIfLocation = $15
    }
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
    Ast.StmtCall $5
}

-- ***************
-- *             *
-- * stmt_return *
-- *             *
-- ***************
stmt_return: 'Return' '(' loc ')'
{
    Ast.StmtReturn $ Ast.StmtReturnContent
    {
        Ast.stmtReturnValue = Nothing,
        Ast.stmtReturnLocation = $3
    }
}

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
    Ast.StmtAssign $ Ast.StmtAssignContent
    {
        Ast.stmtAssignLhs = $5,
        Ast.stmtAssignRhs = $13
    }
}

-- *************
-- *           *
-- * with_item *
-- *           *
-- *************
with_item:
'withitem'
'('
    'context_expr' '=' exp
')'
{
    $5
}

-- *************
-- *           *
-- * stmt_with *
-- *           *
-- *************
stmt_with:
'With'
'('
    'items' '=' '[' commalistof(with_item) ']' ','
    'body' '=' stmts ','
    loc
')'
{
    Ast.StmtIf $ Ast.StmtIfContent
    {
        Ast.stmtIfCond = Ast.ExpCall $ Ast.ExpCallContent
        {
            Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent
            {
                Ast.varName = Token.VarName $ Token.Named
                {
                    Token.content = "nop",
                    Token.location = $13
                }
            },
            Ast.args = $6,
            Ast.expCallLocation = $13
        },
        Ast.stmtIfBody = $11,
        Ast.stmtIfLocation = $13
    }
}

-- ********
-- *      *
-- * stmt *
-- *      *
-- ********
stmt:
stmt_if          { $1 } |
stmt_call        { $1 } |
stmt_with        { $1 } |
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
    Ast.Param
    {
        Ast.paramName = Token.ParamName $ Token.Named
        {
            Token.content = unquote (tokIDValue $5),
            Token.location = $7
        },
        Ast.paramNominalType = Token.NominalTy $ Token.Named
        {
            Token.content = "any",
            Token.location = $7
        },
        Ast.paramSerialIdx = 156
    }
}

defaults:
'['                  ']' { Nothing } |
'[' commalistof(exp) ']' { Nothing }

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
    'defaults' '=' defaults
')'
{
    $11
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
    'body' '=' stmts ','
    'decorator_list' '=' '[' ']' ','
    'type_params' '=' '[' ']' ','
    loc
')'
{
    Ast.StmtFunc $ Ast.StmtFuncContent
    {
        Ast.stmtFuncReturnType = Token.NominalTy $ Token.Named
        {
            Token.content = unquote (tokIDValue $5),
            Token.location = $25
        },
        Ast.stmtFuncName = Token.FuncName $ Token.Named
        {
            Token.content = tokIDValue $5,
            Token.location = $25
        },
        Ast.stmtFuncParams = $9,
        Ast.stmtFuncBody = $13
    }
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
    Token.Named
    {
        Token.content = unquote (tokIDValue $5),
        Token.location = $11
    }
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
    Ast.StmtAssign $ Ast.StmtAssignContent
    {
        Ast.stmtAssignLhs = case $6 of
        {
            [] -> dummyVar $13;
            (v:_) -> Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName v
        },
        Ast.stmtAssignRhs = $11
    }
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
    Ast.StmtImport $ Ast.StmtImportContent
    {
        Ast.stmtImportName  = case $6 of { [] -> "BBB"; ((name, alias):_) -> name  },
        Ast.stmtImportAlias = case $6 of { [] -> "YYY"; ((name, alias):_) -> alias },
        Ast.stmtImportLocation = $9
    }
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
    Ast.StmtImport $ Ast.StmtImportContent
    {
        Ast.stmtImportName  = case $10 of { [] -> tokIDValue $5; ((name, alias):_) -> name  },
        Ast.stmtImportAlias = case $10 of { [] -> tokIDValue $5; ((name, alias):_) -> alias },
        Ast.stmtImportLocation = $17
    }
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
    case $7 of
    {
        Nothing -> (unquote (tokIDValue $5), unquote (tokIDValue $5));
        Just n -> (unquote (tokIDValue $5), n)
    }
}

-- **********
-- *        *
-- * asname *
-- *        *
-- **********
asname: 'asname' '=' ID ',' { unquote (tokIDValue $3) }

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
        Location.filename = getFilename $1,
        lineStart = tokIntValue $3,
        colStart = tokIntValue $7,
        lineEnd = tokIntValue $11,
        colEnd = tokIntValue $15
    }
}

{

dummyExp :: Location -> Ast.Exp
dummyExp loc = Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt 888 loc

dummyVar :: Location -> Ast.Var
dummyVar loc = Ast.VarSimple $ Ast.VarSimpleContent {
    Ast.varName = Token.VarName $ Token.Named {
        Token.content = "bestVarInTheWorld",
        Token.location = loc
    }
}

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

