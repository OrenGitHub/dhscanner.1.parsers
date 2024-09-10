{
{-# OPTIONS -Werror=missing-fields #-}

module PhpParser( parseProgram ) where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
import Ast
import PhpLexer
import Location
import qualified Token

-- *******************
-- *                 *
-- * general imports *
-- *                 *
-- *******************
import Data.Maybe
import Data.Either
import Data.List ( map, foldl' )
import Data.Map ( fromList )

}

-- ***********************
-- *                     *
-- * API function: parse *
-- *                     *
-- ***********************
%name parse

-- *************
-- * tokentype *
-- *************
%tokentype { AlexTokenTag }

-- *********
-- * monad *
-- *********
%monad { Alex }

-- *********
-- * lexer *
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

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************

':'    { AlexTokenTag AlexRawToken_COLON  _ }
'\\'   { AlexTokenTag AlexRawToken_SLASH  _ }
'-'    { AlexTokenTag AlexRawToken_HYPHEN _ }

-- *********************
-- *                   *
-- * reserved keywords *
-- *                   *
-- *********************

'Arg'                   { AlexTokenTag AlexRawToken_ARG             _ }
'var'                   { AlexTokenTag AlexRawToken_VAR             _ }
'args'                  { AlexTokenTag AlexRawToken_ARGS            _ }
'name'                  { AlexTokenTag AlexRawToken_NAME            _ }
'uses'                  { AlexTokenTag AlexRawToken_USES            _ }
'expr'                  { AlexTokenTag AlexRawToken_EXPR            _ }
'Name'                  { AlexTokenTag AlexRawToken_MAME            _ }
'null'                  { AlexTokenTag AlexRawToken_NULL            _ }
'type'                  { AlexTokenTag AlexRawToken_TYPE            _ }
'left'                  { AlexTokenTag AlexRawToken_LEFT            _ }
'loop'                  { AlexTokenTag AlexRawToken_LOOP            _ }
'init'                  { AlexTokenTag AlexRawToken_INIT            _ }
'cond'                  { AlexTokenTag AlexRawToken_COND            _ }
'exprs'                 { AlexTokenTag AlexRawToken_EXPRS           _ }
'value'                 { AlexTokenTag AlexRawToken_VALUE           _ }
'right'                 { AlexTokenTag AlexRawToken_RIGHT           _ }
'stmts'                 { AlexTokenTag AlexRawToken_STMTS           _ }
'array'                 { AlexTokenTag AlexRawToken_ARRAY           _ }
'Param'                 { AlexTokenTag AlexRawToken_PARAM           _ }
'UseItem'               { AlexTokenTag AlexRawToken_USE_ITEM        _ }
'Stmt_If'               { AlexTokenTag AlexRawToken_STMT_IF         _ }
'Stmt_Else'             { AlexTokenTag AlexRawToken_STMT_ELSE       _ }
'Stmt_ElseIf'           { AlexTokenTag AlexRawToken_STMT_ELIF       _ }
'Stmt_For'              { AlexTokenTag AlexRawToken_STMT_FOR        _ }
'Stmt_Switch'           { AlexTokenTag AlexRawToken_STMT_SWITCH     _ }
'Stmt_Case'             { AlexTokenTag AlexRawToken_STMT_CASE       _ }
'Stmt_Foreach'          { AlexTokenTag AlexRawToken_STMT_FOREACH    _ }
'Stmt_Echo'             { AlexTokenTag AlexRawToken_STMT_ECHO       _ }
'Stmt_Unset'            { AlexTokenTag AlexRawToken_STMT_UNSET      _ }
'Expr_Closure'          { AlexTokenTag AlexRawToken_EXPR_LAMBDA     _ }
'Expr_Cast_Double'      { AlexTokenTag AlexRawToken_EXPR_CAST       _ }
'Expr_Cast_Int'         { AlexTokenTag AlexRawToken_EXPR_CAST2      _ }
'Expr_Cast_Bool'        { AlexTokenTag AlexRawToken_EXPR_CAST4      _ }
'Expr_Cast_String'      { AlexTokenTag AlexRawToken_EXPR_CAST3      _ }
'Expr_New'              { AlexTokenTag AlexRawToken_EXPR_NEW        _ }
'Expr_Exit'             { AlexTokenTag AlexRawToken_EXPR_EXIT       _ }
'Expr_Ternary'          { AlexTokenTag AlexRawToken_EXPR_TERNARY    _ }
'Expr_Variable'         { AlexTokenTag AlexRawToken_EXPR_VAR        _ }
'Expr_FuncCall'         { AlexTokenTag AlexRawToken_EXPR_CALL       _ }
'Expr_MethodCall'       { AlexTokenTag AlexRawToken_EXPR_MCALL      _ }
'Expr_StaticCall'       { AlexTokenTag AlexRawToken_EXPR_SCALL      _ }
'Stmt_Use'              { AlexTokenTag AlexRawToken_STMT_USE        _ }
'Stmt_Expression'       { AlexTokenTag AlexRawToken_STMT_EXPR       _ }
'Scalar_Int'            { AlexTokenTag AlexRawToken_SCALAR_INT      _ }
'Scalar_InterpolatedString' { AlexTokenTag AlexRawToken_SCALAR_FSTRING _ }
'Identifier'            { AlexTokenTag AlexRawToken_IDENTIFIER      _ }
'Stmt_Return'           { AlexTokenTag AlexRawToken_STMT_RETURN     _ }
'Stmt_Property'         { AlexTokenTag AlexRawToken_STMT_PROPERTY   _ }
'Stmt_ClassMethod'      { AlexTokenTag AlexRawToken_STMT_CLASSMETH  _ }
'returnType'            { AlexTokenTag AlexRawToken_RETURN_TYPE     _ }
'Stmt_Class'            { AlexTokenTag AlexRawToken_STMT_CLASS      _ }
'Stmt_Continue'         { AlexTokenTag AlexRawToken_STMT_CONT       _ }
'Stmt_Break'            { AlexTokenTag AlexRawToken_STMT_BREAK      _ }
'Stmt_Function'         { AlexTokenTag AlexRawToken_STMT_FUNCTION   _ }
'Expr_Assign'           { AlexTokenTag AlexRawToken_EXPR_ASSIGN     _ }
'Expr_Array'            { AlexTokenTag AlexRawToken_EXPR_ARRAY      _ }
'Expr_Empty'            { AlexTokenTag AlexRawToken_EXPR_EMPTY      _ }
'ArrayItem'             { AlexTokenTag AlexRawToken_EXPR_ARRAY3     _ }
'Expr_ArrayDimFetch'    { AlexTokenTag AlexRawToken_EXPR_ARRAY2     _ }
'Expr_Isset'            { AlexTokenTag AlexRawToken_EXPR_ISSET      _ }
'Expr_ConstFetch'       { AlexTokenTag AlexRawToken_EXPR_CONST_GET  _ }
'Expr_PropertyFetch'    { AlexTokenTag AlexRawToken_EXPR_PROP_GET   _ }
'Expr_BooleanNot'       { AlexTokenTag AlexRawToken_EXPR_UNOP_NOT   _ }
'Expr_UnaryMinus'       { AlexTokenTag AlexRawToken_EXPR_UNOP_MINUS _ }
'Expr_BinaryOp_Plus'    { AlexTokenTag AlexRawToken_EXPR_BINOP_PLUS _ }
'Expr_BinaryOp_Concat'  { AlexTokenTag AlexRawToken_EXPR_BINOP_CONCAT _ }
'Expr_BinaryOp_BooleanOr' { AlexTokenTag AlexRawToken_EXPR_BINOP_OR _ }
'Expr_BinaryOp_BooleanAnd' { AlexTokenTag AlexRawToken_EXPR_BINOP_AND _ }
'Expr_BinaryOp_Smaller' { AlexTokenTag AlexRawToken_EXPR_BINOP_LT   _ }
'Expr_BinaryOp_Equal'   { AlexTokenTag AlexRawToken_EXPR_BINOP_EQ   _ }
'Expr_BinaryOp_Greater' { AlexTokenTag AlexRawToken_EXPR_BINOP_GT   _ }
'Expr_BinaryOp_Identical' { AlexTokenTag AlexRawToken_EXPR_BINOP_IS _ }
'Expr_BinaryOp_NotIdentical' { AlexTokenTag AlexRawToken_EXPR_BINOP_ISNOT _ }

-- ****************************
-- *                          *
-- * integers and identifiers *
-- *                          *
-- ****************************

INT    { AlexTokenTag (AlexRawToken_INT  i) _ }
ID     { AlexTokenTag (AlexRawToken_ID  id) _ }
STR    { AlexTokenTag (AlexRawToken_STR  s) _ }

-- *************************
-- *                       *
-- * grammar specification *
-- *                       *
-- *************************
%%

-- *********************
-- *                   *
-- * Ast root: program *
-- *                   *
-- *********************
program: stmts
{
    Ast.Root
    {
        Ast.filename = "DDD",
        Ast.stmts = $1
    }
}

tokenID:
ID      { tokIDValue $1 } |
'array' { "array"       } |
'value' { "value"       } |
'null'  { "null"        }


-- **********************
-- *                    *
-- * parametrized lists *
-- *                    *
-- **********************
listof(a): a { [$1] } | a listof(a) { $1:$2 }
ornull(a): 'null' { Nothing } | a { Just $1 }

-- *********
-- *       *
-- * stmts *
-- *       *
-- *********
stmts: 'array' '(' listof(numbered_stmt) ')' { $3 }

-- *****************
-- *               *
-- * numbered_stmt *
-- *               *
-- *****************
numbered_stmt: INT ':' stmt { $3 }

-- *****************
-- *               *
-- * stmt_function *
-- *               *
-- *****************
stmt_function:
'Stmt_Function' loc
'('
    ID ':' 'array' '(' ')'
    ID ':' ID
    'name' ':' identifier
    ID ':' params
    'returnType' ':' type
    'stmts' ':' stmts
')'
{
    Ast.StmtFunc $ Ast.StmtFuncContent
    {
        Ast.stmtFuncReturnType = Token.NominalTy (Token.Named "any" $2),
        Ast.stmtFuncName = Token.FuncName $14,
        Ast.stmtFuncParams = [],
        Ast.stmtFuncBody = [],
        Ast.stmtFuncAnnotations = [],
        Ast.stmtFuncLocation = $2
    }
}

-- *************
-- *           *
-- * stmt_cont *
-- *           *
-- *************
stmt_cont:
'Stmt_Continue' loc
'('
    ID ':' 'null'
')'
{
    Ast.StmtContinue (Ast.StmtContinueContent $2)
}

case:
'Stmt_Case' loc
'('
    'cond' ':' exp
    'stmts' ':' stmts
')'
{
    Nothing
}

numbered_case: INT ':' case { Nothing }
cases: 'array' '(' listof(numbered_case) ')' { Nothing }

-- ***************
-- *             *
-- * stmt_switch *
-- *             *
-- ***************
stmt_switch:
'Stmt_Switch' loc
'('
    'cond' ':' exp
    ID ':' cases
')'
{
    Ast.StmtExp $6
}

-- *************
-- *           *
-- * stmt_cont *
-- *           *
-- *************
stmt_break:
'Stmt_Break' loc
'('
    ID ':' 'null'
')'
{
    Ast.StmtBreak (Ast.StmtBreakContent $2)
}

-- ********
-- *      *
-- * stmt *
-- *      *
-- ********
stmt:
stmt_if        { $1 } | 
stmt_use       { $1 } |
stmt_for       { $1 } |
stmt_foreach   { $1 } |
stmt_exp       { $1 } |
stmt_echo      { $1 } |
stmt_unset     { $1 } |
stmt_assign    { $1 } |
stmt_switch    { $1 } |
stmt_class     { $1 } |
stmt_cont      { $1 } |
stmt_break     { $1 } |
stmt_function  { $1 } |
stmt_return    { $1 }

importee: tokenID { [$1] } | tokenID '\\' importee { $1:$3 }

name: 'Name' loc '(' 'name' ':' importee ')' { $6 }

use_item:
'UseItem' loc '(' 'type' ':' stmt_use_type 'name' ':' name ID ':' ID ')' { $9 }

numbered_use_item: INT ':' use_item { $3 }

use_items: listof(numbered_use_item) { head $1 }

stmt_use_type: tokenID '(' INT ')' { Nothing }

-- ************
-- *          *
-- * stmt_use *
-- *          *
-- ************
stmt_use:
'Stmt_Use' loc '(' 'type' ':' stmt_use_type 'uses' ':' 'array' '(' use_items ')' ')'
{
    Ast.StmtImport $ Ast.StmtImportContent
    {
        Ast.stmtImportName = (Data.List.foldl' (\x y -> x ++ "." ++ y) "" $11),
        Ast.stmtImportAlias = last $11,
        Ast.stmtImportLocation = $2
    }
} 

-- **************
-- *            *
-- * identifier *
-- *            *
-- **************
identifier: 'Identifier' loc '(' 'name' ':' tokenID ')'
{
    Token.Named
    {
        Token.content = $6,
        Token.location = $2
    }
}

numbered_class_attr: INT ':' class_attr { $3 }

class_attr: method { Nothing } | data_member { Nothing }

dec_var:
ID loc
'('
    'name' ':' ID loc '(' 'name' ':' ID ')'
    ID ':' 'Expr_ConstFetch' loc '(' 'name' ':' 'Name' loc '(' 'name' ':' ID ')' ')'
')'
{
    Nothing
}

data_member:
'Stmt_Property' loc
'('
    ID ':' 'array' '(' ')'
    ID ':' ID '(' INT ')'
    'type' ':' ID
    ID ':' 'array' '(' INT ':' dec_var ')'
')'
{
    Nothing
}

type:
tokenID { Nothing } |
identifier { Nothing } |
'Name' loc '(' 'name' ':' ID ')' { Nothing }

param_default_value:
tokenID { Nothing } |
exp     { Nothing }

param:
'Param' loc
'('
    ID ':' 'array' '(' ')'
    ID ':' INT
    'type' ':' type
    ID ':' ID
    ID ':' ID
    'var' ':' 'Expr_Variable' loc '(' 'name' ':' tokenID ')'
    ID ':' param_default_value
')'
{
    Ast.Param
    {
        Ast.paramName = Token.ParamName $ Token.Named $28 $24,
        Ast.paramNominalType = Token.NominalTy $ Token.Named "any" $24,
        paramSerialIdx = 15555
    }
}

numbered_param: INT ':' param { $3 }

params: 'array' '(' ')' { [] } | 'array' '(' listof(numbered_param) ')' { $3 }

method:
'Stmt_ClassMethod' loc
'('
    ID ':' 'array' '(' ')'
    ID ':' ID '(' INT ')'
    ID ':' ID
    'name' ':' identifier
    ID ':' params
    'returnType' ':' ID
    'stmts' ':' 'array' '(' listof(numbered_stmt) ')' 
')'
{
    Nothing
}


stmt_class:
'Stmt_Class' loc '('
    tokenID ':' 'array' '(' ')'
    ID ':' INT
    'name' ':' identifier
    ID ':' tokenID
    ID ':' 'array' '(' ')'
    'stmts' ':' 'array' '(' listof(numbered_class_attr) ')' 
')'
{
    Ast.StmtImport $ Ast.StmtImportContent
    {
        Ast.stmtImportName = $4,
        Ast.stmtImportAlias = $4,
        Ast.stmtImportLocation = $2
    }
} 

-- *************
-- *           *
-- * stmt_call *
-- *           *
-- *************
stmt_exp: 'Stmt_Expression' loc '(' 'expr' ':' exp ')' { Ast.StmtExp $6 }

-- *************
-- *           *
-- * stmt_else *
-- *           *
-- *************
stmt_else:
'Stmt_Else' loc
'('
    'stmts' ':' stmts
')'
{
    $6
}

elseif:
'Stmt_ElseIf' loc
'('
    'cond' ':' exp
    'stmts' ':' stmts
')'
{
    $9
}

numbered_elseif: INT ':' elseif { $3 }

elseifs:
'array' '(' ')' { [] } | 'array' '(' listof(numbered_elseif) ')' { [] }

-- ***********
-- *         *
-- * stmt_if *
-- *         *
-- ***********
stmt_if:
'Stmt_If' loc
'('
    'cond' ':' exp
    'stmts' ':' stmts
    ID ':' elseifs
    ID ':' ornull(stmt_else)
')'
{
    Ast.StmtIf $ Ast.StmtIfContent
    {
        Ast.stmtIfCond = $6,
        Ast.stmtIfBody = $9,
        Ast.stmtElseBody = [],
        Ast.stmtIfLocation = $2
    }
}

-- **************
-- *            *
-- * var_simple *
-- *            *
-- **************
var_simple:
'Expr_Variable' loc '(' 'name' ':' tokenID ')'
{
    Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
    {
        Token.content = $6,
        Token.location = $2
    }
}

-- *************
-- *           *
-- * var_field *
-- *           *
-- *************
var_field:
'Expr_PropertyFetch' loc
'('
    'var' ':' exp_var
    'name' ':' identifier
')'
{
    Ast.VarField $ Ast.VarFieldContent
    {
        Ast.varFieldLhs = $6,
        Ast.varFieldName = Token.FieldName $9,
        Ast.varFieldLocation = $2
    }
}

-- *****************
-- *               *
-- * var_subscript *
-- *               *
-- *****************
var_subscript:
'Expr_ArrayDimFetch' loc
'('
    'var' ':' exp
    ID ':' exp
')'
{
    Ast.VarSubscript $ Ast.VarSubscriptContent
    {
        Ast.varSubscriptLhs = $6,
        Ast.varSubscriptIdx = $9,
        Ast.varSubscriptLocation = $2
    }
}

-- *******
-- *     *
-- * var *
-- *     *
-- *******
var:
var_simple    { $1 } |
var_field     { $1 } |
var_subscript { $1 }

stmt_assign:
'Stmt_Expression' loc
'('
    'expr' ':' 'Expr_Assign' loc
    '('
        'var' ':' var
        'expr' ':' exp
    ')'
')'
{
    Ast.StmtAssign $ Ast.StmtAssignContent
    {
        Ast.stmtAssignLhs = $11,
        Ast.stmtAssignRhs = $14
    }
}

-- *************
-- *           *
-- * stmt_echo *
-- *           *
-- *************
stmt_echo:
'Stmt_Echo' loc
'('
    'exprs' ':' exps
')'
{
    Ast.StmtCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "echo",
            Token.location = $2
        },
        Ast.args = $6,
        Ast.expCallLocation = $2
    }
}

-- *************
-- *           *
-- * stmt_echo *
-- *           *
-- *************
stmt_unset:
'Stmt_Unset' loc
'('
    ID ':' exps
')'
{
    Ast.StmtCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "unset",
            Token.location = $2
        },
        Ast.args = $6,
        Ast.expCallLocation = $2
    }
}

exp_or_null:
'null' { Nothing } |
exp { Just $1 }

-- ***************
-- *             *
-- * stmt_return *
-- *             *
-- ***************
stmt_return: 'Stmt_Return' loc '(' 'expr' ':' exp_or_null ')'
{
    Ast.StmtReturn $ Ast.StmtReturnContent
    {
        Ast.stmtReturnValue = $6,
        Ast.stmtReturnLocation = $2
    }
}

-- ********
-- *      *
-- * exps *
-- *      *
-- ********
exps: 'array' '(' numbered_exps ')' { $3 }

-- *****************
-- *               *
-- * numbered_exps *
-- *               *
-- *****************
numbered_exps: numbered_exp numbered_exps { $1:$2 } | numbered_exp { [$1] }

-- ****************
-- *              *
-- * numbered_exp *
-- *              *
-- ****************
numbered_exp: INT ':' exp { $3 }

-- ***********
-- *         *
-- * exp_not *
-- *         *
-- ***********
exp_not:
'Expr_BooleanNot' loc
'('
    'expr' ':' exp
')'
{
    $6
}

-- ************
-- *          *
-- * exp_exit *
-- *          *
-- ************
exp_exit:
'Expr_Exit' loc
'('
    'expr' ':' 'null'
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "exit",
            Token.location = $2
        },
        Ast.args = [],
        Ast.expCallLocation = $2
    }
}

-- *************
-- *           *
-- * exp_isset *
-- *           *
-- *************
exp_isset:
'Expr_Isset' loc
'('
    ID ':' exps 
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "isset",
            Token.location = $2
        },
        Ast.args = $6,
        Ast.expCallLocation = $2
    }
}

-- ***************
-- *             *
-- * exp_ternary *
-- *             *
-- ***************
exp_ternary:
'Expr_Ternary' loc
'('
    'cond' ':' exp
    ID ':' exp
    ID ':' exp
')'
{
    $6
}

-- ************
-- *          *
-- * exp_cast *
-- *          *
-- ************
exp_cast1:
'Expr_Cast_Double' loc
'('
    'expr' ':' exp
')'
{
    $6
}

-- ************
-- *          *
-- * exp_cast *
-- *          *
-- ************
exp_cast2:
'Expr_Cast_Int' loc
'('
    'expr' ':' exp
')'
{
    $6
}

-- ************
-- *          *
-- * exp_cast *
-- *          *
-- ************
exp_cast3:
'Expr_Cast_String' loc
'('
    'expr' ':' exp
')'
{
    $6
}

-- ************
-- *          *
-- * exp_cast *
-- *          *
-- ************
exp_cast4:
'Expr_Cast_Bool' loc
'('
    'expr' ':' exp
')'
{
    $6
}

-- ************
-- *          *
-- * exp_cast *
-- *          *
-- ************
exp_cast:
exp_cast1 { $1 } |
exp_cast2 { $1 } |
exp_cast3 { $1 } |
exp_cast4 { $1 }

-- *************
-- *           *
-- * exp_empty *
-- *           *
-- *************
exp_empty:
'Expr_Empty' loc
'('
    'expr' ':' exp
')'
{
    $6
}

interpolated_string_part: exp { $1 }

numbered_interpolated_string_part:
INT ':' interpolated_string_part { $3 }

interpolated_string_parts:
'array' '(' listof(numbered_interpolated_string_part) ')' { $3 }

-- ***************
-- *             *
-- * exp_fstring *
-- *             *
-- ***************
exp_fstring:
'Scalar_InterpolatedString' loc
'('
    ID ':' interpolated_string_parts
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "fstring",
            Token.location = $2
        },
        Ast.args = $6,
        Ast.expCallLocation = $2
    }
}

unop_operator:
'Expr_UnaryMinus' { Nothing }

-- ************
-- *          *
-- * exp_unop *
-- *          *
-- ************
exp_unop:
unop_operator loc
'('
    'expr' ':' exp
')'
{
    $6
} 


-- *******
-- *     *
-- * exp *
-- *     *
-- *******
exp:
exp_int     { $1 } |
exp_str     { $1 } |
exp_new     { $1 } |
exp_not     { $1 } |
exp_bool    { $1 } |
exp_exit    { $1 } |
exp_cast    { $1 } |
exp_call    { Ast.ExpCall $1 } |
exp_binop   { $1 } |
exp_unop    { $1 } |
exp_isset   { $1 } |
exp_empty   { $1 } |
exp_array   { $1 } |
exp_lambda  { $1 } |
exp_ternary { $1 } |
exp_fstring { $1 } |
exp_var     { $1 }

array_item:
'ArrayItem' loc
'('
    ID ':' ornull(exp)
    'value' ':' exp 
    ID ':' ID
    ID ':' ID
')'
{
    Nothing
}

numbered_array_item: INT ':' array_item { $3 }

array_items:
'array' '(' ')' { [] } | 'array' '(' listof(numbered_array_item) ')' { $3 }

-- *************
-- *           *
-- * exp_array *
-- *           *
-- *************
exp_array:
'Expr_Array' loc
'('
    ID ':' array_items
')'
{
    Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt
    {
        Token.constIntValue = 3333,
        Token.constIntLocation = $2
    }
}

-- **************
-- *            *
-- * exp_lambda *
-- *            *
-- **************
exp_lambda:
'Expr_Closure' loc
'('
    ID ':' 'array' '(' ')'
    ID ':' ID
    ID ':' ID
    ID ':' params
    'uses' ':' 'array' '(' ')'
    'returnType' ':' ID
    'stmts' ':' stmts
')'
{
    Ast.ExpLambda $ Ast.ExpLambdaContent
    {
        Ast.expLambdaParams = $17,
        Ast.expLambdaBody = $28,
        Ast.expLambdaLocation = $2
    }
}

-- ***********
-- *         *
-- * exp_int *
-- *         *
-- ***********
exp_int: 'Scalar_Int' loc '(' 'value' ':' INT ')'
{
    Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt
    {
        Token.constIntValue = tokIntValue $6,
        Token.constIntLocation = $2
    }
}

str:
tokenID { $1 } |
STR     { tokStrValue $1 } |
INT     { show $ tokIntValue $1 }

-- ***********
-- *         *
-- * exp_str *
-- *         *
-- ***********
exp_str: STR
{
    Ast.ExpStr $ Ast.ExpStrContent $ Token.ConstStr
    {
        Token.constStrValue = tokStrValue $1,
        Token.constStrLocation = Location "MMM" 0 0 0 0
    }
}

-- ************
-- *          *
-- * exp_bool *
-- *          *
-- ************
exp_bool:
'Expr_ConstFetch' loc '(' 'name' ':' 'Name' loc '(' 'name' ':' ID ')' ')'
{
    Ast.ExpBool $ Ast.ExpBoolContent $ Token.ConstBool
    {
        Token.constBoolValue = True,
        Token.constBoolLocation = $2
    }
}
 
-- ***********
-- *         *
-- * exp_var *
-- *         *
-- ***********
exp_var: var { Ast.ExpVar (Ast.ExpVarContent $1) }

operator:
'Expr_BinaryOp_Smaller'      { Nothing } |
'Expr_BinaryOp_Equal'        { Nothing } |
'Expr_BinaryOp_Greater'      { Nothing } |
'Expr_BinaryOp_Concat'       { Nothing } |
'Expr_BinaryOp_Identical'    { Nothing } |
'Expr_BinaryOp_NotIdentical' { Nothing } |
'Expr_BinaryOp_BooleanOr'    { Nothing } |
'Expr_BinaryOp_BooleanAnd'   { Nothing }

-- *************
-- *           *
-- * exp_binop *
-- *           *
-- *************
exp_binop:
operator loc
'('
    'left' ':' exp
    'right' ':' exp
')'
{
    Ast.ExpBinop $ Ast.ExpBinopContent
    {
        Ast.expBinopLeft = $6,
        Ast.expBinopRight = $9,
        Ast.expBinopOperator = Ast.PLUS,
        Ast.expBinopLocation = $2
    }
} 

-- ***********
-- *         *
-- * exp_new *
-- *         *
-- ***********
exp_new:
'Expr_New' loc
'('
    ID ':' 'Name' loc '(' 'name' ':' tokenID ')'
    'args' ':' args
')' 
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = $11,
            Token.location = $2
        },
        Ast.args = [],
        Ast.expCallLocation = $2
    }
}


-- ************
-- *          *
-- * exp_call *
-- *          *
-- ************
exp_call:
exp_func_call          { $1 } |
exp_method_call        { $1 } |
exp_static_method_call { $1 }

-- *****************
-- *               *
-- * exp_func_call *
-- *               *
-- *****************
exp_func_call:
'Expr_FuncCall' loc
'('
    'name' ':' 'Name' loc '(' 'name' ':' tokenID ')'
    'args' ':' args
')' 
{
    Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent
        {
            Ast.varName = Token.VarName $ Token.Named { Token.content = $11, Token.location = $2 }
        },
        Ast.args = $15,
        Ast.expCallLocation = $2
    }
}

-- *******************
-- *                 *
-- * exp_method_call *
-- *                 *
-- *******************
exp_method_call:
'Expr_MethodCall' loc
'('
    'var' ':' var
    'name' ':' 'Identifier' loc '(' 'name' ':' tokenID ')'
    'args' ':' args
')' 
{
    Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarField $ Ast.VarFieldContent
        {
            Ast.varFieldLhs = Ast.ExpVar (Ast.ExpVarContent $6),
            Ast.varFieldName = Token.FieldName $ Token.Named { Token.content = $14, Token.location = $10 },
            Ast.varFieldLocation = Location {
                Location.filename = getFilename $1,
                lineStart = lineStart $2,
                colStart = colStart $2,
                lineEnd = lineEnd $10,
                colEnd = colEnd $10
            }
        },
        Ast.args = $18,
        Ast.expCallLocation = $2
    }
}

-- **************************
-- *                        *
-- * exp_static_method_call *
-- *                        *
-- **************************
exp_static_method_call:
'Expr_StaticCall' loc
'('
    ID ':' 'Name' loc '(' 'name' ':' tokenID ')'
    'name' ':' identifier
    'args' ':' args
')'
{
    Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarField $ Ast.VarFieldContent
        {
            Ast.varFieldLhs = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent
            {
                Ast.varName = Token.VarName $ Token.Named { Token.content = $11, Token.location = $7 }
            },
            Ast.varFieldName = Token.FieldName $15,
            Ast.varFieldLocation = $2
        },
        Ast.args = $18,
        Ast.expCallLocation = $2
    }
}

-- ********
-- *      *
-- * args *
-- *      *
-- ********
args:
'array' '('                      ')' { [] } |
'array' '(' listof(numbered_arg) ')' { $3 }

-- ****************
-- *              *
-- * numbered_arg *
-- *              *
-- ****************
numbered_arg: INT ':' arg { $3 }

-- *******
-- *     *
-- * arg *
-- *     *
-- *******
arg: 'Arg' loc '(' 'name' ':' tokenID 'value' ':' exp ID ':' ID ID ':' ID ')' { $9 }

-- ************
-- *          *
-- * stmt_for *
-- *          *
-- ************
stmt_for: 'Stmt_For' loc '(' 'init' ':' stmts 'cond' ':' exps 'loop' ':' exp 'stmts' ':' stmts ')'
{
    Ast.StmtWhile $ Ast.StmtWhileContent
    {
        Ast.stmtWhileCond = $12,
        Ast.stmtWhileBody = $15,
        Ast.stmtWhileLocation = $2
    }
}

-- ****************
-- *              *
-- * stmt_foreach *
-- *              *
-- ****************
stmt_foreach:
'Stmt_Foreach' loc
'('
    'expr' ':' exp
    ID ':' exp
    ID ':' ID 
    ID ':' exp
    'stmts' ':' stmts
')'
{
    Ast.StmtWhile $ Ast.StmtWhileContent
    {
        Ast.stmtWhileCond = $6,
        Ast.stmtWhileBody = $18,
        Ast.stmtWhileLocation = $2
    }
}


-- *******
-- *     *
-- * loc *
-- *     *
-- *******
loc: '[' INT ':' INT '-' INT ':' INT ']'
{
    Location
    {
        Location.filename = getFilename $1,
        lineStart = fromIntegral (tokIntValue $2),
        colStart = fromIntegral (tokIntValue $4),
        lineEnd = fromIntegral (tokIntValue $6),
        colEnd = fromIntegral (tokIntValue $8)
    }
}

{

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
