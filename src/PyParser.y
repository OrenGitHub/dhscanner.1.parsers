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
import Data.Map ( fromList, empty )

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
'iter'                      { AlexTokenTag AlexRawToken_ITER            _ }
'Dict'                      { AlexTokenTag AlexRawToken_DICT            _ }
'keys'                      { AlexTokenTag AlexRawToken_KEYS            _ }
'right'                     { AlexTokenTag AlexRawToken_RIGHT           _ }
'op'                        { AlexTokenTag AlexRawToken_OPERATOR        _ }
'ops'                       { AlexTokenTag AlexRawToken_OPERATOR2       _ }
'decorator_list'            { AlexTokenTag AlexRawToken_DECORATORS      _ }
'optional_vars'             { AlexTokenTag AlexRawToken_WITH_VARS       _ }
'type_params'               { AlexTokenTag AlexRawToken_TYPE_PARAMS     _ }
'type_ignores'              { AlexTokenTag AlexRawToken_TYPE_IGNORES    _ }
'comparators'               { AlexTokenTag AlexRawToken_COMPARE2        _ }
'BoolOp'                    { AlexTokenTag AlexRawToken_COMPARE3        _ }
'Compare'                   { AlexTokenTag AlexRawToken_COMPARE         _ }
'operand'                   { AlexTokenTag AlexRawToken_OPERAND         _ }
'Return'                    { AlexTokenTag AlexRawToken_STMT_RETURN     _ }
'returns'                   { AlexTokenTag AlexRawToken_STMT_RETURN2    _ }
'Yield'                     { AlexTokenTag AlexRawToken_STMT_YIELD      _ }
'Raise'                     { AlexTokenTag AlexRawToken_STMT_RAISE      _ }
'cause'                     { AlexTokenTag AlexRawToken_STMT_CAUSE      _ }
'Del'                       { AlexTokenTag AlexRawToken_STMT_DEL        _ }
'Delete'                    { AlexTokenTag AlexRawToken_STMT_DELETE     _ }
'Global'                    { AlexTokenTag AlexRawToken_STMT_GLOBAL     _ }
'Try'                       { AlexTokenTag AlexRawToken_STMT_TRY        _ }
'exc'                       { AlexTokenTag AlexRawToken_EXC             _ }
'With'                      { AlexTokenTag AlexRawToken_WITH            _ }
'AsyncWith'                 { AlexTokenTag AlexRawToken_WITH            _ }
'withitem'                  { AlexTokenTag AlexRawToken_WITH2           _ }
'context_expr'              { AlexTokenTag AlexRawToken_CTX_MANAGER     _ }
'items'                     { AlexTokenTag AlexRawToken_ITEMS           _ }
'List'                      { AlexTokenTag AlexRawToken_LIST            _ }
'Set'                       { AlexTokenTag AlexRawToken_SET             _ }
'SetComp'                   { AlexTokenTag AlexRawToken_SET_COMP        _ }
'ListComp'                  { AlexTokenTag AlexRawToken_LIST_COMP       _ }
'DictComp'                  { AlexTokenTag AlexRawToken_DICT_COMP       _ }
'GeneratorExp'              { AlexTokenTag AlexRawToken_GENERATOR_EXP   _ }
'Tuple'                     { AlexTokenTag AlexRawToken_TUPLE           _ }
'elt'                       { AlexTokenTag AlexRawToken_ELT             _ }
'elts'                      { AlexTokenTag AlexRawToken_ELTS            _ }
'False'                     { AlexTokenTag AlexRawToken_FALSE           _ }
'True'                      { AlexTokenTag AlexRawToken_TRUE            _ }
'Ellipsis'                  { AlexTokenTag AlexRawToken_ELLIPSIS        _ }
'Constant'                  { AlexTokenTag AlexRawToken_EXPR_CONST      _ }
'Continue'                  { AlexTokenTag AlexRawToken_STMT_CONTINUE   _ }
'Break'                     { AlexTokenTag AlexRawToken_STMT_BREAK      _ }
'Pass'                      { AlexTokenTag AlexRawToken_STMT_PASS       _ }
'Not'                       { AlexTokenTag AlexRawToken_NOT             _ }
'NotEq'                     { AlexTokenTag AlexRawToken_NOTEQ           _ }
'NotIn'                     { AlexTokenTag AlexRawToken_NOTIN           _ }
'Add'                       { AlexTokenTag AlexRawToken_ADD             _ }
'Pow'                       { AlexTokenTag AlexRawToken_POW             _ }
'FloorDiv'                  { AlexTokenTag AlexRawToken_FLOOR_DIV       _ }
'Mod'                       { AlexTokenTag AlexRawToken_MOD             _ }
'Div'                       { AlexTokenTag AlexRawToken_DIV             _ }
'Sub'                       { AlexTokenTag AlexRawToken_SUB             _ }
'USub'                      { AlexTokenTag AlexRawToken_USUB            _ }
'Mult'                      { AlexTokenTag AlexRawToken_MULT            _ }
'Eq'                        { AlexTokenTag AlexRawToken_EQ              _ }
'Gt'                        { AlexTokenTag AlexRawToken_GT              _ }
'GtE'                       { AlexTokenTag AlexRawToken_GE              _ }
'LtE'                       { AlexTokenTag AlexRawToken_LE              _ }
'Lt'                        { AlexTokenTag AlexRawToken_LT              _ }
'In'                        { AlexTokenTag AlexRawToken_IN              _ }
'Is'                        { AlexTokenTag AlexRawToken_IS              _ }
'IsNot'                     { AlexTokenTag AlexRawToken_ISNOT           _ }
'Or'                        { AlexTokenTag AlexRawToken_OR              _ }
'BitOr'                     { AlexTokenTag AlexRawToken_OR2             _ }
'BitXor'                    { AlexTokenTag AlexRawToken_BITXOR          _ }
'And'                       { AlexTokenTag AlexRawToken_AND             _ }
'BitAnd'                    { AlexTokenTag AlexRawToken_AND2            _ }
'LShift'                    { AlexTokenTag AlexRawToken_LSHIFT          _ }
'RShift'                    { AlexTokenTag AlexRawToken_RSHIFT          _ }
'ctx'                       { AlexTokenTag AlexRawToken_CTX             _ }
'kwonlyargs'                { AlexTokenTag AlexRawToken_ARGS4           _ }
'posonlyargs'               { AlexTokenTag AlexRawToken_ARGS3           _ }
'arguments'                 { AlexTokenTag AlexRawToken_ARGS2           _ }
'arg'                       { AlexTokenTag AlexRawToken_ARG             _ }
'vararg'                    { AlexTokenTag AlexRawToken_VARARG          _ }
'args'                      { AlexTokenTag AlexRawToken_ARGS            _ }
'attr'                      { AlexTokenTag AlexRawToken_ATTR            _ }
'Attribute'                 { AlexTokenTag AlexRawToken_ATTR2           _ }
'Starred'                   { AlexTokenTag AlexRawToken_STARRED         _ }
'Subscript'                 { AlexTokenTag AlexRawToken_SUBSCRIPT       _ }
'slice'                     { AlexTokenTag AlexRawToken_SLICE           _ }
'lower'                     { AlexTokenTag AlexRawToken_LOWER           _ }
'step'                      { AlexTokenTag AlexRawToken_STEP            _ }
'upper'                     { AlexTokenTag AlexRawToken_UPPER           _ }
'Slice'                     { AlexTokenTag AlexRawToken_EXPR_SLICE      _ }
'func'                      { AlexTokenTag AlexRawToken_FUNC            _ }
'body'                      { AlexTokenTag AlexRawToken_BODY            _ }
'None'                      { AlexTokenTag AlexRawToken_NONE            _ }
'handlers'                  { AlexTokenTag AlexRawToken_HANDLERS        _ }
'type'                      { AlexTokenTag AlexRawToken_TYPE            _ }
'finalbody'                 { AlexTokenTag AlexRawToken_BODY2           _ }
'ExceptHandler'             { AlexTokenTag AlexRawToken_EXCEPT_HANDLER  _ }
'test'                      { AlexTokenTag AlexRawToken_TEST            _ }
'Name'                      { AlexTokenTag AlexRawToken_NAME2           _ }
'Call'                      { AlexTokenTag AlexRawToken_CALL            _ }
'Expr'                      { AlexTokenTag AlexRawToken_STMT_EXPR       _ }
'level'                     { AlexTokenTag AlexRawToken_LEVEL           _ }
'key'                       { AlexTokenTag AlexRawToken_KEY             _ }
'value'                     { AlexTokenTag AlexRawToken_VALUE           _ }
'kind'                      { AlexTokenTag AlexRawToken_KIND            _ }
'values'                    { AlexTokenTag AlexRawToken_VALUES          _ }
'name'                      { AlexTokenTag AlexRawToken_NAME            _ }
'asname'                    { AlexTokenTag AlexRawToken_ASNAME          _ }
'orelse'                    { AlexTokenTag AlexRawToken_ORELSE          _ }
'defaults'                  { AlexTokenTag AlexRawToken_DEFAULTS        _ }
'kwarg'                     { AlexTokenTag AlexRawToken_KWARG           _ }
'comprehension'             { AlexTokenTag AlexRawToken_COMPREHENSION   _ }
'generators'                { AlexTokenTag AlexRawToken_GENERATORS      _ }
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
'format_spec'               { AlexTokenTag AlexRawToken_FORMAT_SPEC     _ }
'FormattedValue'            { AlexTokenTag AlexRawToken_FORMATTED_VAL   _ }
'Load'                      { AlexTokenTag AlexRawToken_LOAD            _ }
'Store'                     { AlexTokenTag AlexRawToken_STORE           _ }
'is_async'                  { AlexTokenTag AlexRawToken_IS_ASYNC        _ }
'simple'                    { AlexTokenTag AlexRawToken_SIMPLE          _ }
'Assign'                    { AlexTokenTag AlexRawToken_ASSIGN          _ }
'Await'                     { AlexTokenTag AlexRawToken_AWAIT           _ }
'Assert'                    { AlexTokenTag AlexRawToken_ASSERT          _ }
'Lambda'                    { AlexTokenTag AlexRawToken_LAMBDA          _ }
'AugAssign'                 { AlexTokenTag AlexRawToken_ASSIGN2         _ }
'AnnAssign'                 { AlexTokenTag AlexRawToken_ASSIGN3         _ }
'annotation'                { AlexTokenTag AlexRawToken_ANNOTATION      _ }
'Module'                    { AlexTokenTag AlexRawToken_MODULE          _ }
'module'                    { AlexTokenTag AlexRawToken_MODULE2         _ }
'msg' { AlexTokenTag AlexRawToken_msg _ }
-- last keywords first part

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
'BinOp'                     { AlexTokenTag AlexRawToken_EXPR_BINOP      _ }


-- **************
-- *            *
-- * statements *
-- *            *
-- **************

'If'                        { AlexTokenTag AlexRawToken_STMT_IF         _ }
'ifs'                       { AlexTokenTag AlexRawToken_STMT_IFS        _ }
'While'                     { AlexTokenTag AlexRawToken_STMT_WHILE      _ }
'IfExp'                     { AlexTokenTag AlexRawToken_EXPR_IF         _ }
'For'                       { AlexTokenTag AlexRawToken_FOR             _ }
'AsyncFor'                  { AlexTokenTag AlexRawToken_FOR             _ }
'FunctionDef'               { AlexTokenTag AlexRawToken_STMT_FUNCTION   _ }
'AsyncFunctionDef'          { AlexTokenTag AlexRawToken_STMT_FUNCTION   _ }
'ClassDef'                  { AlexTokenTag AlexRawToken_STMT_CLASS      _ }
'bases'                     { AlexTokenTag AlexRawToken_SUPERS          _ }

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
optional(a): { Nothing } | a { Just $1 }

-- **********************
-- *                    *
-- * parametrized lists *
-- *                    *
-- **********************
commalistof(a): a { [$1] } | a ',' commalistof(a) { $1:$3 }

-- ****************************
-- *                          *
-- * possibly_empty_listof(a) *
-- *                          *
-- ****************************
possibly_empty_listof(a): '[' ']' { [] } | '[' commalistof(a) ']' { $2 }

-- **********************
-- *                    *
-- * nonempty_listof(a) *
-- *                    *
-- **********************
nonempty_listof(a): '[' commalistof(a) ']' { $2 }

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
'Not'  '(' ')' { Nothing } |
'NotEq' '(' ')' { Nothing } |
'NotIn' '(' ')' { Nothing } |
'Add'  '(' ')' { Nothing } |
'Pow'  '(' ')' { Nothing } |
'Mod'  '(' ')' { Nothing } |
'Div'  '(' ')' { Nothing } |
'USub' '(' ')' { Nothing } |
'Sub'  '(' ')' { Nothing } |
'Mult' '(' ')' { Nothing } |
'Eq'   '(' ')' { Nothing } |
'Gt'   '(' ')' { Nothing } |
'LtE'  '(' ')' { Nothing } |
'GtE'  '(' ')' { Nothing } |
'Lt'   '(' ')' { Nothing } |
'In'   '(' ')' { Nothing } |
'Is'   '(' ')' { Nothing } |
'IsNot' '(' ')' { Nothing } |
'And'  '(' ')' { Nothing } |
'BitAnd' '(' ')' { Nothing } |
'LShift' '(' ')' { Nothing } |
'RShift' '(' ')' { Nothing } |
'FloorDiv' '(' ')' { Nothing } |
'BitOr' '(' ')' { Nothing } |
'BitXor' '(' ')' { Nothing } |
'Or'   '(' ')' { Nothing }

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
    'value' '=' exp','
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

-- *****************
-- *               *
-- * var_subscript *
-- *               *
-- *****************
var_subscript:
'Subscript'
'('
    'value' '=' exp ','
    'slice' '=' exp ','
    'ctx' '=' ctx ','
    loc
')'
{
    Ast.VarSubscript $ Ast.VarSubscriptContent
    {
        Ast.varSubscriptLhs = $5,
        Ast.varSubscriptIdx = $9,
        Ast.varSubscriptLocation = $15
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

-- ***********
-- *         *
-- * exp_var *
-- *         *
-- ***********
exp_var: var { Ast.ExpVar (Ast.ExpVarContent $1) }

exp_str_kind:
'kind' '=' tokenID ',' { Nothing }

-- ***********
-- *         *
-- * exp_str *
-- *         *
-- ***********
exp_str:
'Constant'
'('
    'value' '=' ID ','
    optional(exp_str_kind)
    loc
')'
{
    Ast.ExpStr $ Ast.ExpStrContent
    {
        Ast.expStrValue = Token.ConstStr
        {
            Token.constStrValue = unquote (tokIDValue $5),
            Token.constStrLocation = $8
        }
    }
}

-- ***********
-- *         *
-- * exp_int *
-- *         *
-- ***********
exp_int:
'Constant'
'('
    'value' '=' INT ','
    loc
')'
{
    Ast.ExpInt $ Ast.ExpIntContent
    {
        Ast.expIntValue = Token.ConstInt
        {
            Token.constIntValue = tokIntValue $5,
            Token.constIntLocation = $7
        }
    }
}

-- ************
-- *          *
-- * exp_none *
-- *          *
-- ************
exp_none:
'Constant'
'('
    'value' '=' 'None' ','
    loc
')'
{
    Ast.ExpInt $ Ast.ExpIntContent
    {
        Ast.expIntValue = Token.ConstInt
        {
            Token.constIntValue = 0,
            Token.constIntLocation = $7
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
conversion:
'-' INT { Nothing } |
INT     { Nothing }

format_spec: 'format_spec' '=' exp ',' { Nothing }

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
    optional(format_spec)
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

-- ***************
-- *             *
-- * exp_relop_1 *
-- *             *
-- ***************
exp_relop_1:
'Compare'
'('
    'left' '=' exp ','
    'ops' '=' '[' commalistof(op) ']' ','
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

-- ***************
-- *             *
-- * exp_relop_2 *
-- *             *
-- ***************
exp_relop_2:
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
-- * exp_relop *
-- *           *
-- *************
exp_relop:
exp_relop_1 { $1 } |
exp_relop_2 { $1 }

-- *************
-- *           *
-- * exp_binop *
-- *           *
-- *************
exp_binop:
'BinOp'
'('
    'left' '=' exp ','
    'op' '=' op ','
    'right' '=' exp ','
    loc
')'
{
    Ast.ExpBinop $ Ast.ExpBinopContent
    {
        Ast.expBinopLeft = $5,
        Ast.expBinopRight = $13,
        Ast.expBinopOperator = Ast.PLUS,
        Ast.expBinopLocation = $15
    }
}

-- ************
-- *          *
-- * exp_list *
-- *          *
-- ************
exp_list:
'List'
'('
    'elts' '=' possibly_empty_listof(exp) ','
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
                    Token.location = $11
                }
            }
        },
        Ast.args = $5,
        Ast.expCallLocation = $11
    }
}

-- *************
-- *           *
-- * exp_field *
-- *           *
-- *************
exp_field:
'Attribute'
'('
    'value' '=' exp_str ','
    'attr' '=' ID ','
    'ctx' '=' ctx ','
    loc
')'
{
    $5
}

slice_lower: 'lower' '=' exp ',' { $3 }
slice_step:  'step'  '=' exp ',' { $3 }
slice_upper: 'upper' '=' exp ',' { $3 }

-- *************
-- *           *
-- * exp_slice *
-- *           *
-- *************
exp_slice:
'Slice'
'('
    optional(slice_lower)
    optional(slice_upper)
    optional(slice_step)
    loc
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "slicify",
            Token.location = $6
        },
        Ast.args = [],
        Ast.expCallLocation = $6
    }
}

-- **********
-- *        *
-- * exp_if *
-- *        *
-- **********
exp_if:
'IfExp'
'('
    'test' '=' exp ','
    'body' '=' exp ','
    'orelse' '=' exp ','
    loc
')'
{
    $5
}

-- ********
-- *      *
-- * exps *
-- *      *
-- ********
exps: possibly_empty_listof(exp) { $1 }

none: 'None' { Nothing }

dict_key:
exp  { Nothing } |
none { Nothing }

dict_keys: possibly_empty_listof(dict_key) { $1 }

-- ************
-- *          *
-- * exp_dict *
-- *          *
-- ************
exp_dict:
'Dict'
'('
    'keys' '=' dict_keys ','
    'values' '=' exps ','
    loc
')'
{
    Ast.ExpInt $ Ast.ExpIntContent
    {
        Ast.expIntValue = Token.ConstInt
        {
            Token.constIntValue = 999,
            Token.constIntLocation = $11
        }
    } 
}

-- ****************
-- *              *
-- * exp_ellipsis *
-- *              *
-- ****************
exp_ellipsis:
'Constant'
'('
    'value' '=' 'Ellipsis' ','
    loc
')'
{
    Ast.ExpInt $ Ast.ExpIntContent
    {
        Ast.expIntValue = Token.ConstInt
        {
            Token.constIntValue = 999,
            Token.constIntLocation = $7
        }
    }
}

-- *************
-- *           *
-- * exp_tuple *
-- *           *
-- *************
exp_tuple:
'Tuple'
'('
    'elts' '=' possibly_empty_listof(exp) ','
    'ctx' '=' ctx ','
    loc
')'
{
    case $5 of {
        [] -> Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt 888 $11;
        (x:_) -> x
    }
}

comprehension:
'comprehension'
'('
    'target' '=' exp ','
    'iter' '=' exp ','
    'ifs' '=' possibly_empty_listof(exp) ','
    'is_async' '=' INT
')'
{
    Nothing
}

exp_setcomp:
'SetComp'
'('
    'elt' '=' exp ','
    'generators' '=' nonempty_listof(comprehension) ','
    loc
')'
{
    $5
}


exp_listcomp:
'ListComp'
'('
    'elt' '=' exp ','
    'generators' '=' nonempty_listof(comprehension) ','
    loc
')'
{
    $5
}

exp_dictcomp:
'DictComp'
'('
    'key' '=' exp ','
    'value' '=' exp ','
    'generators' '=' nonempty_listof(comprehension) ','
    loc
')'
{
    $5
}

generator:
'GeneratorExp'
'('
    'elt' '=' exp ','
    'generators' '=' nonempty_listof(comprehension) ','
    loc
')'
{
    $5
}

exp_set:
'Set'
'('
    'elts' '=' possibly_empty_listof(exp) ','
    loc
')'
{
    Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt
    {
        Token.constIntValue = 999,
        Token.constIntLocation = $7
    }
}

exp_lambda:
'Lambda'
'('
    'args' '=' params ','
    'body' '=' exp ','
    loc 
')'
{
    $9
}

exp_starred:
'Starred'
'('
    'value' '=' exp ','
    'ctx' '=' ctx ','
    loc
')'
{
    $5
}

exp_await:
'Await'
'('
    'value' '=' exp ','
    loc
')'
{
    $5
}

-- *******
-- *     *
-- * exp *
-- *     *
-- *******
exp:
exp_if        { $1 } |
exp_str       { $1 } |
exp_int       { $1 } |
exp_none      { $1 } |
exp_yield     { $1 } |
exp_starred   { $1 } |
exp_dict      { $1 } |
exp_var       { $1 } |
exp_bool      { $1 } |
exp_set       { $1 } |
exp_list      { $1 } |
generator     { $1 } |
exp_setcomp   { $1 } |
exp_listcomp  { $1 } |
exp_dictcomp  { $1 } |
exp_tuple     { $1 } |
exp_lambda    { $1 } |
exp_unop      { $1 } |
exp_slice     { $1 } |
exp_relop     { $1 } |
exp_await     { $1 } |
exp_binop     { $1 } |
exp_call      { Ast.ExpCall $1 } |
exp_fstring   { $1 } |
exp_ellipsis  { $1 }

keyword_arg: 'arg' '=' tokenID ',' { $3 }

-- ***********
-- *         *
-- * keyword *
-- *         *
-- ***********
keyword:
'keyword'
'('
    optional(keyword_arg)
    'value' '=' exp ','
    loc
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = expmyname (Token.Named "keyword_arg" $8),
        Ast.args = [ (expmyname (Token.Named (case $3 of { Just s -> s; _ -> "<missing>" }) $8)), $6 ],
        Ast.expCallLocation = $8
    }
}

-- ************
-- *          *
-- * keywords *
-- *          *
-- ************
keywords:
'[' ']'                      { [] } | 
'[' commalistof(keyword) ']' { $2 }

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
        Ast.args = $9 ++ $13,
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
        Ast.stmtElseBody = $13,
        Ast.stmtIfLocation = $15
    }
}

-- **************
-- *            *
-- * stmt_while *
-- *            *
-- **************
stmt_while:
'While'
'('
    'test' '=' exp ','
    'body' '=' stmts ','
    'orelse' '=' stmts ','
    loc 
')'
{
    Ast.StmtWhile $ Ast.StmtWhileContent
    {
        Ast.stmtWhileCond = $5,
        Ast.stmtWhileBody = $9,
        Ast.stmtWhileLocation = $15
    }
}


-- ****************
-- *              *
-- * return_value *
-- *              *
-- ****************
return_value: 'value' '=' exp ',' { $3 }

-- ***************
-- *             *
-- * stmt_return *
-- *             *
-- ***************
stmt_return: 'Return' '(' optional(return_value) loc ')'
{
    Ast.StmtReturn $ Ast.StmtReturnContent
    {
        Ast.stmtReturnValue = $3,
        Ast.stmtReturnLocation = $4
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
-- * with_vars *
-- *           *
-- *************
with_vars: ',' 'optional_vars' '=' name { $4 }

-- *************
-- *           *
-- * with_item *
-- *           *
-- *************
with_item:
'withitem'
'('
    'context_expr' '=' exp
    optional(with_vars)
')'
{
    case $6 of {
        Nothing -> Ast.StmtExp $5;
        Just name -> Ast.StmtAssign $ Ast.StmtAssignContent
            {
                Ast.stmtAssignLhs = Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName name,
                Ast.stmtAssignRhs = $5
            }
    }
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
        Ast.stmtIfCond = Ast.ExpBool $ Ast.ExpBoolContent $ Token.ConstBool True $13,
        Ast.stmtIfBody = $6 ++ $11,
        Ast.stmtElseBody = [],
        Ast.stmtIfLocation = $13
    }
}

-- **************
-- *            *
-- * stmt_class *
-- *            *
-- **************
stmt_class:
'ClassDef'
'('
    'name' '=' tokenID ','
    'bases' '=' possibly_empty_listof(var) ','
    'keywords' '=' keywords ','
    'body' '=' stmts ','
    'decorator_list' '=' exps ','
    'type_params' '=' '[' ']' ','
    loc
')'
{
    Ast.StmtClass $ Ast.StmtClassContent
    {
        Ast.stmtClassName = Token.ClassName (Token.Named $5 $28),
        Ast.stmtClassSupers = superify $9,
        Ast.stmtClassDataMembers = Ast.DataMembers empty,
        Ast.stmtClassMethods = Ast.Methods $ Data.Map.fromList $ methodify (Token.ClassName $ Token.Named $5 $28) $9 $17 
    }
}

ann_assign_simple: 'simple' '=' INT ',' { Nothing }
ann_assign_exp: 'value' '=' exp ',' { $3 }

-- *******************
-- *                 *
-- * stmt_ann_assign *
-- *                 *
-- *******************
stmt_ann_assign:
'AnnAssign'
'('
    'target' '=' var ','
    'annotation' '=' exp ','
    optional(ann_assign_exp)
    ann_assign_simple
    loc
')'
{
    Ast.StmtReturn $ Ast.StmtReturnContent
    {
        Ast.stmtReturnValue = Nothing,
        Ast.stmtReturnLocation = $13
    }
}

-- ******************
-- *                *
-- * exception_type *
-- *                *
-- ******************
exception_type: 'type' '=' exp ',' { $3 }


-- ******************
-- *                *
-- * exception_name *
-- *                *
-- ******************
exception_name: 'name' '=' ID ',' { $3 }

-- *********************
-- *                   *
-- * exception_handler *
-- *                   *
-- *********************
exception_handler:
'ExceptHandler'
'('
    optional(exception_type)
    optional(exception_name)
    'body' '=' stmts ','
    loc
')'
{
    Nothing
}

-- **********************
-- *                    *
-- * exception_handlers *
-- *                    *
-- **********************
exception_handlers: possibly_empty_listof(exception_handler) { $1 }

-- ************
-- *          *
-- * stmt_try *
-- *          *
-- ************
stmt_try:
'Try'
'('
    'body' '=' stmts ','
    'handlers' '=' exception_handlers ','
    'orelse' '=' stmts ','
    'finalbody' '=' stmts ','
    loc
')'
{
    Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = $5,
        Ast.stmtBlockLocation = $19
    }
}

-- ************
-- *          *
-- * stmt_for *
-- *          *
-- ************
stmt_for:
'For'
'('
    'target' '=' exp ','
    'iter' '=' exp ','
    'body' '=' stmts ','
    'orelse' '=' stmts ','
    loc
')'
{
    Ast.StmtWhile $ Ast.StmtWhileContent
    {
        Ast.stmtWhileCond = $9,
        Ast.stmtWhileBody = $13,
        Ast.stmtWhileLocation = $19
    }
}

yield_exp: 'value' '=' exp ',' { $3 }

-- *************
-- *           *
-- * exp_yield *
-- *           *
-- *************
exp_yield:
'Yield'
'('
    optional(yield_exp)
    loc
')'
{
    dummyExp $4
}

-- ************
-- *          *
-- * stmt_exp *
-- *          *
-- ************
stmt_exp:
'Expr'
'('
    'value' '=' exp ','
    loc
')'
{
    Ast.StmtExp $5
}

nonempty_raise_part: 'exc'   '=' exp ',' { $3 }
nonempty_cause_part: 'cause' '=' exp ',' { $3 }

-- **************
-- *            *
-- * stmt_raise *
-- *            *
-- **************
stmt_raise:
'Raise'
'('
    optional(nonempty_raise_part)
    optional(nonempty_cause_part)
    loc
')'
{
    Ast.StmtReturn $ Ast.StmtReturnContent
    {
        Ast.stmtReturnValue = $3,
        Ast.stmtReturnLocation = $5
    }
}

-- *****************
-- *               *
-- * stmt_continue *
-- *               *
-- *****************
stmt_continue:
'Continue'
'('
    loc
')'
{
    Ast.StmtContinue $ Ast.StmtContinueContent
    {
        Ast.stmtContinueLocation = $3
    }
}

assert_msg:
'msg' '=' exp ','
{
    Nothing
}

-- ***************
-- *             *
-- * stmt_assert *
-- *             *
-- ***************
stmt_assert:
'Assert'
'('
    'test' '=' exp ','
    optional(assert_msg)
    loc
')'
{
    Ast.StmtContinue $ Ast.StmtContinueContent
    {
        Ast.stmtContinueLocation = $8
    }
}

-- *************
-- *           *
-- * stmt_pass *
-- *           *
-- *************
stmt_pass: 'Pass' '(' loc ')'
{
    Ast.StmtContinue $ Ast.StmtContinueContent
    {
        Ast.stmtContinueLocation = $3
    }
}

-- **************
-- *            *
-- * stmt_break *
-- *            *
-- **************
stmt_break:
'Break'
'('
    loc
')'
{
    Ast.StmtBreak $ Ast.StmtBreakContent
    {
        Ast.stmtBreakLocation = $3
    }
}

-- ***************
-- *             *
-- * stmt_delete *
-- *             *
-- ***************
stmt_delete:
'Delete'
'('
    'targets' '=' nonempty_listof(var) ','
    loc
')'
{
    Ast.StmtBreak $ Ast.StmtBreakContent
    {
        Ast.stmtBreakLocation = $7
    }
}

-- ***************
-- *             *
-- * stmt_global *
-- *             *
-- ***************
stmt_global:
'Global'
'('
    'names' '=' nonempty_listof(tokenID) ','
    loc
')'
{
    Ast.StmtBreak $ Ast.StmtBreakContent
    {
        Ast.stmtBreakLocation = $7
    }
}


-- ********
-- *      *
-- * stmt *
-- *      *
-- ********
stmt:
stmt_if          { $1 } |
stmt_try         { $1 } |
stmt_exp         { $1 } |
stmt_for         { $1 } |
stmt_with        { $1 } |
stmt_class       { $1 } |
stmt_while       { $1 } |
stmt_break       { $1 } |
stmt_import      { $1 } |
stmt_raise       { $1 } |
stmt_delete      { $1 } |
stmt_return      { $1 } |
stmt_assign      { $1 } |
stmt_assert      { $1 } |
stmt_global      { $1 } |
stmt_function    { $1 } |
stmt_pass        { $1 } |
stmt_continue    { $1 } |
stmt_aug_assign  { $1 } |
stmt_ann_assign  { $1 } |
stmt_import_from { $1 }

-- *************
-- *           *
-- * type_hint *
-- *           *
-- *************
type_hint: 'annotation' '=' exp ',' { $3 }

-- *********
-- *       *
-- * param *
-- *       *
-- *********
param:
'arg'
'('
    'arg' '=' tokenID ','
    optional(type_hint)
    loc
')'
{
    Ast.Param
    {
        Ast.paramName = Token.ParamName $ Token.Named
        {
            Token.content = $5,
            Token.location = $8 { Location.colEnd = (Location.colStart $8) + (fromIntegral (length $5)) }
        },
        Ast.paramNominalType = Token.NominalTy $ Token.Named "any" $8,
        Ast.paramNominalTypeV2 = Nothing,
        Ast.paramSerialIdx = 156
    }
}

defaults:
'['                  ']' { Nothing } |
'[' commalistof(exp) ']' { Nothing }

kw_default: exp { Nothing } | none { Nothing }

vararg: 'vararg' '=' param ',' { $3 }
kwarg:  'kwarg'  '=' param ',' { $3 } 

-- **********
-- *        *
-- * params *
-- *        *
-- **********
params:
'arguments'
'('
    'posonlyargs' '=' '[' ']' ','
    'args' '=' possibly_empty_listof(param) ','
    optional(vararg)
    'kwonlyargs' '=' possibly_empty_listof(param) ','
    'kw_defaults' '=' possibly_empty_listof(kw_default) ','
    optional(kwarg)
    'defaults' '=' defaults
')'
{
    $10
}

function_def:
'FunctionDef'      { Nothing } |
'AsyncFunctionDef' { Nothing }

return_type_hint: 'returns' '=' exp ',' { Nothing }

-- *****************
-- *               *
-- * stmt_function *
-- *               *
-- *****************
stmt_function:
function_def
'('
    'name' '=' ID ','
    'args' '=' params ','
    'body' '=' stmts ','
    'decorator_list' '=' exps ','
    optional(return_type_hint) 
    'type_params' '=' '[' ']' ','
    loc
')'
{
    Ast.StmtFunc $ Ast.StmtFuncContent
    {
        Ast.stmtFuncReturnType = Token.NominalTy $ Token.Named
        {
            Token.content = "any",
            Token.location = $25
        },
        Ast.stmtFuncName = Token.FuncName $ Token.Named
        {
            Token.content = unquote (tokIDValue $5),
            Token.location = $25
        },
        Ast.stmtFuncParams = $9,
        Ast.stmtFuncBody = $13,
        Ast.stmtFuncAnnotations = $17,
        Ast.stmtFuncLocation = $25
    }
}

-- *******
-- *     *
-- * ctx *
-- *     *
-- *******
ctx:
'Store' '(' ')' { Nothing } |
'Del'   '(' ')' { Nothing } |
'Load'  '(' ')' { Nothing }

-- ********
-- *      *
-- * name *
-- *      *
-- ********
name:
'Name'
'('
    'id' '=' tokenID ','
    'ctx' '=' ctx ','
    loc
')'
{
    Token.Named
    {
        Token.content = $5,
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
    'targets' '=' nonempty_listof(exp) ','
    'value' '=' exp ','
    loc
')'
{
    Ast.StmtAssign $ Ast.StmtAssignContent
    {
        Ast.stmtAssignLhs = case $5 of
            ((Ast.ExpVar (Ast.ExpVarContent v)):_) -> v
            _ -> Ast.VarSimple (Ast.VarSimpleContent (Token.VarName (Token.Named "dummyAssignVar" $11))),
        Ast.stmtAssignRhs = $9
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
    Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = simportify $9 $6,
        Ast.stmtBlockLocation = $9
    }
}

import_from_module: 'module' '=' tokenID ',' { $3 }


-- ********************
-- *                  *
-- * stmt_import_from *
-- *                  *
-- ********************
stmt_import_from:
'ImportFrom'
'('
    optional(import_from_module)
    'names' '=' '[' commalistof(alias) ']' ','
    'level' '=' INT ','
    loc
')'
{
    Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = importify $14 $3 $7,
        Ast.stmtBlockLocation = $14
    }
}

-- ***********
-- *         *
-- * tokenID *
-- *         *
-- ***********
tokenID: ID { unquote (tokIDValue $1) }

-- *********
-- *       *
-- * alias *
-- *       *
-- *********
alias:
'alias'
'('
    'name' '=' tokenID ','
    optional(asname)
    loc
')'
{
    ($5, $7)
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
        lineStart = fromIntegral (tokIntValue $3),
        colStart = 1 + (fromIntegral (tokIntValue $7)),
        lineEnd = fromIntegral (tokIntValue $11),
        colEnd = 1 + (fromIntegral (tokIntValue $15))
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

methodify :: Token.ClassName -> [ Ast.Var ] -> [ Ast.Stmt ] -> [(Token.MethdName, Ast.StmtMethodContent)]
methodify c vars stmts = catMaybes $ Data.List.map (methodify' c vars) stmts

methodify' :: Token.ClassName -> [ Ast.Var ] -> Ast.Stmt -> Maybe (Token.MethdName, Ast.StmtMethodContent)
methodify' c vars (Ast.StmtFunc f) = Just (methodify'' c vars f)
methodify' _ _ _ = Nothing

methodify'' :: Token.ClassName -> [ Ast.Var ] -> Ast.StmtFuncContent -> (Token.MethdName, Ast.StmtMethodContent)
methodify'' c vars f = let m = Token.MethdName $ Token.getFuncNameToken (Ast.stmtFuncName f) in (m, Ast.StmtMethodContent {
    Ast.stmtMethodReturnType = (Ast.stmtFuncReturnType f),
    Ast.stmtMethodName = m,
    Ast.stmtMethodParams = (Ast.stmtFuncParams f),
    Ast.stmtMethodBody = (Ast.stmtFuncBody f),
    Ast.stmtMethodLocation = (Ast.stmtFuncLocation f),
    Ast.hostingClassName = c,
    Ast.hostingClassSupers = superify vars
})

expmyname :: Token.Named -> Ast.Exp
expmyname = Ast.ExpVar . Ast.ExpVarContent . Ast.VarSimple . Ast.VarSimpleContent . Token.VarName

unquote :: String -> String
unquote s = let n = length s in take (n-2) (drop 1 s)

simportify :: Location -> [(String, Maybe String)] -> [ Ast.Stmt ]
simportify loc args = Data.List.map (simportify' loc) args

simportify' :: Location -> (String, Maybe String) -> Ast.Stmt
simportify' loc (src, Nothing) = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = src,
    Ast.stmtImportFromSource = Nothing,
    Ast.stmtImportAlias = Nothing,
    Ast.stmtImportLocation = loc
}
simportify' loc (src, Just alias) = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = src,
    Ast.stmtImportFromSource = Nothing,
    Ast.stmtImportAlias = Just alias,
    Ast.stmtImportLocation = loc
}

importify :: Location -> (Maybe String) -> [(String, Maybe String)] -> [ Ast.Stmt ] 
importify loc Nothing args = Data.List.map (importify' loc) args
importify loc (Just src) args = Data.List.map (importify'' loc src) args

importify' :: Location -> (String, Maybe String) -> Ast.Stmt
importify' loc (specific, Nothing) = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = "",
    Ast.stmtImportFromSource = Just specific,
    Ast.stmtImportAlias = Nothing,
    Ast.stmtImportLocation = loc
}
importify' loc (specific, Just alias) = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource  = "",
    Ast.stmtImportFromSource = Just specific,
    Ast.stmtImportAlias = Just alias,
    Ast.stmtImportLocation = loc
}

importify'' :: Location -> String -> (String, Maybe String) -> Ast.Stmt
importify'' loc src (specific, Nothing) = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = src,
    Ast.stmtImportFromSource = Just specific,
    Ast.stmtImportAlias = Nothing,
    Ast.stmtImportLocation = loc
}
importify'' loc src (specific, Just alias) = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = src,
    Ast.stmtImportFromSource = Just specific,
    Ast.stmtImportAlias = Just alias,
    Ast.stmtImportLocation = loc
}

extractParamSingleName' :: [ Token.ParamName ] -> Maybe Token.ParamName
extractParamSingleName' ps = case ps of { [p] -> Just p; _ -> Nothing }
 
extractParamSingleName :: [ Either Token.ParamName Token.NominalTy ] -> Maybe Token.ParamName
extractParamSingleName = extractParamSingleName' . lefts  

extractParamNominalType' :: [ Token.NominalTy ] -> Maybe Token.NominalTy
extractParamNominalType' ts = case ts of { [t] -> Just t; _ -> Nothing }
 
extractParamNominalType :: [ Either Token.ParamName Token.NominalTy ] -> Maybe Token.NominalTy
extractParamNominalType = extractParamNominalType' . rights 

superify' :: Ast.Var -> Token.SuperName
superify' (Ast.VarSimple (Ast.VarSimpleContent (Token.VarName v))) = Token.SuperName v
superify' (Ast.VarField (Ast.VarFieldContent _ (Token.FieldName v) _)) = Token.SuperName v
superify' (Ast.VarSubscript (Ast.VarSubscriptContent _ _ l)) = Token.SuperName (Token.Named "subscript" l)

superify :: [ Ast.Var ] -> [ Token.SuperName ]
superify = Data.List.map superify' 

paramify :: [ Either Token.ParamName Token.NominalTy ] -> Location -> Maybe Ast.Param
paramify attrs l = let
    name = extractParamSingleName attrs
    nominalType = extractParamNominalType attrs
    in case (name, nominalType) of { (Just n, Just t) -> Just $ Ast.Param n t Nothing 0; _ -> Nothing }

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
        head = Param { paramName = n, paramNominalType = t, paramNominalTypeV2 = Nothing, paramSerialIdx = i }
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
parseProgram :: FilePath -> Maybe String -> String -> Either String Ast.Root
parseProgram = runAlex' parse
}

