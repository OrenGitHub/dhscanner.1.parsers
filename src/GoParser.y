{
{-# OPTIONS -Werror=missing-fields #-}

module GoParser( parseProgram ) where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
import Ast
import GoLexer
import Location
import qualified Token

-- *******************
-- *                 *
-- * general imports *
-- *                 *
-- *******************
import Data.Maybe
import Data.Either
import Data.List ( map, isPrefixOf )
import Data.List.Split ( splitOn )
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

'(' { AlexTokenTag AlexRawToken_LPAREN _ }
')' { AlexTokenTag AlexRawToken_RPAREN _ }
'[' { AlexTokenTag AlexRawToken_LBRACK _ }
']' { AlexTokenTag AlexRawToken_RBRACK _ }
'{' { AlexTokenTag AlexRawToken_LBRACE _ }
'}' { AlexTokenTag AlexRawToken_RBRACE _ }

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************

':' { AlexTokenTag AlexRawToken_COLON _ }
',' { AlexTokenTag AlexRawToken_COMMA _ }
'.' { AlexTokenTag AlexRawToken_DOT   _ }
'=' { AlexTokenTag AlexRawToken_EQ    _ }

-- *************
-- *           *
-- * operators *
-- *           *
-- *************

'-'  { AlexTokenTag AlexRawToken_MINUS _ }
'*'  { AlexTokenTag AlexRawToken_TIMES _ }
'+'  { AlexTokenTag AlexRawToken_OP_PLUS _ }
'@'  { AlexTokenTag AlexRawToken_AT _    }
'&'  { AlexTokenTag AlexRawToken_ampersand _ }
':=' { AlexTokenTag AlexRawToken_OP_ASSIGN _ }
'!=' { AlexTokenTag AlexRawToken_OP_NEQ _ }
'!'  { AlexTokenTag AlexRawToken_OP_BANG _ }
'&&' { AlexTokenTag AlexRawToken_OP_AND _ }
'>=' { AlexTokenTag AlexRawToken_OP_GEQ _ }

-- ************
-- *          *
-- * keywords *
-- *          *
-- ************

'*ast.File' { AlexTokenTag AlexRawToken_astFile _ }
'Doc' { AlexTokenTag AlexRawToken_Doc _ }
'nil' { AlexTokenTag AlexRawToken_nil _ }
'Package' { AlexTokenTag AlexRawToken_Package _ }
'Name' { AlexTokenTag AlexRawToken_Name _ }
'*ast.Ident' { AlexTokenTag AlexRawToken_astIdent _ }
'NamePos' { AlexTokenTag AlexRawToken_NamePos _ }
'Obj' { AlexTokenTag AlexRawToken_Obj _ }
'Decls' { AlexTokenTag AlexRawToken_Decls _ }
'ast.Decl' { AlexTokenTag AlexRawToken_astDecl _ }
'*ast.GenDecl' { AlexTokenTag AlexRawToken_astGenDecl _ }
'TokPos' { AlexTokenTag AlexRawToken_TokPos _ }
'Tok' { AlexTokenTag AlexRawToken_Tok _ }
'Lparen' { AlexTokenTag AlexRawToken_Lparen _ }
'Specs' { AlexTokenTag AlexRawToken_Specs _ }
'ast.Spec' { AlexTokenTag AlexRawToken_astSpec _ }
'len' { AlexTokenTag AlexRawToken_len _ }
'import' { AlexTokenTag AlexRawToken_import _ }
'*ast.ImportSpec' { AlexTokenTag AlexRawToken_astImportSpec _ }
'*ast.BasicLit' { AlexTokenTag AlexRawToken_astBasicLit _ }
'ValuePos' { AlexTokenTag AlexRawToken_ValuePos _ }
'Kind' { AlexTokenTag AlexRawToken_Kind _ }
'Value' { AlexTokenTag AlexRawToken_Value _ }
'Path' { AlexTokenTag AlexRawToken_Path _ }
'Comment' { AlexTokenTag AlexRawToken_Comment _ }
'EndPos' { AlexTokenTag AlexRawToken_EndPos _ }
'STRING' { AlexTokenTag AlexRawToken_STRING _ }
'Rparen' { AlexTokenTag AlexRawToken_Rparen _ }
'var' { AlexTokenTag AlexRawToken_var _ }
'Names' { AlexTokenTag AlexRawToken_Names _ }
'Values' { AlexTokenTag AlexRawToken_Values _ }
'ast.Expr' { AlexTokenTag AlexRawToken_astExpr _ }
'*ast.ValueSpec' { AlexTokenTag AlexRawToken_astValueSpec _ }
'Types' { AlexTokenTag AlexRawToken_Types _ }
'Type' { AlexTokenTag AlexRawToken_Type _ }
'Decl' { AlexTokenTag AlexRawToken_Decl _ }
'Data' { AlexTokenTag AlexRawToken_Data _ }
'obj' { AlexTokenTag AlexRawToken_obj _ }
'*ast.Object' { AlexTokenTag AlexRawToken_astObject _ }
'*ast.CallExpr' { AlexTokenTag AlexRawToken_astCallExpr _ }
'Fun' { AlexTokenTag AlexRawToken_Fun _ }
'Args' { AlexTokenTag AlexRawToken_Args _ }
'Ellipsis' { AlexTokenTag AlexRawToken_Ellipsis _ }
'*ast.IndexExpr' { AlexTokenTag AlexRawToken_astIndexExpr _ }
'X' { AlexTokenTag AlexRawToken_X _ }
'Lbrack' { AlexTokenTag AlexRawToken_Lbrack _ }
'Rbrack' { AlexTokenTag AlexRawToken_Rbrack _ }
'Index' { AlexTokenTag AlexRawToken_Index _ }
'*ast.SelectorExpr' { AlexTokenTag AlexRawToken_astSelectorExpr _ }
'Sel' { AlexTokenTag AlexRawToken_Sel _ }
'Y' { AlexTokenTag AlexRawToken_Y _ }
'Op' { AlexTokenTag AlexRawToken_Op _ }
'OpPos' { AlexTokenTag AlexRawToken_OpPos _ }
'*ast.BinaryExpr' { AlexTokenTag AlexRawToken_astBinaryExpr _ }
'INT' { AlexTokenTag AlexRawToken_KW_INT _ }
'type' { AlexTokenTag AlexRawToken_type _ }
'*ast.Field' { AlexTokenTag AlexRawToken_astField _ }
'Tag' { AlexTokenTag AlexRawToken_Tag _ }
'*ast.FieldList' { AlexTokenTag AlexRawToken_astFieldList _ }
'Opening' { AlexTokenTag AlexRawToken_Opening _ }
'List' { AlexTokenTag AlexRawToken_List _ }
'Closing' { AlexTokenTag AlexRawToken_Closing _ }
'*ast.StructType' { AlexTokenTag AlexRawToken_astStructType _ }
'Struct' { AlexTokenTag AlexRawToken_Struct _ }
'Fields' { AlexTokenTag AlexRawToken_Fields _ }
'Incomplete' { AlexTokenTag AlexRawToken_Incomplete _ }
'false' { AlexTokenTag AlexRawToken_false _ }
'*ast.TypeSpec' { AlexTokenTag AlexRawToken_astTypeSpec _ }
'TypeParams' { AlexTokenTag AlexRawToken_TypeParams _ }
'Assign' { AlexTokenTag AlexRawToken_Assign _ }
'*ast.FuncType' { AlexTokenTag AlexRawToken_astFuncType _ }
'Func' { AlexTokenTag AlexRawToken_Func _ }
'Params' { AlexTokenTag AlexRawToken_Params _ }
'Results' { AlexTokenTag AlexRawToken_Results _ }
'*ast.BlockStmt' { AlexTokenTag AlexRawToken_astBlockStmt _ }
'Lbrace' { AlexTokenTag AlexRawToken_Lbrace _ }
'Rbrace' { AlexTokenTag AlexRawToken_Rbrace _ }
'*ast.FuncDecl' { AlexTokenTag AlexRawToken_astFuncDecl _ }
'Recv' { AlexTokenTag AlexRawToken_Recv _ }
'Body' { AlexTokenTag AlexRawToken_Body _ }
'func' { AlexTokenTag AlexRawToken_func _ }
'Star' { AlexTokenTag AlexRawToken_Star _ }
'*ast.StarExpr' { AlexTokenTag AlexRawToken_astStarExpr _ }
'ast.Stmt' { AlexTokenTag AlexRawToken_astStmt _ }
'*ast.DeclStmt' { AlexTokenTag AlexRawToken_astDeclStmt _ }
'If' { AlexTokenTag AlexRawToken_If _ }
'Init' { AlexTokenTag AlexRawToken_Init _ }
'Cond' { AlexTokenTag AlexRawToken_Cond _ }
'Else' { AlexTokenTag AlexRawToken_Else _ }
'*ast.IfStmt' { AlexTokenTag AlexRawToken_astIfStmt _ }
'Lhs' { AlexTokenTag AlexRawToken_Lhs _ }
'Rhs' { AlexTokenTag AlexRawToken_Rhs _ }
'*ast.AssignStmt' { AlexTokenTag AlexRawToken_astAssignStmt _ }
'*ast.UnaryExpr' { AlexTokenTag AlexRawToken_astUnaryExpr _ }
'*ast.ExprStmt' { AlexTokenTag AlexRawToken_astExprStmt _ }
'*ast.ReturnStmt' { AlexTokenTag AlexRawToken_astReturnStmt _ }
'Return' { AlexTokenTag AlexRawToken_Return _ }
'Key' { AlexTokenTag AlexRawToken_Key _ }
'Colon' { AlexTokenTag AlexRawToken_Colon _ }
'Elts' { AlexTokenTag AlexRawToken_Elts _ }
'*ast.KeyValueExpr' { AlexTokenTag AlexRawToken_astKeyValueExpr _ }
'*ast.CompositeLit' { AlexTokenTag AlexRawToken_astCompositeLit _ }
'*ast.TypeAssertExpr' { AlexTokenTag AlexRawToken_astTypeAssertExpr _ }
'FileStart' { AlexTokenTag AlexRawToken_FileStart _ }
'FileEnd' { AlexTokenTag AlexRawToken_FileEnd _ }
'Scope' { AlexTokenTag AlexRawToken_Scope _ }
'Imports' { AlexTokenTag AlexRawToken_Imports _ }
'Unresolved' { AlexTokenTag AlexRawToken_Unresolved _ }
'Comments' { AlexTokenTag AlexRawToken_Comments _ }
'Outer' { AlexTokenTag AlexRawToken_Outer _ }
'Objects' { AlexTokenTag AlexRawToken_Objects _ }
'map' { AlexTokenTag AlexRawToken_map _ }
'string' { AlexTokenTag AlexRawToken_string _ }
'*ast.Scope' { AlexTokenTag AlexRawToken_astScope _ }
'Low' { AlexTokenTag AlexRawToken_Low _ }
'High' { AlexTokenTag AlexRawToken_High _ }
'*ast.SliceExpr' { AlexTokenTag AlexRawToken_astSliceExpr _ }
'Max' { AlexTokenTag AlexRawToken_Max _ }
'Slice3' { AlexTokenTag AlexRawToken_Slice3 _ }
-- last keywords first part

-- ************
-- *          *
-- * location *
-- *          *
-- ************

-- ************
-- *          *
-- * integers *
-- *          *
-- ************

INT { AlexTokenTag (AlexRawToken_INT i) _ }
ID { AlexTokenTag (AlexRawToken_ID s) _ }
QUOTED_ID { AlexTokenTag (AlexRawToken_QUOTED_ID s) _ }

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

-- **********
-- *        *
-- * ornull *
-- *        *
-- **********
ornull(a): 'nil' { Nothing } | a { Just $1 }
orempty(a): 'nil' { [] } | a { $1 }

-- **********************
-- *                    *
-- * parametrized lists *
-- *                    *
-- **********************
listof(a): a { [$1] }

-- *********************
-- *                   *
-- * Ast root: program *
-- *                   *
-- *********************
program:
'*ast.File'
'{'
    'Doc' ':' 'nil'
    'Package' ':' filename ':' location
    'Name' ':' identifier 
    'Decls' ':' stmts
    'FileStart' ':' filename ':' location
    'FileEnd' ':' filename ':' location
    'Scope' ':' scope
    'Imports' ':' imports
    'Unresolved' ':' unresolved
    'Comments' ':' 'nil'
'}'
{
    Ast.Root
    {
        Ast.filename = "DDD",
        stmts = $16
    }
}

scope_object: QUOTED_ID ':' '*' '(' 'obj' '@' INT ')' { Nothing }

scope_objects:
scope_object { Nothing } |
scope_object scope_objects { Nothing }

scope:
'*ast.Scope'
'{'
    'Outer' ':' 'nil'
    'Objects' ':' 'map' '[' 'string' ']' '*ast.Object' '(' 'len' '=' INT ')' '{' scope_objects '}'
'}'
{
    Nothing
}

imported_obj: INT ':' '*' '(' 'obj' '@' INT ')' { Nothing }

imported_objs:
imported_obj { Nothing } |
imported_obj imported_objs { Nothing }

imports:
'[' ']' '*ast.ImportSpec' '(' 'len' '=' INT ')' '{' imported_objs '}'
{
    Nothing
}

unresolved_obj: INT ':' '*' '(' 'obj' '@' INT ')' { Nothing }

unresolved_objs:
unresolved_obj { Nothing } |
unresolved_obj unresolved_objs { Nothing }

unresolved:
'[' ']' '*ast.Ident' '(' 'len' '=' INT ')' '{' unresolved_objs '}'
{
    Nothing
}

tokenID:
'import' { "import" } |
'var'    { "var"    } |
'nil'    { "null"   } |
'type'   { "type"   } |
'func'   { "func"   }

stmts:
'[' ']' 'ast.Decl' '(' 'len' '=' INT ')'
'{'
    numbered_stmts
'}'
{
    $10
}

numbered_stmts:
numbered_stmt { [$1] } |
numbered_stmt numbered_stmts { $1:$2 }

specs:
'[' ']' 'ast.Spec' '(' 'len' '=' INT ')'
'{'
    numbered_stmts
'}'
{
    $10
}

paren:
filename ':' location { Nothing } |
'-'                   { Nothing }

stmt_gendecl:
'*ast.GenDecl'
'{'
    'Doc' ':' 'nil'
    'TokPos' ':' filename ':' location
    'Tok' ':' tokenID
    'Lparen' ':' paren
    'Specs' ':' specs
    'Rparen' ':' paren
'}'
{
    Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = $19,
        Ast.stmtBlockLocation = $10 
    }
}

exp_str:
'*ast.BasicLit'
'{'
    'ValuePos' ':' filename ':' location
    'Kind' ':' 'STRING'
    'Value' ':' QUOTED_ID
'}'
{
    Token.ConstStr
    {
        Token.constStrValue = tokIDValue $13,
        Token.constStrLocation = $7 {
            Location.colEnd = (Location.colEnd $7) + (fromIntegral (length (tokIDValue $13)))
        }
    }
}

exp_int:
'*ast.BasicLit'
'{'
    'ValuePos' ':' filename ':' location
    'Kind' ':' 'INT'
    'Value' ':' QUOTED_ID
'}'
{
    Token.ConstInt
    {
        Token.constIntValue = 8888,
        Token.constIntLocation = $7
    }
}

exp_call:
'*ast.CallExpr'
'{'
    'Fun' ':' exp
    'Lparen' ':' filename ':' location
    'Args' ':' orempty(values)
    'Ellipsis' ':' '-'
    'Rparen' ':' filename ':' location
'}'
{
    let loc = case (startloc $5) of { Nothing -> $10; Just l -> l } in Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = $5,
        Ast.args = $13,
        Ast.expCallLocation = loc {
            Location.lineEnd = Location.lineEnd $21,
            Location.colEnd = Location.colEnd $21
        }
    }
}

var_subscript:
'*ast.IndexExpr'
'{'
    'X' ':' exp
    'Lbrack' ':' filename ':' location
    'Index' ':' exp
    'Rbrack' ':' filename ':' location
'}'
{
    Ast.VarSubscript $ Ast.VarSubscriptContent
    {
        Ast.varSubscriptLhs = $5,
        Ast.varSubscriptIdx = $13,
        Ast.varSubscriptLocation = $10
    }
}

var_field:
'*ast.SelectorExpr'
'{'
    'X' ':' exp
    'Sel' ':' identifier
'}'
{
    Ast.VarField $ Ast.VarFieldContent
    {
        Ast.varFieldLhs = $5,
        Ast.varFieldName = Token.FieldName $8,
        Ast.varFieldLocation = Token.location $8
    }
}

var_simple: identifier { Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $1 }

var:
var_simple    { $1 } |
var_field     { $1 } |
var_subscript { $1 }

exp_var:
var { Ast.ExpVar $ Ast.ExpVarContent $1 }

operator:
'*'  { Nothing } |
'+'  { Nothing } |
'-'  { Nothing } |
'!'  { Nothing } |
'&'  { Nothing } |
'&&' { Nothing } |
':=' { Nothing } |
'='  { Nothing } |
'>=' { Nothing } |
'!=' { Nothing }

exp_binop:
'*ast.BinaryExpr'
'{'
    'X' ':' exp
    'OpPos' ':' filename ':' location
    'Op' ':' operator
    'Y' ':' exp
'}'
{
    Ast.ExpBinop $ Ast.ExpBinopContent
    {
        Ast.expBinopLeft = $5,
        Ast.expBinopRight = $16,
        Ast.expBinopOperator = Ast.PLUS,
        Ast.expBinopLocation = $10
    }
}

exp_star:
'*ast.StarExpr'
'{'
    'Star' ':' filename ':' location
    'X' ':' exp
'}'
{
    $10
}

exp_unop:
'*ast.UnaryExpr'
'{'
    'OpPos' ':' filename ':' location
    'Op' ':' operator
    'X' ':' exp
'}'
{
    $13
}

kv:
'*ast.KeyValueExpr'
'{'
    'Key' ':' exp
    'Colon' ':' filename ':' location
    'Value' ':' exp
'}'
{
    $13
}

numbered_kv: INT ':' kv { $3 }

numbered_kvs:
numbered_kv { [$1] } |
numbered_kv numbered_kvs { $1:$2 }

exp_dict:
'*ast.CompositeLit'
'{'
    'Type' ':' exp
    'Lbrace' ':' filename ':' location
    'Elts' ':' '[' ']' 'ast.Expr' '(' 'len' '=' INT ')' '{' numbered_kvs '}'
    'Rbrace' ':' filename ':' location
    'Incomplete' ':' 'false'
'}'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named {
            Token.content = "dictify",
            Token.location = $10
        },
        Ast.args = $22,
        Ast.expCallLocation = $10
    }
}

exp_ty_assert:
'*ast.TypeAssertExpr'
'{'
    'X' ':' exp
    'Lparen' ':' filename ':' location
    'Type' ':' exp
    'Rparen' ':' filename ':' location
'}'
{
    $5
}

exp_slice:
'*ast.SliceExpr'
'{'
    'X' ':' exp
    'Lbrack' ':' filename ':' location
    'Low' ':' ornull(exp)
    'High' ':' ornull(exp)
    'Max' ':' 'nil'
    'Slice3' ':' 'false'
    'Rbrack' ':' filename ':' location
'}'
{
    $5
}

exp:
exp_str { Ast.ExpStr $ Ast.ExpStrContent $1 } |
exp_int { Ast.ExpInt $ Ast.ExpIntContent $1 } |
exp_var { $1 } |
exp_dict { $1 } |
exp_call { $1 } |
exp_star { $1 } |
exp_unop { $1 } |
exp_slice { $1 } |
exp_binop { $1 } |
exp_ty_assert { $1 }

stmt_import:
'*ast.ImportSpec'
'{'
    'Doc' ':' 'nil'
    'Name' ':' 'nil'
    'Path' ':' exp_str
    'Comment' ':' 'nil'
    'EndPos' ':' '-'
'}'
{
    let s = (Token.constStrValue $11) in Ast.StmtImport $ Ast.StmtImportContent
    {
        Ast.stmtImportSource = s,
        Ast.stmtImportFromSource = Nothing,
        Ast.stmtImportAlias = Just (extractPackageName s),
        Ast.stmtImportLocation = Token.constStrLocation $11
    }
}

numbered_identifier:
INT ':' identifier { $3 }

numbered_identifiers:
numbered_identifier { [$1] } |
numbered_identifier numbered_identifiers { $1:$2 }

names:
'[' ']' '*ast.Ident' '(' 'len' '=' INT ')'
'{'
    numbered_identifiers
'}'
{
    $10
}

numbered_exp:
INT ':' exp { $3 }

numbered_exps:
numbered_exp { [$1] } |
numbered_exp numbered_exps { $1:$2 }

values:
'[' ']' 'ast.Expr' '(' 'len' '=' INT ')'
'{'
    numbered_exps
'}'
{
    $10
}

stmt_decvar:
'*ast.ValueSpec'
'{'
    'Doc' ':' 'nil'
    'Names' ':' names
    'Type' ':' ornull(exp)
    'Values' ':' orempty(values)
    'Comment' ':' 'nil'
'}'
{
    Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = vardecify $8 $14,
        Ast.stmtBlockLocation = Location "" 1 1 1 1
    }
}

field:
'*ast.Field'
'{'
    'Doc' ':' 'nil'
    'Names' ':' orempty(names)
    'Type' ':' exp
    'Tag' ':' ornull(exp_str)
    'Comment' ':' 'nil'
'}'
{
    case $8 of {
        [] -> Nothing;
        (name:_) -> case (nameExp $11) of {
            Nothing -> Nothing;
            Just nominalType -> Just Ast.Param {
                Ast.paramName = Token.ParamName name,
                Ast.paramNominalType = Token.NominalTy name,
                Ast.paramNominalTypeV2 = Just nominalType,
                Ast.paramSerialIdx = 0
            }
        }
    } 
}

numbered_field:
INT ':' field { $3 }

numbered_fields:
numbered_field { [$1] } |
numbered_field numbered_fields { $1:$2 }

fields_list: '[' ']' '*ast.Field' '(' 'len' '=' INT ')'
'{'
    numbered_fields
'}'
{
    catMaybes $10
}

fields:
'*ast.FieldList'
'{'
    'Opening' ':' filename ':' location
    'List' ':' fields_list
    'Closing' ':' filename ':' location
'}'
{
    $10
}

type_struct:
'*ast.StructType'
'{'
    'Struct' ':' filename ':' location
    'Fields' ':' fields
    'Incomplete' ':' 'false'
'}'
{
    $10
}

stmt_class:
'*ast.TypeSpec'
'{'
    'Doc' ':' 'nil'
    'Name' ':' identifier
    'TypeParams' ':' 'nil'
    'Assign' ':' '-'
    'Type' ':' type_struct
    'Comment' ':' 'nil'
'}'
{
    Ast.StmtClass $ Ast.StmtClassContent
    {
        Ast.stmtClassName = Token.ClassName $8,
        Ast.stmtClassSupers = [],
        Ast.stmtClassDataMembers = Ast.DataMembers Data.Map.empty,
        Ast.stmtClassMethods = Ast.Methods Data.Map.empty
    }
}

stmt_type:
stmt_class { $1 }

type_func:
'*ast.FuncType'
'{'
    'Func' ':' filename ':' location
    'TypeParams' ':' 'nil'
    'Params' ':' fields
    'Results' ':' 'nil'
'}'
{
    $13
}

block_stmts:
'[' ']' 'ast.Stmt' '(' 'len' '=' INT ')'
'{'
    numbered_stmts
'}'
{
    $10
}

stmt_block:
'*ast.BlockStmt'
'{'
    'Lbrace' ':' filename ':' location
    'List' ':' block_stmts
    'Rbrace' ':' filename ':' location
'}'
{
    $10
}

stmt_func:
'*ast.FuncDecl'
'{'
    'Doc' ':' 'nil'
    'Recv' ':' 'nil'
    'Name' ':' identifier
    'Type' ':' type_func
    'Body' ':' stmt_block
'}'
{
    Ast.StmtFunc $ Ast.StmtFuncContent
    {
        Ast.stmtFuncReturnType = Token.NominalTy (Token.Named "any" (Token.location $11)),
        Ast.stmtFuncName = Token.FuncName $11,
        Ast.stmtFuncParams = $14,
        Ast.stmtFuncBody = $17,
        Ast.stmtFuncAnnotations = [],
        Ast.stmtFuncLocation = Token.location $11 
    }
}

decl:
'*ast.GenDecl'
'{'
    'Doc' ':' 'nil'
    'TokPos' ':' filename ':' location
    'Tok' ':' 'var'
    'Lparen' ':' '-'
    'Specs' ':' specs
    'Rparen' ':' '-'
'}'
{
    Ast.StmtBlock $ Ast.StmtBlockContent $19 $10
}

stmt_decl:
'*ast.DeclStmt'
'{'
    'Decl' ':' decl
'}'
{
    $5
}

stmt_if:
'*ast.IfStmt'
'{'
    'If' ':' filename ':' location
    'Init' ':' ornull(stmt)
    'Cond' ':' exp
    'Body' ':' stmt_block
    'Else' ':' orempty(stmt_block)
'}'
{
    Ast.StmtIf $ Ast.StmtIfContent
    {
        Ast.stmtIfCond = $13,
        Ast.stmtIfBody = $16,
        Ast.stmtElseBody = [],
        Ast.stmtIfLocation = $7
    }
}

numbered_var:
INT ':' var { $3 }

numbered_vars:
numbered_var { ($1,[]) } |
numbered_var numbered_vars { ($1,(fst $2):(snd $2)) }

stmt_assign:
'*ast.AssignStmt'
'{'
    'Lhs' ':' '[' ']' 'ast.Expr' '(' 'len' '=' INT ')' '{' numbered_vars '}'
    'TokPos' ':' filename ':' location
    'Tok' ':' operator
    'Rhs' ':' '[' ']' 'ast.Expr' '(' 'len' '=' INT ')' '{' numbered_exps '}'
'}'
{
    Ast.StmtAssign $ Ast.StmtAssignContent
    {
        Ast.stmtAssignLhs = fst $14,
        Ast.stmtAssignRhs = Ast.ExpCall $ Ast.ExpCallContent {
            Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named {
                Token.content = "expify",
                Token.location = $20
            },
            Ast.args = $35,
            Ast.expCallLocation = $20
        }
    }
}

stmt_exp:
'*ast.ExprStmt'
'{'
    'X' ':' exp
'}'
{
    Ast.StmtExp $5
}

stmt_return:
'*ast.ReturnStmt'
'{'
    'Return' ':' filename ':' location
    'Results' ':' ornull(exp)
'}'
{
    Ast.StmtReturn $ Ast.StmtReturnContent
    {
        Ast.stmtReturnValue = $10,
        Ast.stmtReturnLocation = $7
    }
}

stmt_obj:
'*' '(' 'obj' '@' INT ')'
{
    StmtExp $ ExpInt $ ExpIntContent $ Token.ConstInt 0 (Location "" 1 1 1 1)
}

stmt:
stmt_assign  { $1 } |
stmt_gendecl { $1 } |
stmt_type    { $1 } |
stmt_if      { $1 } |
stmt_obj     { $1 } |
stmt_exp     { $1 } |
stmt_func    { $1 } |
stmt_import  { $1 } |
stmt_return  { $1 } |
stmt_decl    { $1 } |
stmt_decvar  { $1 }

numbered_stmt:
INT ':' stmt
{
    $3
}

objectdata:
INT { Nothing } |
'nil' { Nothing }

object_1:
'*ast.Object'
'{'
    'Kind' ':' tokenID
    'Name' ':' QUOTED_ID
    'Decl' ':' stmt
    'Data' ':' objectdata
    'Type' ':' tokenID
'}'
{
    Nothing
}

object_2: '*' '(' 'obj' '@' INT ')' { Nothing }

object:
object_1 { $1 } |
object_2 { $1 }

identifier:
'*ast.Ident'
'{'
    'NamePos' ':' filename ':' location
    'Name' ':' QUOTED_ID
    'Obj' ':' ornull(object)
'}'
{
    Token.Named
    {
        Token.content = tokIDValue $10,
        Token.location = Location {
            Location.filename = Location.filename $7,
            Location.lineStart = Location.lineStart $7,
            Location.colStart = Location.colStart $7,
            Location.lineEnd = Location.lineStart $7,
            Location.colEnd = (Location.colStart $7) + (fromIntegral (length (tokIDValue $10)))
        }
    }
}

filename:
ID '.' ID { Nothing }

location:
INT ':' INT
{
    Location
    {
        Location.filename = getFilename $1,
        lineStart = fromIntegral (tokIntValue $1),
        colStart = (fromIntegral (tokIntValue $3)),
        lineEnd = fromIntegral (tokIntValue $1),
        colEnd = (fromIntegral (tokIntValue $3))
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

stringifyVarSimple :: Ast.VarSimpleContent -> String
stringifyVarSimple (Ast.VarSimpleContent (Token.VarName (Token.Named v _))) = v

stringifyVarField :: Ast.VarFieldContent -> String
stringifyVarField (Ast.VarFieldContent (Ast.ExpVar (Ast.ExpVarContent v)) (Token.FieldName (Token.Named f _)) _) = (stringify' v) ++ "." ++ f

stringify' :: Ast.Var -> String	
stringify' (Ast.VarSimple v) = stringifyVarSimple v
stringify' (Ast.VarField v) = stringifyVarField v
stringify' _ = "MOISH"

stringify :: [ Ast.Var ] -> String
stringify = concatMap stringify' 

startloc :: Ast.Exp -> Maybe Location
startloc (Ast.ExpVar (Ast.ExpVarContent (Ast.VarSimple (Ast.VarSimpleContent (Token.VarName v))))) = Just (Token.location v)
startloc (Ast.ExpVar (Ast.ExpVarContent (Ast.VarField (Ast.VarFieldContent e _ _)))) = startloc e
startloc _ = Nothing

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

unquote :: String -> String
unquote s = let n = length s in take (n-2) (drop 1 s)

nameExp''' :: Token.Named -> Token.FieldName -> Token.NominalTy
nameExp''' v (Token.FieldName f) = let
    n = (Token.content v) ++ "." ++ (Token.content f)
    vloc = Token.location v
    floc = Token.location f
    l = vloc { Location.colEnd = Location.colEnd floc }
    in Token.NominalTy (Token.Named n l)

nameExp' :: Ast.Var -> Maybe Ast.Var
nameExp' (Ast.VarField v) = Just (Ast.VarField v)
nameExp' _ = Nothing

nameExp :: Ast.Exp -> Maybe Ast.Var
nameExp (Ast.ExpVar (Ast.ExpVarContent v)) = nameExp' v
nameExp _ = Nothing

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

extractPackageName :: String -> String
extractPackageName s = case ("github.com/" `isPrefixOf` s) of {
    True -> last ( splitOn "/" s );
    False -> s
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

vardecify' :: Token.Named -> Ast.Exp -> Ast.Stmt
vardecify' v exp = Ast.StmtAssign $ Ast.StmtAssignContent {
    Ast.stmtAssignLhs = Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName v,
    Ast.stmtAssignRhs = exp
}

vardecify :: [ Token.Named ] -> [ Ast.Exp ] -> [ Ast.Stmt ]
vardeicfy [] [] = []
vardecify _ [] = []
vardecify (name:names) (exp:exps) = (vardecify' name exp):(vardeicfy names exps)

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
parseProgram :: FilePath -> String -> Either String Ast.Root
parseProgram = runAlex' parse
}

