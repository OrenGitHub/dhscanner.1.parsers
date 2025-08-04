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
import Data.List ( map, isPrefixOf, isInfixOf, stripPrefix )
import Data.List.Split ( splitOn )
import Data.Map ( toList, fromList, empty, Map, filter )

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
%lexer { lexwrap } { AlexTokenTag TokenEOF _ _ }

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

'(' { AlexTokenTag AlexRawToken_LPAREN _ _ }
')' { AlexTokenTag AlexRawToken_RPAREN _ _ }
'[' { AlexTokenTag AlexRawToken_LBRACK _ _ }
']' { AlexTokenTag AlexRawToken_RBRACK _ _ }
'{' { AlexTokenTag AlexRawToken_LBRACE _ _ }
'}' { AlexTokenTag AlexRawToken_RBRACE _ _ }

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************

':' { AlexTokenTag AlexRawToken_COLON _ _ }
',' { AlexTokenTag AlexRawToken_COMMA _ _ }
'.' { AlexTokenTag AlexRawToken_DOT   _ _ }
'=' { AlexTokenTag AlexRawToken_EQ    _ _ }

-- *************
-- *           *
-- * operators *
-- *           *
-- *************

'-'  { AlexTokenTag AlexRawToken_MINUS _ _ }
'*'  { AlexTokenTag AlexRawToken_TIMES _ _ }
'+'  { AlexTokenTag AlexRawToken_OP_PLUS _ _ }
'@'  { AlexTokenTag AlexRawToken_AT _ _ }
'&'  { AlexTokenTag AlexRawToken_ampersand _ _ }
':=' { AlexTokenTag AlexRawToken_OP_ASSIGN _ _ }
'!=' { AlexTokenTag AlexRawToken_OP_NEQ _ _ }
'!'  { AlexTokenTag AlexRawToken_OP_BANG _ _ }
'&&' { AlexTokenTag AlexRawToken_OP_AND _ _ }
'++' { AlexTokenTag AlexRawToken_OP_PLUSPLUS _ _ }
'||' { AlexTokenTag AlexRawToken_OP_OR _ _ }
'|'  { AlexTokenTag AlexRawToken_OP_BITWISE_OR _ _ }
'>=' { AlexTokenTag AlexRawToken_OP_GEQ _ _ }
'<=' { AlexTokenTag AlexRawToken_OP_LEQ _ _ }
'==' { AlexTokenTag AlexRawToken_OP_EQEQ _ _ }
'>' { AlexTokenTag AlexRawToken_OP_GT _ _ }
'<-' { AlexTokenTag AlexRawToken_OP_LARROW _ _ }
'+=' { AlexTokenTag AlexRawToken_OP_PLUSEQ _ _ }

-- ************
-- *          *
-- * keywords *
-- *          *
-- ************

'*ast.File' { AlexTokenTag AlexRawToken_astFile _ _ }
'Doc' { AlexTokenTag AlexRawToken_Doc _ _ }
'nil' { AlexTokenTag AlexRawToken_nil _ _ }
'Package' { AlexTokenTag AlexRawToken_Package _ _ }
'Name' { AlexTokenTag AlexRawToken_Name _ _ }
'*ast.Ident' { AlexTokenTag AlexRawToken_astIdent _ _ }
'NamePos' { AlexTokenTag AlexRawToken_NamePos _ _ }
'Obj' { AlexTokenTag AlexRawToken_Obj _ _ }
'Decls' { AlexTokenTag AlexRawToken_Decls _ _ }
'ast.Decl' { AlexTokenTag AlexRawToken_astDecl _ _ }
'*ast.GenDecl' { AlexTokenTag AlexRawToken_astGenDecl _ _ }
'TokPos' { AlexTokenTag AlexRawToken_TokPos _ _ }
'Tok' { AlexTokenTag AlexRawToken_Tok _ _ }
'Lparen' { AlexTokenTag AlexRawToken_Lparen _ _ }
'Specs' { AlexTokenTag AlexRawToken_Specs _ _ }
'ast.Spec' { AlexTokenTag AlexRawToken_astSpec _ _ }
'len' { AlexTokenTag AlexRawToken_len _ _ }
'import' { AlexTokenTag AlexRawToken_import _ _ }
'*ast.ImportSpec' { AlexTokenTag AlexRawToken_astImportSpec _ _ }
'*ast.BasicLit' { AlexTokenTag AlexRawToken_astBasicLit _ _ }
'ValuePos' { AlexTokenTag AlexRawToken_ValuePos _ _ }
'Kind' { AlexTokenTag AlexRawToken_Kind _ _ }
'Value' { AlexTokenTag AlexRawToken_Value _ _ }
'Path' { AlexTokenTag AlexRawToken_Path _ _ }
'Comment' { AlexTokenTag AlexRawToken_Comment _ _ }
'EndPos' { AlexTokenTag AlexRawToken_EndPos _ _ }
'STRING' { AlexTokenTag AlexRawToken_STRING _ _ }
'Rparen' { AlexTokenTag AlexRawToken_Rparen _ _ }
'var' { AlexTokenTag AlexRawToken_var _ _ }
'Names' { AlexTokenTag AlexRawToken_Names _ _ }
'Values' { AlexTokenTag AlexRawToken_Values _ _ }
'ast.Expr' { AlexTokenTag AlexRawToken_astExpr _ _ }
'*ast.ValueSpec' { AlexTokenTag AlexRawToken_astValueSpec _ _ }
'Types' { AlexTokenTag AlexRawToken_Types _ _ }
'Type' { AlexTokenTag AlexRawToken_Type _ _ }
'Decl' { AlexTokenTag AlexRawToken_Decl _ _ }
'Data' { AlexTokenTag AlexRawToken_Data _ _ }
'obj' { AlexTokenTag AlexRawToken_obj _ _ }
'*ast.Object' { AlexTokenTag AlexRawToken_astObject _ _ }
'*ast.CallExpr' { AlexTokenTag AlexRawToken_astCallExpr _ _ }
'Fun' { AlexTokenTag AlexRawToken_Fun _ _ }
'Args' { AlexTokenTag AlexRawToken_Args _ _ }
'Ellipsis' { AlexTokenTag AlexRawToken_Ellipsis _ _ }
'*ast.IndexExpr' { AlexTokenTag AlexRawToken_astIndexExpr _ _ }
'X' { AlexTokenTag AlexRawToken_X _ _ }
'Lbrack' { AlexTokenTag AlexRawToken_Lbrack _ _ }
'Rbrack' { AlexTokenTag AlexRawToken_Rbrack _ _ }
'Index' { AlexTokenTag AlexRawToken_Index _ _ }
'*ast.SelectorExpr' { AlexTokenTag AlexRawToken_astSelectorExpr _ _ }
'Sel' { AlexTokenTag AlexRawToken_Sel _ _ }
'Y' { AlexTokenTag AlexRawToken_Y _ _ }
'Op' { AlexTokenTag AlexRawToken_Op _ _ }
'OpPos' { AlexTokenTag AlexRawToken_OpPos _ _ }
'*ast.BinaryExpr' { AlexTokenTag AlexRawToken_astBinaryExpr _ _ }
'INT' { AlexTokenTag AlexRawToken_KW_INT _ _ }
'type' { AlexTokenTag AlexRawToken_type _ _ }
'*ast.Field' { AlexTokenTag AlexRawToken_astField _ _ }
'Tag' { AlexTokenTag AlexRawToken_Tag _ _ }
'*ast.FieldList' { AlexTokenTag AlexRawToken_astFieldList _ _ }
'Opening' { AlexTokenTag AlexRawToken_Opening _ _ }
'List' { AlexTokenTag AlexRawToken_List _ _ }
'Closing' { AlexTokenTag AlexRawToken_Closing _ _ }
'*ast.StructType' { AlexTokenTag AlexRawToken_astStructType _ _ }
'Struct' { AlexTokenTag AlexRawToken_Struct _ _ }
'Fields' { AlexTokenTag AlexRawToken_Fields _ _ }
'Incomplete' { AlexTokenTag AlexRawToken_Incomplete _ _ }
'false' { AlexTokenTag AlexRawToken_false _ _ }
'*ast.TypeSpec' { AlexTokenTag AlexRawToken_astTypeSpec _ _ }
'TypeParams' { AlexTokenTag AlexRawToken_TypeParams _ _ }
'Assign' { AlexTokenTag AlexRawToken_Assign _ _ }
'*ast.FuncType' { AlexTokenTag AlexRawToken_astFuncType _ _ }
'Func' { AlexTokenTag AlexRawToken_Func _ _ }
'Params' { AlexTokenTag AlexRawToken_Params _ _ }
'Results' { AlexTokenTag AlexRawToken_Results _ _ }
'*ast.BlockStmt' { AlexTokenTag AlexRawToken_astBlockStmt _ _ }
'Lbrace' { AlexTokenTag AlexRawToken_Lbrace _ _ }
'Rbrace' { AlexTokenTag AlexRawToken_Rbrace _ _ }
'*ast.FuncDecl' { AlexTokenTag AlexRawToken_astFuncDecl _ _ }
'Recv' { AlexTokenTag AlexRawToken_Recv _ _ }
'Body' { AlexTokenTag AlexRawToken_Body _ _ }
'func' { AlexTokenTag AlexRawToken_func _ _ }
'Star' { AlexTokenTag AlexRawToken_Star _ _ }
'*ast.StarExpr' { AlexTokenTag AlexRawToken_astStarExpr _ _ }
'ast.Stmt' { AlexTokenTag AlexRawToken_astStmt _ _ }
'*ast.DeclStmt' { AlexTokenTag AlexRawToken_astDeclStmt _ _ }
'If' { AlexTokenTag AlexRawToken_If _ _ }
'Init' { AlexTokenTag AlexRawToken_Init _ _ }
'Cond' { AlexTokenTag AlexRawToken_Cond _ _ }
'Else' { AlexTokenTag AlexRawToken_Else _ _ }
'*ast.IfStmt' { AlexTokenTag AlexRawToken_astIfStmt _ _ }
'Lhs' { AlexTokenTag AlexRawToken_Lhs _ _ }
'Rhs' { AlexTokenTag AlexRawToken_Rhs _ _ }
'*ast.AssignStmt' { AlexTokenTag AlexRawToken_astAssignStmt _ _ }
'*ast.UnaryExpr' { AlexTokenTag AlexRawToken_astUnaryExpr _ _ }
'*ast.ExprStmt' { AlexTokenTag AlexRawToken_astExprStmt _ _ }
'*ast.ReturnStmt' { AlexTokenTag AlexRawToken_astReturnStmt _ _ }
'Return' { AlexTokenTag AlexRawToken_Return _ _ }
'Key' { AlexTokenTag AlexRawToken_Key _ _ }
'Colon' { AlexTokenTag AlexRawToken_Colon _ _ }
'Elts' { AlexTokenTag AlexRawToken_Elts _ _ }
'*ast.KeyValueExpr' { AlexTokenTag AlexRawToken_astKeyValueExpr _ _ }
'*ast.CompositeLit' { AlexTokenTag AlexRawToken_astCompositeLit _ _ }
'*ast.TypeAssertExpr' { AlexTokenTag AlexRawToken_astTypeAssertExpr _ _ }
'FileStart' { AlexTokenTag AlexRawToken_FileStart _ _ }
'FileEnd' { AlexTokenTag AlexRawToken_FileEnd _ _ }
'Scope' { AlexTokenTag AlexRawToken_Scope _ _ }
'Imports' { AlexTokenTag AlexRawToken_Imports _ _ }
'Unresolved' { AlexTokenTag AlexRawToken_Unresolved _ _ }
'Comments' { AlexTokenTag AlexRawToken_Comments _ _ }
'Outer' { AlexTokenTag AlexRawToken_Outer _ _ }
'Objects' { AlexTokenTag AlexRawToken_Objects _ _ }
'map' { AlexTokenTag AlexRawToken_map _ _ }
'string' { AlexTokenTag AlexRawToken_string _ _ }
'*ast.Scope' { AlexTokenTag AlexRawToken_astScope _ _ }
'Low' { AlexTokenTag AlexRawToken_Low _ _ }
'High' { AlexTokenTag AlexRawToken_High _ _ }
'*ast.SliceExpr' { AlexTokenTag AlexRawToken_astSliceExpr _ _ }
'Max' { AlexTokenTag AlexRawToken_Max _ _ }
'Slice3' { AlexTokenTag AlexRawToken_Slice3 _ _ }
'Elt' { AlexTokenTag AlexRawToken_Elt _ _ }
'Len' { AlexTokenTag AlexRawToken_Len _ _ }
'*ast.ArrayType' { AlexTokenTag AlexRawToken_astArrayType _ _ }
'*ast.ParenExpr' { AlexTokenTag AlexRawToken_astParenExpr _ _ }
'*ast.InterfaceType' { AlexTokenTag AlexRawToken_astInterfaceType _ _ }
'Interface' { AlexTokenTag AlexRawToken_Interface _ _ }
'Methods' { AlexTokenTag AlexRawToken_Methods _ _ }
'For' { AlexTokenTag AlexRawToken_For _ _ }
'Post' { AlexTokenTag AlexRawToken_Post _ _ }
'*ast.ForStmt' { AlexTokenTag AlexRawToken_astForStmt _ _ }
'*ast.IncDecStmt' { AlexTokenTag AlexRawToken_astIncDecStmt _ _ }
'Label' { AlexTokenTag AlexRawToken_Label _ _ }
'continue' { AlexTokenTag AlexRawToken_continue _ _ }
'*ast.BranchStmt' { AlexTokenTag AlexRawToken_astBranchStmt _ _ }
'Select' { AlexTokenTag AlexRawToken_Select _ _ }
'*ast.SelectStmt' { AlexTokenTag AlexRawToken_astSelectStmt _ _ }
'Comm' { AlexTokenTag AlexRawToken_Comm _ _ }
'Case' { AlexTokenTag AlexRawToken_Case _ _ }
'*ast.CommClause' { AlexTokenTag AlexRawToken_astCommClause _ _ }
'break' { AlexTokenTag AlexRawToken_break _ _ }
'Go' { AlexTokenTag AlexRawToken_Go _ _ }
'Call' { AlexTokenTag AlexRawToken_Call _ _ }
'*ast.GoStmt' { AlexTokenTag AlexRawToken_astGoStmt _ _ }
'*ast.FuncLit' { AlexTokenTag AlexRawToken_astFuncLit _ _ }
'*ast.MapType' { AlexTokenTag AlexRawToken_astMapType _ _ }
'Map' { AlexTokenTag AlexRawToken_Map _ _ }
'*ast.RangeStmt' { AlexTokenTag AlexRawToken_astRangeStmt _ _ }
'Range' { AlexTokenTag AlexRawToken_Range _ _ }
'range' { AlexTokenTag AlexRawToken_range _ _ }
'Defer' { AlexTokenTag AlexRawToken_Defer _ _ }
'*ast.DeferStmt' { AlexTokenTag AlexRawToken_astDeferStmt _ _ }
'*ast.TypeSwitchStmt' { AlexTokenTag AlexRawToken_astTypeSwitchStmt _ _ }
'Switch' { AlexTokenTag AlexRawToken_Switch _ _ }
'*ast.CaseClause' { AlexTokenTag AlexRawToken_astCaseClause _ _ }
'*ast.SwitchStmt' { AlexTokenTag AlexRawToken_astSwitchStmt _ _ }
'const' { AlexTokenTag AlexRawToken_const _ _ }
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

INT { AlexTokenTag (AlexRawToken_INT i) _ _ }
ID { AlexTokenTag (AlexRawToken_ID s) _ _ }
QUOTED_ID { AlexTokenTag (AlexRawToken_QUOTED_ID s) _ _ }

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
ordash(a): '-' { Nothing } | a { Just $1 }
orempty(a): 'nil' { [] } | a { $1 }

-- ************
-- *          *
-- * numbered *
-- *          *
-- ************
numbered(a): INT ':' a { $3 }

-- **********************
-- *                    *
-- * parametrized lists *
-- *                    *
-- **********************
listof(a): a { [$1] } | a listof(a) { $1:$2 }

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

scope_objects: { Nothing } | scope_object scope_objects { Nothing }

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
'const'  { "const"  } |
'func'   { "func"   }

stmts:
'[' ']' 'ast.Decl' '(' 'len' '=' INT ')'
'{'
    numbered_stmts
'}'
{
    $10
} |
'[' ']' 'ast.Stmt' '(' 'len' '=' INT ')'
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
            Location.colEnd = 2 + (Location.colEnd $7) + (fromIntegral (length (tokIDValue $13)))
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
            Location.colEnd = Location.colEnd $21 + 1
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
    let loc = case (startloc $5) of { Nothing -> Token.location $8; Just l -> l }  in Ast.VarField $ Ast.VarFieldContent
    {
        Ast.varFieldLhs = $5,
        Ast.varFieldName = Token.FieldName $8,
        Ast.varFieldLocation = loc {
            Location.lineEnd = Location.lineEnd ( Token.location $8 ),
            Location.colEnd = Location.colEnd ( Token.location $8 )
        }
    }
}

var_simple: identifier { Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $1 }

var_object:
'*' '(' 'obj' '@' INT ')'
{
    Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named "obj" dummyLoc
}

var:
var_object    { $1 } |
var_simple    { $1 } |
var_field     { $1 } |
var_star      { $1 } |
var_subscript { $1 }

exp_var:
var { Ast.ExpVar $ Ast.ExpVarContent $1 }

operator:
'*'  { Nothing } |
'+'  { Nothing } |
'-'  { Nothing } |
'!'  { Nothing } |
'&'  { Just "ampersand" } |
'++' { Nothing } |
'&&' { Nothing } |
'|'  { Nothing } |
'||' { Nothing } |
':=' { Nothing } |
'='  { Nothing } |
'>=' { Nothing } |
'+=' { Nothing } |
'>'  { Nothing } |
'<-' { Nothing } |
'<=' { Nothing } |
'==' { Nothing } |
'!=' { Nothing } |
'range' { Nothing }

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

var_star:
'*ast.StarExpr'
'{'
    'Star' ':' filename ':' location
    'X' ':' exp
'}'
{
    Ast.VarSubscript $ Ast.VarSubscriptContent {
        Ast.varSubscriptLhs = $10,
        Ast.varSubscriptIdx = Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt 0 $7,
        Ast.varSubscriptLocation = $7
    }
}

exp_unop:
'*ast.UnaryExpr'
'{'
    'OpPos' ':' ordash(filename_location)
    'Op' ':' operator
    'X' ':' exp
'}'
{
    case $8 of {
        Nothing -> $11;
        Just s -> case s of {
            "ampersand" -> Ast.ExpCall $ Ast.ExpCallContent {
                Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named "ampersand" (case $5 of { Just l -> l; _ -> dummyLoc }),
                Ast.args = [$11],
                Ast.expCallLocation = case $5 of { Just l -> l; _ -> dummyLoc }
            };
            _ -> $11
        }
    }
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

element:
INT ':' kv { $3 } |
INT ':' exp { $3 }

elements:
element { [$1] } |
element elements { $1:$2 }

elts:
'[' ']' 'ast.Expr' '(' 'len' '=' INT ')'
'{'
    elements
'}'
{
    $10
}

exp_dict:
'*ast.CompositeLit'
'{'
    'Type' ':' ornull(exp)
    'Lbrace' ':' filename ':' location
    'Elts' ':' orempty(elts)
    'Rbrace' ':' filename ':' location
    'Incomplete' ':' 'false'
'}'
{
    let loc = case $5 of {
        Nothing -> $18;
        Just e -> case (startloc e) of {
            Nothing -> $18;
            Just l -> l
        }
    } in Ast.ExpCall $ Ast.ExpCallContent {
        Ast.callee = case $5 of {
            Nothing -> Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named "composite" $18;
            Just e -> e
        }, 
        Ast.args = $13,
        Ast.expCallLocation = loc {
            Location.lineEnd = Location.lineEnd $18,
            Location.colEnd = Location.colEnd $18
        }
    }
}

exp_ty_assert:
'*ast.TypeAssertExpr'
'{'
    'X' ':' exp
    'Lparen' ':' filename ':' location
    'Type' ':' ornull(exp)
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

exp_array:
'*ast.ArrayType'
'{'
    'Lbrack' ':' filename ':' location
    'Len' ':' 'nil'
    'Elt' ':' exp
'}'
{
    $13
}

exp_paren:
'*ast.ParenExpr'
'{'
    'Lparen' ':' filename ':' location
    'X' ':' exp
    'Rparen' ':' filename ':' location
'}'
{
    $10
}

exp_interface:
'*ast.InterfaceType'
'{'
    'Interface' ':' filename ':' location
    'Methods' ':' fields
    'Incomplete' ':' 'false'
'}'
{
    Ast.ExpNull $ Ast.ExpNullContent $ Token.ConstNull $7
}

exps:
values
{
    case $1 of {
        (v:_) -> v;
        [] -> Ast.ExpNull $ Ast.ExpNullContent $ Token.ConstNull (Location "" 1 1 1 1)
    }
}

exp_lambda:
'*ast.FuncLit'
'{'
    'Type' ':' type_func
    'Body' ':' stmt
'}'
{
    Ast.ExpLambda $ Ast.ExpLambdaContent
    {
        Ast.expLambdaParams = snd (fst $5),
        Ast.expLambdaBody = [$8],
        Ast.expLambdaLocation = (fst (fst $5)) {
            Location.colEnd = Location.colStart (fst (fst $5)) + (fromIntegral (length "func"))
        }
    }
}

exp_maptype:
'*ast.MapType'
'{'
    'Map' ':' filename ':' location
    'Key' ':' exp
    'Value' ':' exp
'}'
{
    let v = Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named "mapify" $7
    in Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent v,
        Ast.args = [$10,$13],
        Ast.expCallLocation = $7
    }
}

exp:
exps { $1 } |
exp_str { Ast.ExpStr $ Ast.ExpStrContent $1 } |
exp_int { Ast.ExpInt $ Ast.ExpIntContent $1 } |
exp_var { $1 } |
exp_dict { $1 } |
exp_call { $1 } |
exp_unop { $1 } |
exp_maptype { $1 } |
exp_slice { $1 } |
exp_binop { $1 } |
exp_paren { $1 } |
exp_lambda { $1 } |
exp_array { $1 } |
exp_struct { $1 } |
exp_interface { $1 } |
exp_ty_assert { $1 }

stmt_import:
'*ast.ImportSpec'
'{'
    'Doc' ':' 'nil'
    'Name' ':' ornull(identifier)
    'Path' ':' exp_str
    'Comment' ':' 'nil'
    'EndPos' ':' '-'
'}'
{
    import_normalizer (getModule $1) $8 $11
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
        Ast.stmtBlockContent = vardecify $8 $14 $11,
        Ast.stmtBlockLocation = dummyLoc
    }
}

field_1:
'*ast.Field'
'{'
    'Doc' ':' 'nil'
    'Names' ':' names
    'Type' ':' exp
    'Tag' ':' ornull(exp_str)
    'Comment' ':' 'nil'
'}'
{
    paramify (nameExp $11) $8
}

field_2:
'*ast.Field'
'{'
    'Doc' ':' 'nil'
    'Names' ':' 'nil'
    'Type' ':' exp
    'Tag' ':' ornull(exp_str)
    'Comment' ':' 'nil'
'}'
{
    let nominalType = case (nameExp $11) of {
        (Just (Ast.VarSimple (Ast.VarSimpleContent (Token.VarName v)))) -> Token.NominalTy v;
        _ -> Token.NominalTy (Token.Named "any" (Location "" 1 1 1 1))
    } in [Ast.Param {
        Ast.paramName = Token.ParamName (Token.Named "_" (Location "" 1 1 1 1)),
        Ast.paramNominalType = nominalType,
        Ast.paramNominalTypeV2 = nameExp $11,
        Ast.paramSerialIdx = 0
    }]
}

field_3:
'*ast.Field'
'{'
    'Doc' ':' 'nil'
    'Names' ':' names
    'Type' ':' type_func
    'Tag' ':' ornull(exp_str)
    'Comment' ':' 'nil'
'}'
{
    []
}

old_field:
field_1 { $1 } |
field_2 { $1 } |
field_3 { $1 }

numbered_field:
INT ':' old_field { $3 }

numbered_fields:
numbered_field { $1 } |
numbered_field numbered_fields { $1 ++ $2 }

fields_list: '[' ']' '*ast.Field' '(' 'len' '=' INT ')'
'{'
    numbered_fields
'}'
{
    $10
} | 'nil' { [] }

filename_location: filename ':' location { $3 }

old_fields:
'*ast.FieldList'
'{'
    'Opening' ':' ordash(filename_location)
    'List' ':' fields_list
    'Closing' ':' ordash(filename_location)
'}'
{
    $8
}

type_array:
'*ast.ArrayType'
'{'
    'Lbrack' ':' filename_location
    'Len' ':' 'nil'
    'Elt' ':' var
'}'
{
    $11
}

func_type:
'*ast.FuncType'
'{'
    'Func' ':' '-'
    'TypeParams' ':' 'nil'
    'Params' ':' fields
    'Results' ':' 'nil'
'}'
{
    Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named "LOLO" dummyLoc
}

map_type:
'*ast.MapType'
'{'
    'Map' ':' filename_location
    'Key' ':' var
    'Value' ':' var
'}'
{
    $8
}

type:
var { $1 } |
type_array { $1 } |
func_type { $1 } |
map_type { $1 }

field:
'*ast.Field'
'{'
    'Doc' ':' 'nil'
    'Names' ':' ornull(names)
    'Type' ':' type
    'Tag' ':' ornull(exp_str)
    'Comment' ':' 'nil'
'}'
{
    let names = case $8 of { Just n' -> n'; _ -> [] } in [
        (name, $11, jsonme $14) | name <- names
    ]
}

field_list_new:
'[' ']' '*ast.Field' '(' 'len' '=' INT ')'
'{'
    listof(numbered(field))
'}'
{
    concat $10
}

fields:
'*ast.FieldList'
'{'
    'Opening' ':' ordash(filename_location)
    'List' ':' ornull(field_list_new)
    'Closing' ':' ordash(filename_location)
'}'
{
    case $8 of { Nothing -> []; Just f -> f }
}

exp_struct:
'*ast.StructType'
'{'
    'Struct' ':' filename ':' location
    'Fields' ':' fields
    'Incomplete' ':' 'false'
'}'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "structify",
            Token.location = $7
        },
        Ast.args = [ fieldme x y z | (x,y,z) <- $10 ],
        Ast.expCallLocation = $7
    }
}

stmt_class:
'*ast.TypeSpec'
'{'
    'Doc' ':' 'nil'
    'Name' ':' identifier
    'TypeParams' ':' 'nil'
    'Assign' ':' '-'
    'Type' ':' exp
    'Comment' ':' 'nil'
'}'
{
    Ast.StmtClass $ Ast.StmtClassContent
    {
        Ast.stmtClassName = Token.ClassName $8,
        Ast.stmtClassSupers = [],
        Ast.stmtClassDataMembers = dataMembersFrom $17,
        Ast.stmtClassMethods = methodsFrom (Token.ClassName $8) (dataMembersFrom $17)
    }
}

stmt_functype:
'*ast.TypeSpec'
'{'
    'Doc' ':' 'nil'
    'Name' ':' identifier
    'TypeParams' ':' 'nil'
    'Assign' ':' '-'
    'Type' ':' type_func
    'Comment' ':' 'nil'
'}'
{
    Ast.StmtVardec $ Ast.StmtVardecContent {
        Ast.stmtVardecName = Token.VarName $8,
        Ast.stmtVardecNominalType = Token.NominalTy (Token.Named "any" (Token.location $8)),
        Ast.stmtVardecInitValue = Nothing,
        Ast.stmtVardecLocation = Token.location $8
    }
}

stmt_type:
stmt_class { $1 } |
stmt_functype { $1 }

type_func_1:
'*ast.FuncType'
'{'
    'Func' ':' filename ':' location
    'TypeParams' ':' 'nil'
    'Params' ':' old_fields
    'Results' ':' ornull(old_fields)
'}'
{
    let returnType = case $16 of {
        Just (p:_) -> Ast.paramNominalType p;
        _ -> Token.NominalTy (Token.Named "any" $7)
    } in (($7,$13),returnType)
}

type_func_2:
'*ast.FuncType'
'{'
    'Func' ':' '-'
    'TypeParams' ':' 'nil'
    'Params' ':' old_fields
    'Results' ':' ornull(old_fields)
'}'
{
    ((dummyLoc,$11), Token.NominalTy (Token.Named "any" dummyLoc))
}

type_func:
type_func_1 { $1 } |
type_func_2 { $1 }

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
    'List' ':' ornull(block_stmts)
    'Rbrace' ':' filename ':' location
'}'
{
    Ast.StmtBlock $ Ast.StmtBlockContent (case $10 of { Just l -> l; _ -> [] }) $7
}

classes: old_fields { $1 }

stmt_func:
'*ast.FuncDecl'
'{'
    'Doc' ':' 'nil'
    'Recv' ':' orempty(classes)
    'Name' ':' identifier
    'Type' ':' type_func
    'Body' ':' stmt
'}'
{
    case $8 of
        [] -> Ast.StmtFunc $ Ast.StmtFuncContent
              {
                  Ast.stmtFuncReturnType = snd $14,
                  Ast.stmtFuncName = Token.FuncName $11,
                  Ast.stmtFuncParams = snd (fst $14),
                  Ast.stmtFuncBody = [$17],
                  Ast.stmtFuncAnnotations = [],
                  Ast.stmtFuncLocation = Token.location $11 
              }
        (p:_) -> Ast.StmtMethod $ Ast.StmtMethodContent
                 {
                     Ast.stmtMethodReturnType = snd $14,
                     Ast.stmtMethodName = Token.MethdName $11,
                     Ast.stmtMethodParams = [p] ++ (snd (fst $14)),
                     Ast.stmtMethodBody = [$17],
                     Ast.stmtMethodLocation = Token.location $11,
                     Ast.hostingClassName = Token.ClassName (Token.getNominalTyToken (Ast.paramNominalType p)),
                     Ast.hostingClassSupers = []
                 }
}

stmt_decl:
'*ast.DeclStmt'
'{'
    'Decl' ':' stmt
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
    'Body' ':' stmt
    'Else' ':' ornull(stmt)
'}'
{
    let stmt = Ast.StmtIf $ Ast.StmtIfContent {
        Ast.stmtIfCond = $13,
        Ast.stmtIfBody = [$16],
        Ast.stmtElseBody = case $19 of { Nothing -> []; Just s -> [s] },
        Ast.stmtIfLocation = $7
    } in case $10 of {
        Nothing -> stmt;
        Just init_stmt -> Ast.StmtBlock (Ast.StmtBlockContent [ init_stmt, stmt ] $7 )
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
        Ast.stmtAssignRhs = case $35 of { [] -> Ast.ExpNull (Ast.ExpNullContent (Token.ConstNull $20)); (e:_) -> e }
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

stmt_for:
'*ast.ForStmt'
'{'
    'For' ':' filename ':' location
    'Init' ':' ornull(stmt)
    'Cond' ':' ornull(exp)
    'Post' ':' ornull(stmt)
    'Body' ':' stmt
'}'
{
    let true = Ast.ExpBool (Ast.ExpBoolContent (Token.ConstBool True $7)) in Ast.StmtWhile $ Ast.StmtWhileContent
    {
        Ast.stmtWhileCond = case $13 of { Nothing -> true; Just e -> e },
        Ast.stmtWhileBody = [$19],
        Ast.stmtWhileLocation = $7
    }
}

stmt_incdec:
'*ast.IncDecStmt'
'{'
    'X' ':' var
    'TokPos' ':' filename ':' location
    'Tok' ':' operator
'}'
{
    Ast.StmtAssign $ Ast.StmtAssignContent
    {
        Ast.stmtAssignLhs = $5,
        Ast.stmtAssignRhs = Ast.ExpVar $ Ast.ExpVarContent $5
    }
}

stmt_continue:
'*ast.BranchStmt'
'{'
    'TokPos' ':' filename ':' location
    'Tok' ':' 'continue'
    'Label' ':' 'nil'
'}'
{
    Ast.StmtContinue $ Ast.StmtContinueContent $7
}

stmt_break:
'*ast.BranchStmt'
'{'
    'TokPos' ':' filename ':' location
    'Tok' ':' 'break'
    'Label' ':' 'nil'
'}'
{
    Ast.StmtContinue $ Ast.StmtContinueContent $7
}


stmt_switch:
'*ast.SelectStmt'
'{'
    'Select' ':' filename ':' location
    'Body' ':' stmt
'}'
{
    $10
}

stmt_case:
'*ast.CommClause'
'{'
    'Case' ':' filename ':' location
    'Comm' ':' stmt
    'Colon' ':' filename ':' location
    'Body' ':' stmts
'}'
{
    Ast.StmtBlock $ Ast.StmtBlockContent $18 $7
}

stmt_go:
'*ast.GoStmt'
'{'
    'Go' ':' filename ':' location
    'Call' ':' exp
    
'}'
{
    Ast.StmtExp $10
}

range_stmt_token:
':=' { Nothing } |
'='  { Nothing }

stmt_range:
'*ast.RangeStmt'
'{'
    'For' ':' filename ':' location
    'Key' ':' identifier
    'Value' ':' ornull(exp)
    'TokPos' ':' filename ':' location
    'Tok' ':' range_stmt_token
    'Range' ':' filename ':' location
    'X' ':' exp
    'Body' ':' stmt
'}'
{
    $32
}

stmt_defer:
'*ast.DeferStmt'
'{'
    'Defer' ':' filename ':' location
    'Call' ':' exp
'}'
{
    Ast.StmtExp $10
}

stmt_switch2:
'*ast.TypeSwitchStmt'
'{'
    'Switch' ':' filename ':' location
    'Init' ':' 'nil'
    'Assign' ':' stmt
    'Body' ':' stmt
'}'
{
    $13
}

exprs:
'[' ']' 'ast.Expr' '(' 'len' '=' INT ')'
'{'
    listof(numbered_exp)
'}'
{
    $10
}

stmt_caseclause:
'*ast.CaseClause'
'{'
    'Case' ':' filename ':' location
    'List' ':' ornull(exprs)
    'Colon' ':' filename ':' location
    'Body' ':' stmts
'}'
{
    Ast.StmtBlock $ Ast.StmtBlockContent {
        Ast.stmtBlockContent = $18,
        Ast.stmtBlockLocation = $7
    }
}

stmt_switch3:
'*ast.SwitchStmt'
'{'
    'Switch' ':' filename ':' location
    'Init' ':' 'nil'
    'Tag' ':' identifier
    'Body' ':' stmt
'}'
{
    $16
}

stmt:
stmt_assign  { $1 } |
stmt_gendecl { $1 } |
stmt_type    { $1 } |
stmt_switch  { $1 } |
stmt_switch2 { $1 } |
stmt_switch3 { $1 } |
stmt_defer   { $1 } |
stmt_case    { $1 } |
stmt_caseclause { $1 } |
stmt_go      { $1 } |
stmt_if      { $1 } |
stmt_for     { $1 } |
stmt_obj     { $1 } |
stmt_exp     { $1 } |
stmt_func    { $1 } |
stmt_incdec  { $1 } |
stmt_range   { $1 } |
stmt_import  { $1 } |
stmt_block   { $1 } |
stmt_return  { $1 } |
stmt_decl    { $1 } |
stmt_continue { $1 } |
stmt_break { $1 } |
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
startloc (Ast.ExpCall (Ast.ExpCallContent callee _ _)) = startloc callee
startloc _ = Nothing

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
nameExp' (Ast.VarSimple v) = Just (Ast.VarSimple v)
nameExp' (Ast.VarSubscript v) = Just (Ast.VarSubscript v)

dummyLoc :: Location
dummyLoc = Location "T" 1 1 1 1

jsonme :: Maybe Token.ConstStr -> Token.Named
jsonme Nothing = Token.Named "string" dummyLoc
jsonme (Just s) = case "json:" `isInfixOf` (Token.constStrValue s) of {
    True -> Token.Named "json" (Token.constStrLocation s);
    False -> Token.Named "string" (Token.constStrLocation s)
}

fieldme :: Token.Named -> Ast.Var -> Token.Named -> Ast.Exp
fieldme f v info = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarField $ Ast.VarFieldContent {
    Ast.varFieldLhs = Ast.ExpVar (Ast.ExpVarContent (Ast.VarSimple (Ast.VarSimpleContent (Token.VarName info)))),
    Ast.varFieldName = Token.FieldName f,
    Ast.varFieldLocation = Token.location f   
}

paramify :: Maybe Ast.Var -> [ Token.Named ] -> [ Ast.Param ]
paramify Nothing names = Data.List.map paramifySingleNoType names
paramify (Just v) names = Data.List.map (paramifySingleWithType v) names

paramifySingleNoType :: Token.Named -> Ast.Param
paramifySingleNoType name = Ast.Param {
    Ast.paramName = Token.ParamName name,
    Ast.paramNominalType = Token.NominalTy (Token.Named "any" (Token.location name)),
    Ast.paramNominalTypeV2 = Nothing,
    Ast.paramSerialIdx = 0
}

paramifySingleWithType :: Ast.Var -> Token.Named -> Ast.Param
paramifySingleWithType t@(Ast.VarSimple (Ast.VarSimpleContent (Token.VarName v))) name = Ast.Param {
    Ast.paramName = Token.ParamName name,
    Ast.paramNominalType = Token.NominalTy v,
    Ast.paramNominalTypeV2 = Just t,
    Ast.paramSerialIdx = 0
}
paramifySingleWithType nominalType name = Ast.Param {
    Ast.paramName = Token.ParamName name,
    Ast.paramNominalType = Token.NominalTy (Token.Named "any" (Token.location name)),
    Ast.paramNominalTypeV2 = Just nominalType,
    Ast.paramSerialIdx = 0
}

nameExp :: Ast.Exp -> Maybe Ast.Var
nameExp (Ast.ExpVar (Ast.ExpVarContent v)) = nameExp' v
nameExp _ = Nothing

extractPackageName :: String -> String
extractPackageName s = last ( splitOn "/" s )

extractParamSingleName' :: [ Token.ParamName ] -> Maybe Token.ParamName
extractParamSingleName' ps = case ps of { [p] -> Just p; _ -> Nothing }
 
extractParamSingleName :: [ Either Token.ParamName Token.NominalTy ] -> Maybe Token.ParamName
extractParamSingleName = extractParamSingleName' . lefts  

extractParamNominalType' :: [ Token.NominalTy ] -> Maybe Token.NominalTy
extractParamNominalType' ts = case ts of { [t] -> Just t; _ -> Nothing }
 
extractParamNominalType :: [ Either Token.ParamName Token.NominalTy ] -> Maybe Token.NominalTy
extractParamNominalType = extractParamNominalType' . rights 

chooseBetween :: Token.Named -> Ast.Var -> Token.Named
chooseBetween _ (Ast.VarSimple (Ast.VarSimpleContent (Token.VarName v))) = v
chooseBetween f _ = f

dataFrom :: Ast.Var -> Token.Named -> Ast.DataMember
dataFrom v f = Ast.DataMember {
    Ast.dataMemberName = Token.MembrName f,
    Ast.dataMemberNominalType = Token.NominalTy (chooseBetween f v),
    Ast.dataMemberInitValue = Nothing
}

dataMemberFrom'' :: Token.FieldName -> Ast.Var -> Maybe (Token.MembrName, Ast.DataMember)
dataMemberFrom'' (Token.FieldName f) v = Just ((Token.MembrName f), dataFrom v f)

dataMemberFrom' :: Ast.VarFieldContent -> Maybe (Token.MembrName, Ast.DataMember)
dataMemberFrom' (Ast.VarFieldContent (Ast.ExpVar (Ast.ExpVarContent v)) field _) = dataMemberFrom'' field v
dataMemberFrom' _ = Nothing

dataMemberFrom :: Ast.Exp -> Maybe (Token.MembrName, Ast.DataMember)
dataMemberFrom (Ast.ExpVar (Ast.ExpVarContent (Ast.VarField v))) = dataMemberFrom' v
dataMemberFrom _ = Nothing

dataMembersFrom4 :: [ Ast.Exp ] -> Map Token.MembrName Ast.DataMember
dataMembersFrom4 = Data.Map.fromList . mapMaybe dataMemberFrom

dataMembersFrom''' :: [ Ast.Exp ] -> Ast.DataMembers
dataMembersFrom''' args = Ast.DataMembers (dataMembersFrom4 args)

dataMembersFrom'' :: Token.Named -> [ Ast.Exp ] -> Ast.DataMembers
dataMembersFrom'' v args = case ((Token.content v) == "structify") of {
    True -> dataMembersFrom''' args;
    False -> Ast.DataMembers Data.Map.empty
}

dataMembersFrom' :: Ast.Exp -> [ Ast.Exp ] -> Ast.DataMembers
dataMembersFrom' (Ast.ExpVar (Ast.ExpVarContent (Ast.VarSimple (Ast.VarSimpleContent (Token.VarName v))))) args = dataMembersFrom'' v args
dataMembersFrom' _ _ = Ast.DataMembers Data.Map.empty

dataMembersFrom :: Ast.Exp -> Ast.DataMembers
dataMembersFrom (Ast.ExpCall call) = dataMembersFrom' (Ast.callee call) (Ast.args call)
dataMembersFrom _ = Ast.DataMembers Data.Map.empty

keepJsonDataMember :: Ast.DataMember -> Bool
keepJsonDataMember d = (Token.content (Token.getNominalTyToken (Ast.dataMemberNominalType d))) == "json"

keepJsonDataMembers :: Map Token.MembrName Ast.DataMember -> Map Token.MembrName Ast.DataMember
keepJsonDataMembers = Data.Map.filter keepJsonDataMember

paramFrom :: Token.Named -> Ast.Param
paramFrom name = Ast.Param {
    Ast.paramName = Token.ParamName (Token.Named "json" (Token.location name)),
    Ast.paramNominalType = Token.NominalTy (Token.Named "json" (Token.location name)),
    Ast.paramNominalTypeV2 = Just (Ast.VarSimple (Ast.VarSimpleContent (Token.VarName (Token.Named "json" (Token.location name))))),
    Ast.paramSerialIdx = 0
}

methodFrom :: Token.ClassName -> Ast.DataMember -> Ast.StmtMethodContent
methodFrom c (Ast.DataMember (Token.MembrName name) (Token.NominalTy t) _) = Ast.StmtMethodContent {
    Ast.stmtMethodReturnType = Token.NominalTy (Token.Named "any" (Token.location t)),
    Ast.stmtMethodName = Token.MethdName name,
    Ast.stmtMethodParams = [paramFrom name],
    Ast.stmtMethodBody = [],
    Ast.stmtMethodLocation = (Token.location t),
    Ast.hostingClassName = c,
    Ast.hostingClassSupers = []
}

toMethodPair :: Token.ClassName -> (Token.MembrName, Ast.DataMember) -> (Token.MethdName, Ast.StmtMethodContent)
toMethodPair c ((Token.MembrName name), d) = ((Token.MethdName name), methodFrom c d)

toMethodPairs :: Token.ClassName -> [(Token.MembrName, Ast.DataMember)] -> [(Token.MethdName, Ast.StmtMethodContent)]
toMethodPairs c = Data.List.map (toMethodPair c)

methodsFrom'' :: Token.ClassName -> Map Token.MembrName Ast.DataMember -> Map Token.MethdName Ast.StmtMethodContent
methodsFrom'' c = Data.Map.fromList . (toMethodPairs c) . Data.Map.toList

methodsFrom' :: Token.ClassName -> Map Token.MembrName Ast.DataMember -> Ast.Methods
methodsFrom' c = Ast.Methods . (methodsFrom'' c)

methodsFrom :: Token.ClassName -> Ast.DataMembers -> Ast.Methods
methodsFrom c (Ast.DataMembers dataMembers) = methodsFrom' c (keepJsonDataMembers dataMembers)

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

vardecify'' :: Token.Named -> [ Token.Named ] -> Token.Named -> [ Ast.Stmt ]
vardecify'' name names t = [ Ast.StmtVardec $ Ast.StmtVardecContent {
    Ast.stmtVardecName = Token.VarName name,
    Ast.stmtVardecNominalType = Token.NominalTy t,
    Ast.stmtVardecInitValue = Nothing,
    Ast.stmtVardecLocation = Token.location name
}]

vardecify :: [ Token.Named ] -> [ Ast.Exp ] -> Maybe Exp -> [ Ast.Stmt ]
vardeicfy [] [] _ = []
vardecify _ [] Nothing = []
vardecify (name:names) [] (Just (Ast.ExpVar (Ast.ExpVarContent (Ast.VarSimple (Ast.VarSimpleContent (Token.VarName t)))))) = vardecify'' name names t
vardecify (name:names) [] _ = []
vardecify (name:names) (exp:exps) e = (vardecify' name exp):(vardeicfy names exps e)

getLastSegment :: String -> String
getLastSegment = last . splitOn "/"

import_normalizer' :: Token.ConstStr -> Ast.Stmt
import_normalizer' s = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = Token.constStrValue s,
    Ast.stmtImportFromSource = Nothing,
    Ast.stmtImportAlias = Just (getLastSegment (Token.constStrValue s)),
    Ast.stmtImportLocation = Token.constStrLocation s
}

import_normalizer'' :: Token.Named -> Token.ConstStr -> Ast.Stmt
import_normalizer'' alias imported = Ast.StmtImport $ Ast.StmtImportContent {
    Ast.stmtImportSource = Token.constStrValue imported,
    Ast.stmtImportFromSource = Nothing,
    Ast.stmtImportAlias = Just (Token.content alias),
    Ast.stmtImportLocation = Token.constStrLocation imported
}

import_normalizer''' :: String -> Token.ConstStr -> Ast.Stmt
import_normalizer''' m imported = case (m `isPrefixOf` (Token.constStrValue imported)) of {
    False -> import_normalizer' imported;
    True -> Ast.StmtExp $ Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt {
        Token.constIntValue = 0,
        Token.constIntLocation = Token.constStrLocation imported
    }
}

import_normalizer4 :: String -> Token.Named -> Token.ConstStr -> Ast.Stmt
import_normalizer4 m alias imported = case stripPrefix m (Token.constStrValue imported) of {
    Nothing -> import_normalizer'' alias imported;
    Just suffix -> Ast.StmtImport $ Ast.StmtImportContent {
        Ast.stmtImportSource = suffix,
        Ast.stmtImportFromSource = Nothing,
        Ast.stmtImportAlias = Just (Token.content alias),
        Ast.stmtImportLocation = Token.constStrLocation imported
    }
}

import_normalizer :: Maybe String -> Maybe Token.Named -> Token.ConstStr -> Ast.Stmt
import_normalizer Nothing Nothing imported = import_normalizer' imported
import_normalizer Nothing (Just alias) imported = import_normalizer'' alias imported
import_normalizer (Just m) Nothing imported = import_normalizer''' m imported
import_normalizer (Just m) (Just alias) imported = import_normalizer4 m alias imported

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
