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
'++' { AlexTokenTag AlexRawToken_OP_PLUSPLUS _ }
'||' { AlexTokenTag AlexRawToken_OP_OR _ }
'|'  { AlexTokenTag AlexRawToken_OP_BITWISE_OR _ }
'>=' { AlexTokenTag AlexRawToken_OP_GEQ _ }
'<=' { AlexTokenTag AlexRawToken_OP_LEQ _ }
'==' { AlexTokenTag AlexRawToken_OP_EQEQ _ }
'>' { AlexTokenTag AlexRawToken_OP_GT _ }
'<-' { AlexTokenTag AlexRawToken_OP_LARROW _ }
'+=' { AlexTokenTag AlexRawToken_OP_PLUSEQ _ }

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
'Elt' { AlexTokenTag AlexRawToken_Elt _ }
'Len' { AlexTokenTag AlexRawToken_Len _ }
'*ast.ArrayType' { AlexTokenTag AlexRawToken_astArrayType _ }
'*ast.ParenExpr' { AlexTokenTag AlexRawToken_astParenExpr _ }
'*ast.InterfaceType' { AlexTokenTag AlexRawToken_astInterfaceType _ }
'Interface' { AlexTokenTag AlexRawToken_Interface _ }
'Methods' { AlexTokenTag AlexRawToken_Methods _ }
'For' { AlexTokenTag AlexRawToken_For _ }
'Post' { AlexTokenTag AlexRawToken_Post _ }
'*ast.ForStmt' { AlexTokenTag AlexRawToken_astForStmt _ }
'*ast.IncDecStmt' { AlexTokenTag AlexRawToken_astIncDecStmt _ }
'Label' { AlexTokenTag AlexRawToken_Label _ }
'continue' { AlexTokenTag AlexRawToken_continue _ }
'*ast.BranchStmt' { AlexTokenTag AlexRawToken_astBranchStmt _ }
'Select' { AlexTokenTag AlexRawToken_Select _ }
'*ast.SelectStmt' { AlexTokenTag AlexRawToken_astSelectStmt _ }
'Comm' { AlexTokenTag AlexRawToken_Comm _ }
'Case' { AlexTokenTag AlexRawToken_Case _ }
'*ast.CommClause' { AlexTokenTag AlexRawToken_astCommClause _ }
'break' { AlexTokenTag AlexRawToken_break _ }
'Go' { AlexTokenTag AlexRawToken_Go _ }
'Call' { AlexTokenTag AlexRawToken_Call _ }
'*ast.GoStmt' { AlexTokenTag AlexRawToken_astGoStmt _ }
'*ast.FuncLit' { AlexTokenTag AlexRawToken_astFuncLit _ }
'*ast.MapType' { AlexTokenTag AlexRawToken_astMapType _ }
'Map' { AlexTokenTag AlexRawToken_Map _ }
'*ast.RangeStmt' { AlexTokenTag AlexRawToken_astRangeStmt _ }
'Range' { AlexTokenTag AlexRawToken_Range _ }
'range' { AlexTokenTag AlexRawToken_range _ }
'Defer' { AlexTokenTag AlexRawToken_Defer _ }
'*ast.DeferStmt' { AlexTokenTag AlexRawToken_astDeferStmt _ }
'*ast.TypeSwitchStmt' { AlexTokenTag AlexRawToken_astTypeSwitchStmt _ }
'Switch' { AlexTokenTag AlexRawToken_Switch _ }
'*ast.CaseClause' { AlexTokenTag AlexRawToken_astCaseClause _ }
'*ast.SwitchStmt' { AlexTokenTag AlexRawToken_astSwitchStmt _ }
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
ordash(a): '-' { Nothing } | a { Just $1 }
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
exp_star { $1 } |
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

field:
field_1 { $1 } |
field_2 { $1 } |
field_3 { $1 }

numbered_field:
INT ':' field { $3 }

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

fields:
'*ast.FieldList'
'{'
    'Opening' ':' ordash(filename_location)
    'List' ':' fields_list
    'Closing' ':' ordash(filename_location)
'}'
{
    $8
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
        Ast.args = [],
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
        Ast.stmtClassDataMembers = Ast.DataMembers Data.Map.empty,
        Ast.stmtClassMethods = Ast.Methods Data.Map.empty
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
    'Params' ':' fields
    'Results' ':' ornull(fields)
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
    'Params' ':' fields
    'Results' ':' ornull(fields)
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
    'List' ':' block_stmts
    'Rbrace' ':' filename ':' location
'}'
{
    Ast.StmtBlock $ Ast.StmtBlockContent $10 $7
}

classes: fields { $1 }

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
nameExp' _ = Nothing

dummyLoc :: Location
dummyLoc = Location "T" 1 1 1 1

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
extractPackageName s = last ( splitOn "/" s )

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
