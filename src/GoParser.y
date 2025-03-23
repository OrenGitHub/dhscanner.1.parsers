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

'-' { AlexTokenTag AlexRawToken_MINUS _ }
'*' { AlexTokenTag AlexRawToken_TIMES _ }
'@' { AlexTokenTag AlexRawToken_AT _    }

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
'}'
{
    Ast.Root
    {
        Ast.filename = "DDD",
        stmts = []
    }
}

tokenID:
'import' { "import" } |
'var'    { "var"    }

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
        Token.constStrValue = "POPO",
        Token.constStrLocation = $7
    }
}

exp_call:
'*ast.CallExpr'
'{'
    'Fun' ':' exp
    'Lparen' ':' filename ':' location
    'Args' ':' 'nil'
    'Ellipsis' ':' '-'
    'Rparen' ':' filename ':' location
'}'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = $5,
        Ast.args = [],
        Ast.expCallLocation = $10
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
        Ast.varFieldLocation = Location "" 1 1 1 1
    }
}

var_simple: identifier { Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $1 }

var:
var_simple    { $1 } |
var_field     { $1 } |
var_subscript { $1 }

exp_var:
var { Ast.ExpVar $ Ast.ExpVarContent $1 }

exp:
exp_str { Ast.ExpStr $ Ast.ExpStrContent $1 } |
exp_var { $1 } |
exp_call { $1 }

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
    Ast.StmtImport $ Ast.StmtImportContent
    {
        Ast.stmtImportSource = "momo",
        Ast.stmtImportFromSource = Nothing,
        Ast.stmtImportAlias = Nothing,
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
    'Type' ':' 'nil'
    'Values' ':' values
    'Comment' ':' 'nil'
'}'
{
    Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = vardecify $8 $14,
        Ast.stmtBlockLocation = Location "" 1 1 1 1
    }
}

stmt:
stmt_gendecl { $1 } |
stmt_import  { $1 } |
stmt_decvar  { $1 }

numbered_stmt:
INT ':' stmt
{
    $3
}

object:
'*ast.Object'
'{'
    'Kind' ':' 'var'
    'Name' ':' QUOTED_ID
    'Decl' ':' '*' '(' 'obj' '@' INT ')'
    'Data' ':' INT
    'Type' ':' 'nil'
'}'
{
    Nothing
}

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
        Token.content = "popo",
        Token.location = $7
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
        colStart = 1 + (fromIntegral (tokIntValue $3)),
        lineEnd = fromIntegral (tokIntValue $1),
        colEnd = 1 + (fromIntegral (tokIntValue $3))
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

