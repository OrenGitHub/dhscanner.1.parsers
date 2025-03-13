{
{-# OPTIONS -Werror=missing-fields #-}

module CsParser( parseProgram ) where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
import Ast
import CsLexer
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

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************

'kind' { AlexTokenTag AlexRawToken_KIND _ }
'null' { AlexTokenTag AlexRawToken_NULL _ }
'value' { AlexTokenTag AlexRawToken_VALUE _ }
'location' { AlexTokenTag AlexRawToken_LOCATION _ }
'children' { AlexTokenTag AlexRawToken_CHILDREN _ }
'CompilationUnit' { AlexTokenTag AlexRawToken_COMPILATION_UNIT _ }
'UsingDirective' { AlexTokenTag AlexRawToken_USING_DIRECTIVE _ }
'IdentifierName' { AlexTokenTag AlexRawToken_IDENTIFIER_NAME _ }
'QualifiedName' { AlexTokenTag AlexRawToken_QUALIFIED_NAME _ }
'NameEquals' { AlexTokenTag AlexRawToken_NAME_EQUALS _ }
'NamespaceDeclaration' { AlexTokenTag AlexRawToken_NAMESPACE_DECLARATION _ }
'ClassDeclaration' { AlexTokenTag AlexRawToken_CLASS_DECLARATION _ }
'SimpleBaseType' { AlexTokenTag AlexRawToken_SIMPLE_BASE_TYPE _ }
'BaseList' { AlexTokenTag AlexRawToken_BASE_LIST _ }
'ArgumentList' { AlexTokenTag AlexRawToken_ARGUMENT_LIST _ }
'ObjectCreationExpression' { AlexTokenTag AlexRawToken_OBJECT_CREATION_EXPRESSION _ }
'EqualsValueClause' { AlexTokenTag AlexRawToken_EQUALS_VALUE_CLAUSE _ }
'VariableDeclarator' { AlexTokenTag AlexRawToken_VARIABLE_DECLARATOR _ }
'TypeArgumentList' { AlexTokenTag AlexRawToken_TYPE_ARGUMENT_LIST _ }
'ObjectCreationExample' { AlexTokenTag AlexRawToken_OBJECT_CREATION_EXPRESSION _ }
'GenericName' { AlexTokenTag AlexRawToken_GENERIC_NAME _ }
'VariableDeclaration' { AlexTokenTag AlexRawToken_VARIABLE_DECLARATION _ }
'FieldDeclaration' { AlexTokenTag AlexRawToken_FIELD_DECLARATION _ }

-- ************
-- *          *
-- * location *
-- *          *
-- ************

'startLine' { AlexTokenTag AlexRawToken_START_LINE _ }
'startColumn' { AlexTokenTag AlexRawToken_START_COLUMN _ }
'endLine' { AlexTokenTag AlexRawToken_END_LINE _ }
'endColumn' { AlexTokenTag AlexRawToken_END_COLUMN _ }

-- ************
-- *          *
-- * integers *
-- *          *
-- ************

INT { AlexTokenTag (AlexRawToken_INT i) _ }
ID { AlexTokenTag (AlexRawToken_ID s) _ }

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
'{'
    'kind' ':' 'CompilationUnit' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' nonempty_listof(stmt) 
'}'
{
    Ast.Root
    {
        Ast.filename = "DDD",
        stmts = []
    }
}

identifier:
'{'
    'kind' ':' 'IdentifierName' ','
    'value' ':' ID ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    Token.Named
    {
        Token.content = tokIDValue $8,
        Token.location = $12
    }
}

var_simple:
identifier
{
    Ast.VarSimple $ Ast.VarSimpleContent
    {
        Ast.varName = Token.VarName $1
    }
}

var_field:
'{'
    'kind' ':' 'QualifiedName' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' var ',' identifier ']'
'}'
{
    Ast.VarField $ Ast.VarFieldContent
    {
        Ast.varFieldLhs = Ast.ExpVar $ Ast.ExpVarContent $17,
        Ast.varFieldName = Token.FieldName $19,
        Ast.varFieldLocation = $12
    }
}

var:
var_simple { $1 } |
var_field { $1 }

stmt_using_1:
'{'
    'kind' ':' 'UsingDirective' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' nonempty_listof(var)
'}'
{
    Ast.StmtImport $ Ast.StmtImportContent
    {
        Ast.stmtImportSource = stringify $16,
        Ast.stmtImportFromSource = Nothing,
        Ast.stmtImportAlias = Nothing,
        Ast.stmtImportLocation = $12
    }
}

name_equals:
'{'
    'kind' ':' 'NameEquals' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' identifier ']'
'}'
{
    $17
}

stmt_using_2:
'{'
    'kind' ':' 'UsingDirective' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' name_equals ',' var ']'
'}'
{
    Ast.StmtImport $ Ast.StmtImportContent
    {
        Ast.stmtImportSource = stringify [$19],
        Ast.stmtImportFromSource = Just (Token.content $17),
        Ast.stmtImportAlias = Nothing,
        Ast.stmtImportLocation = $12
    }
}

stmt_using:
stmt_using_1 { $1 } |
stmt_using_2 { $1 }

stmt_var:
var
{
    Ast.StmtAssign $ Ast.StmtAssignContent
    {
        Ast.stmtAssignLhs = Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named "theNamespace" (Ast.locationVar $1),
        Ast.stmtAssignRhs = Ast.ExpVar (Ast.ExpVarContent $1)
    }
}

stmt_class:
'{'
    'kind' ':' 'ClassDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(stmt)
'}'
{
    Ast.StmtClass $ Ast.StmtClassContent
    {
        Ast.stmtClassName = Token.ClassName (Token.Named "MyAwesomeClass" $12),
        Ast.stmtClassSupers = [],
        Ast.stmtClassDataMembers = Ast.DataMembers Data.Map.empty,
        Ast.stmtClassMethods = Ast.Methods Data.Map.empty
    }
}

simple_base_type:
'{'
    'kind' ':' 'SimpleBaseType' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' identifier ']'
'}'
{
    $17
}

stmt_super:
'{'
    'kind' ':' 'BaseList' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' simple_base_type ']'
'}'
{
    Ast.StmtAssign $ Ast.StmtAssignContent
    {
        Ast.stmtAssignLhs = Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named "thisClassExtends" $12,
        Ast.stmtAssignRhs = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $17
    }
}

type_argument_list:
'{'
    'kind' ':' 'TypeArgumentList' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' identifier ']'
'}'
{
    $17
}

generic_name:
'{'
    'kind' ':' 'GenericName' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' type_argument_list ']'
'}'
{
    $17
}

nominal_type:
identifier { $1 } |
generic_name { $1 }

args:
'{'
    'kind' ':' 'ArgumentList' ','
    'value' ':' '(' ')' ','
    'location' ':' location ','
    'children' ':' '[' ']'
'}'
{
    []
}

exp_new:
'{'
    'kind' ':' 'ObjectCreationExpression' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominal_type ',' args ']'
'}'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $17,
        Ast.args = [],
        Ast.expCallLocation = $12
    }
}

exp:
exp_new { $1 }

equals_value_clause:
'{'
    'kind' ':' 'EqualsValueClause' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' exp ']'
'}'
{
    $17
}

vardec:
'{'
    'kind' ':' 'VariableDeclarator' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' equals_value_clause ']'
'}'
{
    $17
}

stmt_vardec:
'{'
    'kind' ':' 'VariableDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' nominal_type ',' vardec ']'
'}'
{
    Ast.StmtVardec $ Ast.StmtVardecContent
    {
        Ast.stmtVardecName = Token.VarName (Token.Named "unknown" $12),
        Ast.stmtVardecNominalType = Token.NominalTy (Token.Named "TTT" $12),
        Ast.stmtVardecInitValue = Nothing,
        Ast.stmtVardecLocation = $12 
    }
}

stmt_data_member:
'{'
    'kind' ':' 'FieldDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' '[' stmt_vardec ']'
'}'
{
    $17
}

stmt:
stmt_var { $1 } |
stmt_super { $1 } |
stmt_class { $1 } |
stmt_using { $1 } |
stmt_vardec { $1 } |
stmt_namespace { $1 } |
stmt_data_member { $1 }

stmt_namespace:
'{'
    'kind' ':' 'NamespaceDeclaration' ','
    'value' ':' 'null' ','
    'location' ':' location ','
    'children' ':' possibly_empty_listof(stmt)
'}'
{
    Ast.StmtBlock $ Ast.StmtBlockContent
    {
        Ast.stmtBlockContent = $16,
        Ast.stmtBlockLocation = $12
    }
}

location:
'{'
    'startLine' ':' INT ','
    'startColumn' ':' INT ','
    'endLine' ':' INT ','
    'endColumn' ':' INT
'}'
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

