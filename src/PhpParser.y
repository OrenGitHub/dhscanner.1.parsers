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
'expr'                  { AlexTokenTag AlexRawToken_EXPR            _ }
'Name'                  { AlexTokenTag AlexRawToken_MAME            _ }
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
'Stmt_If'               { AlexTokenTag AlexRawToken_STMT_IF         _ }
'Stmt_For'              { AlexTokenTag AlexRawToken_STMT_FOR        _ }
'Stmt_Echo'             { AlexTokenTag AlexRawToken_STMT_ECHO       _ }
'Expr_Variable'         { AlexTokenTag AlexRawToken_EXPR_VAR        _ }
'Expr_FuncCall'         { AlexTokenTag AlexRawToken_EXPR_CALL       _ }
'Stmt_Expr'             { AlexTokenTag AlexRawToken_STMT_EXPR       _ }
'Scalar_Int'            { AlexTokenTag AlexRawToken_SCALAR_INT      _ }
'Identifier'            { AlexTokenTag AlexRawToken_IDENTIFIER      _ }
'Stmt_Return'           { AlexTokenTag AlexRawToken_STMT_RETURN     _ }
'returnType'            { AlexTokenTag AlexRawToken_RETURN_TYPE     _ }
'Stmt_Function'         { AlexTokenTag AlexRawToken_STMT_FUNCTION   _ }
'Expr_ConstFetch'       { AlexTokenTag AlexRawToken_EXPR_CONST_GET  _ }
'Expr_BinaryOp_Plus'    { AlexTokenTag AlexRawToken_EXPR_BINOP_PLUS _ }
'Expr_BinaryOp_Smaller' { AlexTokenTag AlexRawToken_EXPR_BINOP_LT   _ }

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

-- *********************
-- *                   *
-- * Ast root: program *
-- *                   *
-- *********************
program: 'array' '(' decs ')'
{
    Ast.Root
    {
        Ast.filename = "DDD",
        Ast.decs = [],
        Ast.stmts = []
    }
}

-- **********************
-- *                    *
-- * parametrized lists *
-- *                    *
-- **********************
listof(a): a { [$1] } | a listof(a) { $1:$2 }

-- ********
-- *      *
-- * decs *
-- *      *
-- ********
decs: listof(numbered_dec) { $1 }

-- ****************
-- *              *
-- * numbered_dec *
-- *              *
-- ****************
numbered_dec: INT ':' dec { $3 }

-- *******
-- *     *
-- * dec *
-- *     *
-- *******
dec: dec_function { $1 }

-- ****************
-- *              *
-- * dec_function *
-- *              *
-- ****************
dec_function: 'Stmt_Function' '(' dec_function_attrs ')'
{
    let

        name = getFuncNameAttr $3
        returnType = getFuncReturnType $3
        params = getFuncParams $3
        body = getFuncBody $3

    in

        case name of
            Nothing -> Left "Function missing name"
            Just name' -> case params of
                Nothing -> Left "Function missing params"
                Just params' -> case body of
                    Nothing -> Left "Function missing body"
                    Just body' -> case returnType of
                        Nothing -> Left "MMM"
                        Just returnType' -> Right $ Ast.DecFunc $ Ast.DecFuncContent {
                            Ast.decFuncReturnType = returnType',
                            Ast.decFuncName = name',
                            Ast.decFuncParams = params',
                            Ast.decFuncBody = body'
                        }
}

-- **********************
-- *                    *
-- * dec_function_attrs *
-- *                    *
-- **********************
dec_function_attrs: listof(dec_function_attr) { $1 }

-- *********************
-- *                   *
-- * dec_function_attr *
-- *                   *
-- *********************
dec_function_attr:
dec_func_attr_name       { Left  (Left  $1) } |
dec_func_attr_params     { Left  (Right $1) } |
dec_func_attr_returnType { Right (Left  $1) } |
dec_func_attr_body       { Right (Right $1) }

-- **********************
-- *                    *
-- * dec_func_attr_name *
-- *                    *
-- **********************
dec_func_attr_name: 'name' ':' 'Identifier' loc '(' 'name' ':' ID ')'
{
    Token.FuncName $ Token.Named
    {
        Token.content = tokIDValue $8,
        Token.location = $4
    }
}

-- *******************
-- *                 *
-- * dec_func_params *
-- *                 *
-- *******************
dec_func_attr_params: 'array' '(' numbered_params ')' { catMaybes $3 }

-- *******************
-- *                 *
-- * numbered_params *
-- *                 *
-- *******************
numbered_params: listof(numbered_param) { $1 }

-- ******************
-- *                *
-- * numbered_param *
-- *                *
-- ******************
numbered_param: INT ':' param { $3 }

-- *********
-- *       *
-- * param *
-- *       *
-- *********
param: 'Param' loc '(' listof(param_attr) ')' { paramify $4 $2 }

-- **************
-- *            *
-- * param_attr *
-- *            *
-- **************
param_attr:
param_attr_name { Left  $1 } |
param_attr_type { Right $1 }

-- *******************
-- *                 *
-- * param_attr_name *
-- *                 *
-- *******************
param_attr_name: 'var' ':' 'Expr_Variable' loc '(' 'name' ':' ID ')'
{
    Token.ParamName $ Token.Named
    {
        Token.content = tokIDValue $8,
        Token.location = $4
    }    
}

-- *******************
-- *                 *
-- * param_attr_type *
-- *                 *
-- *******************
param_attr_type: 'type' ':' 'Identifier' loc '(' 'name' ':' ID ')'
{
    Token.NominalTy $ Token.Named
    {
        Token.content = tokIDValue $8,
        Token.location = $4
    }    
}

-- ****************************
-- *                          *
-- * dec_func_attr_returnType *
-- *                          *
-- ****************************
dec_func_attr_returnType: 'returnType' ':' 'Identifier' loc '(' 'name' ':' ID ')'
{
    Token.NominalTy $ Token.Named
    {
        Token.content = tokIDValue $8,
        Token.location = $4
    }
}

-- **********************
-- *                    *
-- * dec_func_attr_body *
-- *                    *
-- **********************
dec_func_attr_body: 'stmts' ':' stmts { $3 }

-- *********
-- *       *
-- * stmts *
-- *       *
-- *********
stmts: 'array' '(' numbered_stmts ')' { $3 }

-- ******************
-- *                *
-- * numbered_stmts *
-- *                *
-- ******************
numbered_stmts: numbered_stmt numbered_stmts { $1:$2 } | numbered_stmt { [$1] }

-- *****************
-- *               *
-- * numbered_stmt *
-- *               *
-- *****************
numbered_stmt: INT ':' stmt { $3 }

-- ********
-- *      *
-- * stmt *
-- *      *
-- ********
stmt:
stmt_if     { $1 } | 
stmt_for    { $1 } |
stmt_return { $1 }



-- ***********
-- *         *
-- * stmt_if *
-- *         *
-- ***********
stmt_if: 'Stmt_If' loc '(' 'cond' ':' exp 'stmts' ':' stmts ')'
{
    Ast.StmtIf $ Ast.StmtIfContent
    {
        Ast.stmtIfCond = $6,
        Ast.stmtIfBody = $9,
        Ast.stmtIfLocation = $2
    }
}

-- ***********
-- *         *
-- * stmt_if *
-- *         *
-- ***********
stmt_echo: 'Stmt_Echo' loc '(' 'exprs' ':' exps ')' { Nothing }

-- ***************
-- *             *
-- * stmt_return *
-- *             *
-- ***************
stmt_return: 'Stmt_Return' loc '(' 'expr' ':' exp ')'
{
    Ast.StmtReturn $ Ast.StmtReturnContent
    {
        Ast.stmtReturnValue = Just $6,
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

-- *******
-- *     *
-- * exp *
-- *     *
-- *******
exp:
exp_int   { $1 } |
exp_bool  { $1 } |
exp_binop { $1 } |
exp_var   { $1 }

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
exp_var: 'Expr_Variable' loc '(' 'name' ':' ID ')'
{
    Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
    {
        Token.content = tokIDValue $6,
        Token.location = $2
    }
}

-- *************
-- *           *
-- * exp_binop *
-- *           *
-- *************
exp_binop: 'Expr_BinaryOp_Smaller' loc '(' 'left' ':' exp 'right' ':' exp ')' { $6 } 

-- ************
-- *          *
-- * exp_call *
-- *          *
-- ************
exp_call: 
'Expr_FuncCall' loc '(' 'name' ':' 'Name' loc '(' 'name' ':' ID ')' 'args' ':' args ')'
{
    Nothing
}

-- ********
-- *      *
-- * args *
-- *      *
-- ********
args: 'array' '(' numbered_args ')' { $3 }

-- *****************
-- *               *
-- * numbered_args *
-- *               *
-- *****************
numbered_args: numbered_arg numbered_args { $1:$2 } | numbered_arg { [$1] }

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
arg: 'Arg' loc '(' arg_attrs ')'
{
    catMaybes $4
}

-- *************
-- *           *
-- * arg_attrs *
-- *           *
-- *************
arg_attrs: arg_attr arg_attrs { $1:$2 } | arg_attr { [$1] }

-- ************
-- *          *
-- * arg_attr *
-- *          *
-- ************
arg_attr:
arg_attr_value  { Just $1 } |
arg_attr_ignore { Nothing }

-- ******************
-- *                *
-- * arg_attr_value *
-- *                *
-- ******************
arg_attr_value: 'value' ':' exp { $3 }

-- *******************
-- *                 *
-- * arg_attr_ignore *
-- *                 *
-- *******************
arg_attr_ignore: ID ':' ID { Nothing }

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

-- *******
-- *     *
-- * loc *
-- *     *
-- *******
loc: '[' INT ':' INT '-' INT ':' INT ']'
{
    Location
    {
        Location.filename = "",
        lineStart = tokIntValue $2,
        colStart = tokIntValue $4,
        lineEnd = tokIntValue $6,
        colEnd = tokIntValue $8
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
