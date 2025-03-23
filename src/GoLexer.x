{

{-# OPTIONS -w  #-}

module GoLexer

where

import Prelude hiding (lex)
import Control.Monad ( liftM )
import Location
}

-- ***********
-- *         *
-- * wrapper *
-- *         *
-- ***********
%wrapper "monadUserState"

-- ***************
-- *             *
-- * parentheses *
-- *             *
-- ***************

@LPAREN = \(
@RPAREN = \)
@LBRACK = \[
@RBRACK = \]
@LBRACE = \{
@RBRACE = \}

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************

@COLON  = ":"
@COMMA  = ","
@DOT    = "."
@EQ     = "="

-- *************
-- *           *
-- * operators *
-- *           *
-- *************

@MINUS = \-
@TIMES = \*

-- ************
-- *          *
-- * keywords *
-- *          *
-- ************

@astFile = "*ast.File"
@Doc = Doc
@nil = nil
@Package = Package
@Name = Name
@astIdent = "*ast.Ident"
@NamePos = NamePos
@Obj = Obj
@Decls = Decls
@astDecl = "ast.Decl"
@astGenDecl = "*ast.GenDecl"
@TokPos = TokPos
@Tok = Tok
@Lparen = Lparen
@Specs = Specs
@astSpec = "ast.Spec"
@len = len
@import = import
@astImportSpec = "*ast.ImportSpec"
@astBasicLit = "*ast.BasicLit"
@ValuePos = ValuePos
@Kind = Kind
@Value = Value
@Path = Path
@Comment = Comment
@EndPos = EndPos
@STRING = STRING
@Rparen = Rparen
@var = var
@Names = Names
@Values = Values
@astExpr = "ast.Expr"
@astValueSpec = "*ast.ValueSpec"
@Types = Types
@Type = Type
@Decl = Decl
@Data = Data
@AT = "@"
@obj = obj
@astObject = "*ast.Object"
@astCallExpr = "*ast.CallExpr"
@Fun = Fun
@Args = Args
@Ellipsis = Ellipsis
@astIndexExpr = "*ast.IndexExpr"
@X = X
@Lbrack = Lbrack
@Rbrack = Rbrack
@Index = Index
@astSelectorExpr = "*ast.SelectorExpr"
@Sel = Sel
-- last keywords first part

-- ************
-- *          *
-- * integers *
-- *          *
-- ************
@DIGIT = 0-9
@LETTER = [a-zA-Z_]
@LETTER_OR_DIGIT = @LETTER | @DIGIT
@INT = @DIGIT+ 
@DQUOTE = \"
@ID = (@LETTER)(@LETTER_OR_DIGIT+)
@NQUOTE = [^\"]
@QUOTED_ID = (@DQUOTE)(@NQUOTE*)(@DQUOTE)

-- ***************
-- *             *
-- * white space *
-- *             *
-- ***************
@WHITE_SPACE = $white+

-- **********
-- *        *
-- * tokens *
-- *        *
-- **********
tokens :-

-- ***************
-- *             *
-- * parentheses *
-- *             *
-- ***************

@LPAREN { lex' AlexRawToken_LPAREN }
@RPAREN { lex' AlexRawToken_RPAREN }
@LBRACK { lex' AlexRawToken_LBRACK }
@RBRACK { lex' AlexRawToken_RBRACK }
@LBRACE { lex' AlexRawToken_LBRACE }
@RBRACE { lex' AlexRawToken_RBRACE }

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************

@COLON { lex' AlexRawToken_COLON }
@COMMA { lex' AlexRawToken_COMMA }
@DOT   { lex' AlexRawToken_DOT   }
@EQ    { lex' AlexRawToken_EQ    }

-- *************
-- *           *
-- * operators *
-- *           *
-- *************

@MINUS { lex' AlexRawToken_MINUS }
@TIMES { lex' AlexRawToken_TIMES }

-- ************
-- *          *
-- * keywords *
-- *          *
-- ************

@astFile { lex' AlexRawToken_astFile }
@Doc {lex' AlexRawToken_Doc}
@nil {lex' AlexRawToken_nil}
@Package {lex' AlexRawToken_Package}
@Name {lex' AlexRawToken_Name}
@astIdent {lex' AlexRawToken_astIdent}
@NamePos {lex' AlexRawToken_NamePos}
@Obj {lex' AlexRawToken_Obj}
@Decls {lex' AlexRawToken_Decls}
@astDecl {lex' AlexRawToken_astDecl}
@astGenDecl {lex' AlexRawToken_astGenDecl}
@TokPos {lex' AlexRawToken_TokPos}
@Tok {lex' AlexRawToken_Tok}
@Lparen {lex' AlexRawToken_Lparen}
@Specs {lex' AlexRawToken_Specs}
@astSpec {lex' AlexRawToken_astSpec}
@len {lex' AlexRawToken_len}
@import {lex' AlexRawToken_import}
@astImportSpec {lex' AlexRawToken_astImportSpec}
@astBasicLit {lex' AlexRawToken_astBasicLit}
@ValuePos {lex' AlexRawToken_ValuePos}
@Kind {lex' AlexRawToken_Kind}
@Value {lex' AlexRawToken_Value}
@Path {lex' AlexRawToken_Path}
@Comment {lex' AlexRawToken_Comment}
@EndPos {lex' AlexRawToken_EndPos}
@STRING {lex' AlexRawToken_STRING}
@Rparen {lex' AlexRawToken_Rparen}
@var {lex' AlexRawToken_var}
@Names {lex' AlexRawToken_Names}
@Values {lex' AlexRawToken_Values}
@astExpr {lex' AlexRawToken_astExpr}
@astValueSpec {lex' AlexRawToken_astValueSpec}
@Types {lex' AlexRawToken_Types}
@Type {lex' AlexRawToken_Type}
@Decl {lex' AlexRawToken_Decl}
@Data {lex' AlexRawToken_Data}
@AT {lex' AlexRawToken_AT}
@obj {lex' AlexRawToken_obj}
@astObject {lex' AlexRawToken_astObject}
@astCallExpr {lex' AlexRawToken_astCallExpr}
@Fun {lex' AlexRawToken_Fun}
@Args {lex' AlexRawToken_Args}
@Ellipsis {lex' AlexRawToken_Ellipsis}
@astIndexExpr {lex' AlexRawToken_astIndexExpr}
@X {lex' AlexRawToken_X}
@Lbrack {lex' AlexRawToken_Lbrack}
@Rbrack {lex' AlexRawToken_Rbrack}
@Index {lex' AlexRawToken_Index}
@astSelectorExpr {lex' AlexRawToken_astSelectorExpr}
@Sel {lex' AlexRawToken_Sel}
-- last keywords second part

-- ***************************
-- *                         *
-- * whitespace ? do nothing *
-- *                         *
-- ***************************

@WHITE_SPACE ;

-- ****************************
-- *                          *
-- * integers and identifiers *
-- *                          *
-- ****************************
@INT { lex (AlexRawToken_INT . round . read) }
@ID { lex (AlexRawToken_ID . read) }
@QUOTED_ID { lex (AlexRawToken_QUOTED_ID . read) }
. { lexicalError }

{

-- | According to [the docs][1] (emphasis mine):
--
-- > `AlexUserState` /must/ be defined in the user's program
--
-- [1]: https://haskell-alex.readthedocs.io/en/latest/api.html#the-monaduserstate-wrapper
data AlexUserState = AlexUserState { filepath :: FilePath } deriving ( Show )

-- | According to [the docs][1] (emphasis mine):
--
-- > a call to an initialization function (`alexInitUserState`) ...
-- > must also be defined in the user’s program
--
-- [1]: https://haskell-alex.readthedocs.io/en/latest/api.html#the-monaduserstate-wrapper
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

-- | getter of the AlexUserState
-- this is w.r.t to alexGetUserState :: Alex AlexUserState
-- according to [the docs][1]
--
-- [1]: https://haskell-alex.readthedocs.io/en/latest/api.html#the-monaduserstate-wrapper
getFilePath :: Alex FilePath
getFilePath = filepath <$> alexGetUserState

-- | setter of the AlexUserState
-- this is w.r.t to alexSetUserState :: AlexUserState -> Alex ()
-- according to [the docs][1]
--
-- [1]: https://haskell-alex.readthedocs.io/en/latest/api.html#the-monaduserstate-wrapper
setFilePath :: FilePath -> Alex ()
setFilePath fp = do
  alexSetUserState (AlexUserState { filepath = fp })

-- *********
-- *       *
-- * Token *
-- *       *
-- *********
data AlexTokenTag
   = AlexTokenTag
     {
         tokenRaw :: AlexRawToken,
         tokenLoc :: Location
     }
     deriving ( Show )

-- *************
-- *           *
-- * Raw Token *
-- *           *
-- *************
data AlexRawToken

     -- ********************
     -- *                  *
     -- * integers and ids *
     -- *                  *
     -- ********************

     = AlexRawToken_INT Int
     | AlexRawToken_ID String
     | AlexRawToken_QUOTED_ID String

     -- ************
     -- *          *
     -- * keywords *
     -- *          *
     -- ************
     | AlexRawToken_astFile
     | AlexRawToken_Doc
     | AlexRawToken_nil
     | AlexRawToken_Package
     | AlexRawToken_Name
     | AlexRawToken_astIdent
     | AlexRawToken_NamePos
     | AlexRawToken_Obj
     | AlexRawToken_Decls
     | AlexRawToken_astDecl
     | AlexRawToken_astGenDecl
     | AlexRawToken_TokPos
     | AlexRawToken_Tok
     | AlexRawToken_Lparen
     | AlexRawToken_Specs
     | AlexRawToken_astSpec
     | AlexRawToken_len
     | AlexRawToken_import
     | AlexRawToken_astImportSpec
     | AlexRawToken_astBasicLit
     | AlexRawToken_ValuePos
     | AlexRawToken_Kind
     | AlexRawToken_Value
     | AlexRawToken_Path
     | AlexRawToken_Comment
     | AlexRawToken_EndPos
     | AlexRawToken_STRING
     | AlexRawToken_Rparen
     | AlexRawToken_var
     | AlexRawToken_Names
     | AlexRawToken_Values
     | AlexRawToken_astExpr
     | AlexRawToken_astValueSpec
     | AlexRawToken_Types
     | AlexRawToken_Type
     | AlexRawToken_Decl
     | AlexRawToken_Data
     | AlexRawToken_AT
     | AlexRawToken_obj
     | AlexRawToken_astObject
     | AlexRawToken_astCallExpr
     | AlexRawToken_Fun
     | AlexRawToken_Args
     | AlexRawToken_Ellipsis
     | AlexRawToken_astIndexExpr
     | AlexRawToken_X
     | AlexRawToken_Lbrack
     | AlexRawToken_Rbrack
     | AlexRawToken_Index
     | AlexRawToken_astSelectorExpr
     | AlexRawToken_Sel
     -- last keywords third part

     -- ***************
     -- *             *
     -- * parentheses *
     -- *             *
     -- ***************

     | AlexRawToken_LPAREN
     | AlexRawToken_RPAREN
     | AlexRawToken_LBRACK
     | AlexRawToken_RBRACK
     | AlexRawToken_LBRACE
     | AlexRawToken_RBRACE

     -- *************
     -- *           *
     -- * operators *
     -- *           *
     -- *************

     | AlexRawToken_MINUS
     | AlexRawToken_TIMES

     -- ***************
     -- *             *
     -- * punctuation *
     -- *             *
     -- ***************

     | AlexRawToken_COLON
     | AlexRawToken_COMMA
     | AlexRawToken_DOT
     | AlexRawToken_EQ
 
     -- *******
     -- *     *
     -- * EOF *
     -- *     *
     -- *******
     | TokenEOF -- ^ [EOF](https://en.wikipedia.org/wiki/End-of-file)
 
     deriving ( Show )

-- ***********
-- * alexEOF *
-- ***********
alexEOF :: Alex AlexTokenTag
alexEOF = do
    ((AlexPn _ l c),_,_,_) <- alexGetInput
    alexUserState <- alexGetUserState
    return $
        AlexTokenTag
        {
            tokenRaw = TokenEOF,
            tokenLoc = Location {
                lineStart = fromIntegral l,
                lineEnd = fromIntegral l,
                colStart = fromIntegral c,
                colEnd = fromIntegral c,
                filename = (filepath alexUserState)
            }
        }

-- *******
-- *     *
-- * lex *
-- *     *
-- *******
lex :: (String -> AlexRawToken) -> AlexInput -> Int -> Alex AlexTokenTag
lex f ((AlexPn _ l c),_,_,str) i = do
    alexUserState <- alexGetUserState
    return $
        AlexTokenTag
        {
            tokenRaw = (f (take i str)),
            tokenLoc = Location {
                lineStart = fromIntegral l,
                lineEnd = fromIntegral l,
                colStart = fromIntegral c,
                colEnd = fromIntegral (c+i),
                filename = (filepath alexUserState)
            }
        }

-- *********************************************
-- * lex' for tokens WITHOUT associated values *
-- *********************************************
lex' :: AlexRawToken -> AlexInput -> Int -> Alex AlexTokenTag
lex' = lex . const

lexicalError :: AlexInput -> Int -> Alex AlexTokenTag
lexicalError ((AlexPn _ l c),_,_,str) i = alexEOF

-- **************
-- * alexError' *
-- **************
alexError' :: Location -> Alex a
alexError' location = alexError $ "Error[ " ++ show location ++ " ]"

-- ************
-- *          *
-- * location *
-- *          *
-- ************
location :: AlexTokenTag -> Location
location = tokenLoc

-- ***************
-- *             *
-- * getFilename *
-- *             *
-- ***************
getFilename :: AlexTokenTag -> String
getFilename = Location.filename . location

-- ***************
-- *             *
-- * tokIntValue *
-- *             *
-- ***************
tokIntValue :: AlexTokenTag -> Int
tokIntValue t = case (tokenRaw t) of { AlexRawToken_INT i -> i; _ -> 0; }

-- ***************
-- *             *
-- * tokIntValue *
-- *             *
-- ***************
tokIDValue :: AlexTokenTag -> String
tokIDValue t = case (tokenRaw t) of { AlexRawToken_ID s -> s; _ -> "" }

-- ************
-- *          *
-- * runAlex' *
-- *          *
-- ************
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)
}
