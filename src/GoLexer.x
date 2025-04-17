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
@Y = Y
@Op = Op
@OpPos = OpPos
@astBinaryExpr = "*ast.BinaryExpr"
@KW_INT = INT
@type = type
@astField = "*ast.Field"
@Tag = Tag
@astFieldList = "*ast.FieldList"
@Opening = Opening
@List = List
@Closing = Closing
@astStructType = "*ast.StructType"
@Struct = Struct
@Fields = Fields
@Incomplete = Incomplete
@false = false
@astTypeSpec = "*ast.TypeSpec"
@TypeParams = TypeParams
@Assign = Assign
@astFuncType = "*ast.FuncType"
@Func = Func
@Params = Params
@Results = Results
@astBlockStmt = "*ast.BlockStmt"
@Lbrace = Lbrace
@Rbrace = Rbrace
@astFuncDecl = "*ast.FuncDecl"
@Recv = Recv
@Body = Body
@func = func
@Star = Star
@astStarExpr = "*ast.StarExpr"
@astStmt = "ast.Stmt"
@astDeclStmt = "*ast.DeclStmt"
@If = If
@Init = Init
@Cond = Cond
@Else = Else
@astIfStmt = "*ast.IfStmt"
@Lhs = Lhs
@Rhs = Rhs
@astAssignStmt = "*ast.AssignStmt"
@OP_ASSIGN = ":="
@astUnaryExpr = "*ast.UnaryExpr"
@ampersand = "&"
@OP_NEQ = "!="
@astExprStmt = "*ast.ExprStmt"
@astReturnStmt = "*ast.ReturnStmt"
@Return = Return
@OP_AND = "&&"
@OP_GEQ = ">="
@OP_PLUS = "+"
@OP_BANG = "!"
@Key = Key
@Colon = Colon
@Elts = Elts
@astKeyValueExpr = "*ast.KeyValueExpr"
@astCompositeLit = "*ast.CompositeLit"
@astTypeAssertExpr = "*ast.TypeAssertExpr"
@FileStart = FileStart
@FileEnd = FileEnd
@Scope = Scope
@Imports = Imports
@Unresolved = Unresolved
@Comments = Comments
@Outer = Outer
@Objects = Objects
@map = map
@string = string
@astScope = "*ast.Scope"
@Low = Low
@High = High
@astSliceExpr = "*ast.SliceExpr"
@Max = Max
@Slice3 = Slice3
@Elt = Elt
@Len = Len
@astArrayType = "*ast.ArrayType"
@OP_LEQ = "<="
@astParenExpr = "*ast.ParenExpr"
@OP_EQEQ = "=="
@astInterfaceType = "*ast.InterfaceType"
@Interface = Interface
@Methods = Methods
@OP_OR = "||"
@For = For
@Post = Post
@astForStmt = "*ast.ForStmt"
@astIncDecStmt = "*ast.IncDecStmt"
@OP_PLUSPLUS = "++"
@Label = Label
@continue = continue
@astBranchStmt = "*ast.BranchStmt"
@OP_GT = ">"
@Select = Select
@astSelectStmt = "*ast.SelectStmt"
@Comm = Comm
@Case = Case
@astCommClause = "*ast.CommClause"
@OP_LARROW = "<-"
@break = break
@Go = Go
@Call = Call
@astGoStmt = "*ast.GoStmt"
@astFuncLit = "*ast.FuncLit"
@OP_BITWISE_OR = "|"
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
@Y {lex' AlexRawToken_Y}
@Op {lex' AlexRawToken_Op}
@OpPos {lex' AlexRawToken_OpPos}
@astBinaryExpr {lex' AlexRawToken_astBinaryExpr}
@KW_INT {lex' AlexRawToken_KW_INT}
@type {lex' AlexRawToken_type}
@astField {lex' AlexRawToken_astField}
@Tag {lex' AlexRawToken_Tag}
@astFieldList {lex' AlexRawToken_astFieldList}
@Opening {lex' AlexRawToken_Opening}
@List {lex' AlexRawToken_List}
@Closing {lex' AlexRawToken_Closing}
@astStructType {lex' AlexRawToken_astStructType}
@Struct {lex' AlexRawToken_Struct}
@Fields {lex' AlexRawToken_Fields}
@Incomplete {lex' AlexRawToken_Incomplete}
@false {lex' AlexRawToken_false}
@astTypeSpec {lex' AlexRawToken_astTypeSpec}
@TypeParams {lex' AlexRawToken_TypeParams}
@Assign {lex' AlexRawToken_Assign}
@astFuncType {lex' AlexRawToken_astFuncType}
@Func {lex' AlexRawToken_Func}
@Params {lex' AlexRawToken_Params}
@Results {lex' AlexRawToken_Results}
@astBlockStmt {lex' AlexRawToken_astBlockStmt}
@Lbrace {lex' AlexRawToken_Lbrace}
@Rbrace {lex' AlexRawToken_Rbrace}
@astFuncDecl {lex' AlexRawToken_astFuncDecl}
@Recv {lex' AlexRawToken_Recv}
@Body {lex' AlexRawToken_Body}
@func {lex' AlexRawToken_func}
@Star {lex' AlexRawToken_Star}
@astStarExpr {lex' AlexRawToken_astStarExpr}
@astStmt {lex' AlexRawToken_astStmt}
@astDeclStmt {lex' AlexRawToken_astDeclStmt}
@If {lex' AlexRawToken_If}
@Init {lex' AlexRawToken_Init}
@Cond {lex' AlexRawToken_Cond}
@Else {lex' AlexRawToken_Else}
@astIfStmt {lex' AlexRawToken_astIfStmt}
@Lhs {lex' AlexRawToken_Lhs}
@Rhs {lex' AlexRawToken_Rhs}
@astAssignStmt {lex' AlexRawToken_astAssignStmt}
@OP_ASSIGN {lex' AlexRawToken_OP_ASSIGN}
@astUnaryExpr {lex' AlexRawToken_astUnaryExpr}
@ampersand {lex' AlexRawToken_ampersand}
@OP_NEQ {lex' AlexRawToken_OP_NEQ}
@astExprStmt {lex' AlexRawToken_astExprStmt}
@astReturnStmt {lex' AlexRawToken_astReturnStmt}
@Return {lex' AlexRawToken_Return}
@OP_AND {lex' AlexRawToken_OP_AND}
@OP_GEQ {lex' AlexRawToken_OP_GEQ}
@OP_PLUS {lex' AlexRawToken_OP_PLUS}
@OP_BANG {lex' AlexRawToken_OP_BANG}
@Key {lex' AlexRawToken_Key}
@Colon {lex' AlexRawToken_Colon}
@Elts {lex' AlexRawToken_Elts}
@astKeyValueExpr {lex' AlexRawToken_astKeyValueExpr}
@astCompositeLit {lex' AlexRawToken_astCompositeLit}
@astTypeAssertExpr {lex' AlexRawToken_astTypeAssertExpr}
@FileStart {lex' AlexRawToken_FileStart}
@FileEnd {lex' AlexRawToken_FileEnd}
@Scope {lex' AlexRawToken_Scope}
@Imports {lex' AlexRawToken_Imports}
@Unresolved {lex' AlexRawToken_Unresolved}
@Comments {lex' AlexRawToken_Comments}
@Outer {lex' AlexRawToken_Outer}
@Objects {lex' AlexRawToken_Objects}
@map {lex' AlexRawToken_map}
@string {lex' AlexRawToken_string}
@astScope {lex' AlexRawToken_astScope}
@Low {lex' AlexRawToken_Low}
@High {lex' AlexRawToken_High}
@astSliceExpr {lex' AlexRawToken_astSliceExpr}
@Max {lex' AlexRawToken_Max}
@Slice3 {lex' AlexRawToken_Slice3}
@Elt {lex' AlexRawToken_Elt}
@Len {lex' AlexRawToken_Len}
@astArrayType {lex' AlexRawToken_astArrayType}
@OP_LEQ {lex' AlexRawToken_OP_LEQ}
@astParenExpr {lex' AlexRawToken_astParenExpr}
@OP_EQEQ {lex' AlexRawToken_OP_EQEQ}
@astInterfaceType {lex' AlexRawToken_astInterfaceType}
@Interface {lex' AlexRawToken_Interface}
@Methods {lex' AlexRawToken_Methods}
@OP_OR {lex' AlexRawToken_OP_OR}
@For {lex' AlexRawToken_For}
@Post {lex' AlexRawToken_Post}
@astForStmt {lex' AlexRawToken_astForStmt}
@astIncDecStmt {lex' AlexRawToken_astIncDecStmt}
@OP_PLUSPLUS {lex' AlexRawToken_OP_PLUSPLUS}
@Label {lex' AlexRawToken_Label}
@continue {lex' AlexRawToken_continue}
@astBranchStmt {lex' AlexRawToken_astBranchStmt}
@OP_GT {lex' AlexRawToken_OP_GT}
@Select {lex' AlexRawToken_Select}
@astSelectStmt {lex' AlexRawToken_astSelectStmt}
@Comm {lex' AlexRawToken_Comm}
@Case {lex' AlexRawToken_Case}
@astCommClause {lex' AlexRawToken_astCommClause}
@OP_LARROW {lex' AlexRawToken_OP_LARROW}
@break {lex' AlexRawToken_break}
@Go {lex' AlexRawToken_Go}
@Call {lex' AlexRawToken_Call}
@astGoStmt {lex' AlexRawToken_astGoStmt}
@astFuncLit {lex' AlexRawToken_astFuncLit}
@OP_BITWISE_OR {lex' AlexRawToken_OP_BITWISE_OR}
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
     | AlexRawToken_Y
     | AlexRawToken_Op
     | AlexRawToken_OpPos
     | AlexRawToken_astBinaryExpr
     | AlexRawToken_KW_INT
     | AlexRawToken_type
     | AlexRawToken_astField
     | AlexRawToken_Tag
     | AlexRawToken_astFieldList
     | AlexRawToken_Opening
     | AlexRawToken_List
     | AlexRawToken_Closing
     | AlexRawToken_astStructType
     | AlexRawToken_Struct
     | AlexRawToken_Fields
     | AlexRawToken_Incomplete
     | AlexRawToken_false
     | AlexRawToken_astTypeSpec
     | AlexRawToken_TypeParams
     | AlexRawToken_Assign
     | AlexRawToken_astFuncType
     | AlexRawToken_Func
     | AlexRawToken_Params
     | AlexRawToken_Results
     | AlexRawToken_astBlockStmt
     | AlexRawToken_Lbrace
     | AlexRawToken_Rbrace
     | AlexRawToken_astFuncDecl
     | AlexRawToken_Recv
     | AlexRawToken_Body
     | AlexRawToken_func
     | AlexRawToken_Star
     | AlexRawToken_astStarExpr
     | AlexRawToken_astStmt
     | AlexRawToken_astDeclStmt
     | AlexRawToken_If
     | AlexRawToken_Init
     | AlexRawToken_Cond
     | AlexRawToken_Else
     | AlexRawToken_astIfStmt
     | AlexRawToken_Lhs
     | AlexRawToken_Rhs
     | AlexRawToken_astAssignStmt
     | AlexRawToken_OP_ASSIGN
     | AlexRawToken_astUnaryExpr
     | AlexRawToken_ampersand
     | AlexRawToken_OP_NEQ
     | AlexRawToken_astExprStmt
     | AlexRawToken_astReturnStmt
     | AlexRawToken_Return
     | AlexRawToken_OP_AND
     | AlexRawToken_OP_GEQ
     | AlexRawToken_OP_PLUS
     | AlexRawToken_OP_BANG
     | AlexRawToken_Key
     | AlexRawToken_Colon
     | AlexRawToken_Elts
     | AlexRawToken_astKeyValueExpr
     | AlexRawToken_astCompositeLit
     | AlexRawToken_astTypeAssertExpr
     | AlexRawToken_FileStart
     | AlexRawToken_FileEnd
     | AlexRawToken_Scope
     | AlexRawToken_Imports
     | AlexRawToken_Unresolved
     | AlexRawToken_Comments
     | AlexRawToken_Outer
     | AlexRawToken_Objects
     | AlexRawToken_map
     | AlexRawToken_string
     | AlexRawToken_astScope
     | AlexRawToken_Low
     | AlexRawToken_High
     | AlexRawToken_astSliceExpr
     | AlexRawToken_Max
     | AlexRawToken_Slice3
     | AlexRawToken_Elt
     | AlexRawToken_Len
     | AlexRawToken_astArrayType
     | AlexRawToken_OP_LEQ
     | AlexRawToken_astParenExpr
     | AlexRawToken_OP_EQEQ
     | AlexRawToken_astInterfaceType
     | AlexRawToken_Interface
     | AlexRawToken_Methods
     | AlexRawToken_OP_OR
     | AlexRawToken_For
     | AlexRawToken_Post
     | AlexRawToken_astForStmt
     | AlexRawToken_astIncDecStmt
     | AlexRawToken_OP_PLUSPLUS
     | AlexRawToken_Label
     | AlexRawToken_continue
     | AlexRawToken_astBranchStmt
     | AlexRawToken_OP_GT
     | AlexRawToken_Select
     | AlexRawToken_astSelectStmt
     | AlexRawToken_Comm
     | AlexRawToken_Case
     | AlexRawToken_astCommClause
     | AlexRawToken_OP_LARROW
     | AlexRawToken_break
     | AlexRawToken_Go
     | AlexRawToken_Call
     | AlexRawToken_astGoStmt
     | AlexRawToken_astFuncLit
     | AlexRawToken_OP_BITWISE_OR
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
tokIDValue t = case (tokenRaw t) of { AlexRawToken_QUOTED_ID s -> s; _ -> "" }

-- ************
-- *          *
-- * runAlex' *
-- *          *
-- ************
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)
}
