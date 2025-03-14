{

{-# OPTIONS -w  #-}

module CsLexer

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

-- ************
-- *          *
-- * keywords *
-- *          *
-- ************

@KIND = kind
@NULL = null
@VALUE = value
@LOCATION = location
@START_LINE = startLine
@START_COLUMN = startColumn
@END_LINE = endLine
@END_COLUMN = endColumn
@CHILDREN = children
@COMPILATION_UNIT = CompilationUnit
@USING_DIRECTIVE = UsingDirective
@IDENTIFIER_NAME = IdentifierName
@QUALIFIED_NAME = QualifiedName
@NAME_EQUALS = NameEquals
@NAMESPACE_DECLARATION = NamespaceDeclaration
@CLASS_DECLARATION = ClassDeclaration
@SIMPLE_BASE_TYPE = SimpleBaseType
@BASE_LIST = BaseList
@ARGUMENT_LIST = ArgumentList
@OBJECT_CREATION_EXPRESSION = ObjectCreationExpression
@EQUALS_VALUE_CLAUSE = EqualsValueClause
@VARIABLE_DECLARATOR = VariableDeclarator
@TYPE_ARGUMENT_LIST = TypeArgumentList
@GENERIC_NAME = GenericName
@VARIABLE_DECLARATION = VariableDeclaration
@FIELD_DECLARATION = FieldDeclaration
@ELEMENT_ACCESS_EXPRESSION = ElementAccessExpression
@BRACKETED_ARGUMENT_LIST = BracketedArgumentList
@ARGUMENT = Argument
@NULLABLE_TYPE = NullableType
@METHOD_DECLARATION = MethodDeclaration
@PARAMETER_LIST = ParameterList
@BLOCK = Block
@IF_STATEMENT = IfStatement
@EQUALS_EXPRESSION = EqualsExpression
@NULL_LITERAL_EXPRESSION = NullLiteralExpression
@WHILE_STATEMENT = WhileStatement
@INVOCATION_EXPRESSION = InvocationExpression
@SIMPLE_MEMBER_ACCESS_EXPRESSION = SimpleMemberAccessExpression
@EXPRESSION_STATEMENT = ExpressionStatement
@SIMPLE_ASSIGNMENT_EXPRESSION = SimpleAssignmentExpression
@IS_PATTERN_EXPRESSION = IsPatternExpression
@DECLARATION_PATTERN = DeclarationPattern
@SINGLE_VARIABLE_DESIGNATION = SingleVariableDesignation
@ELSE_CLAUSE = ElseClause
@BREAK_STATEMENT = BreakStatement
@RETURN_STATEMENT = ReturnStatement
@CONSTRUCTOR_DECLARATION = ConstructorDeclaration
@PARAMETER = Parameter
@THIS_CONSTRUCTOR_INITIALIZER = ThisConstructorInitializer
@THIS_EXPRESSION = ThisExpression
@PROPERTY_DECLARATION = PropertyDeclaration
@ACCESSOR_LIST = AccessorList
@GET_ACCESSOR_DECLARATION = GetAccessorDeclaration
@SET_ACCESSOR_DECLARATION = SetAccessorDeclaration
@PREDEFINED_TYPE = PredefinedType
@FALSE_LITERAL_EXPRESSION = FalseLiteralExpression
@TRUE_LITERAL_EXPRESSION = TrueLiteralExpression
@NUMERIC_LITERAL_EXPRESSION = NumericLiteralExpression

-- ************
-- *          *
-- * integers *
-- *          *
-- ************
@DIGIT = 0-9
@LETTER = [a-zA-Z_]
@LETTER_OR_DIGIT = @LETTER | @DIGIT
@INT = @DIGIT+ 
@ID = (@LETTER)(@LETTER_OR_DIGIT+)

-- ***************
-- *             *
-- * identifiers *
-- *             *
-- ***************

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

-- ************
-- *          *
-- * keywords *
-- *          *
-- ************

@KIND { lex' AlexRawToken_KIND }
@NULL { lex' AlexRawToken_NULL }
@VALUE { lex' AlexRawToken_VALUE }
@LOCATION { lex' AlexRawToken_LOCATION }
@START_LINE { lex' AlexRawToken_START_LINE }
@START_COLUMN { lex' AlexRawToken_START_COLUMN }
@END_LINE { lex' AlexRawToken_END_LINE }
@END_COLUMN { lex' AlexRawToken_END_COLUMN }
@CHILDREN { lex' AlexRawToken_CHILDREN }
@COMPILATION_UNIT { lex' AlexRawToken_COMPILATION_UNIT }
@USING_DIRECTIVE { lex' AlexRawToken_USING_DIRECTIVE }
@IDENTIFIER_NAME { lex' AlexRawToken_IDENTIFIER_NAME }
@QUALIFIED_NAME { lex' AlexRawToken_QUALIFIED_NAME }
@NAME_EQUALS { lex' AlexRawToken_NAME_EQUALS }
@NAMESPACE_DECLARATION { lex' AlexRawToken_NAMESPACE_DECLARATION }
@CLASS_DECLARATION { lex' AlexRawToken_CLASS_DECLARATION }
@SIMPLE_BASE_TYPE { lex' AlexRawToken_SIMPLE_BASE_TYPE }
@BASE_LIST { lex' AlexRawToken_BASE_LIST }
@ARGUMENT_LIST { lex' AlexRawToken_ARGUMENT_LIST }
@OBJECT_CREATION_EXPRESSION { lex' AlexRawToken_OBJECT_CREATION_EXPRESSION }
@EQUALS_VALUE_CLAUSE { lex' AlexRawToken_EQUALS_VALUE_CLAUSE }
@VARIABLE_DECLARATOR { lex' AlexRawToken_VARIABLE_DECLARATOR }
@TYPE_ARGUMENT_LIST { lex' AlexRawToken_TYPE_ARGUMENT_LIST }
@GENERIC_NAME { lex' AlexRawToken_GENERIC_NAME }
@VARIABLE_DECLARATION { lex' AlexRawToken_VARIABLE_DECLARATION }
@FIELD_DECLARATION { lex' AlexRawToken_FIELD_DECLARATION }
@ELEMENT_ACCESS_EXPRESSION { lex' AlexRawToken_ELEMENT_ACCESS_EXPRESSION }
@BRACKETED_ARGUMENT_LIST { lex' AlexRawToken_BRACKETED_ARGUMENT_LIST }
@ARGUMENT { lex' AlexRawToken_ARGUMENT }
@NULLABLE_TYPE { lex' AlexRawToken_NULLABLE_TYPE }
@METHOD_DECLARATION { lex' AlexRawToken_METHOD_DECLARATION }
@PARAMETER_LIST { lex' AlexRawToken_PARAMETER_LIST }
@BLOCK { lex' AlexRawToken_BLOCK }
@IF_STATEMENT { lex' AlexRawToken_IF_STATEMENT }
@EQUALS_EXPRESSION { lex' AlexRawToken_EQUALS_EXPRESSION }
@NULL_LITERAL_EXPRESSION { lex' AlexRawToken_NULL_LITERAL_EXPRESSION }
@WHILE_STATEMENT { lex' AlexRawToken_WHILE_STATEMENT }
@INVOCATION_EXPRESSION { lex' AlexRawToken_INVOCATION_EXPRESSION }
@SIMPLE_MEMBER_ACCESS_EXPRESSION { lex' AlexRawToken_SIMPLE_MEMBER_ACCESS_EXPRESSION }
@EXPRESSION_STATEMENT { lex' AlexRawToken_EXPRESSION_STATEMENT }
@SIMPLE_ASSIGNMENT_EXPRESSION { lex' AlexRawToken_SIMPLE_ASSIGNMENT_EXPRESSION }
@IS_PATTERN_EXPRESSION { lex' AlexRawToken_IS_PATTERN_EXPRESSION }
@DECLARATION_PATTERN { lex' AlexRawToken_DECLARATION_PATTERN }
@SINGLE_VARIABLE_DESIGNATION { lex' AlexRawToken_SINGLE_VARIABLE_DESIGNATION }
@ELSE_CLAUSE { lex' AlexRawToken_ELSE_CLAUSE }
@BREAK_STATEMENT { lex' AlexRawToken_BREAK_STATEMENT }
@RETURN_STATEMENT { lex' AlexRawToken_RETURN_STATEMENT }
@CONSTRUCTOR_DECLARATION { lex' AlexRawToken_CONSTRUCTOR_DECLARATION }
@PARAMETER { lex' AlexRawToken_PARAMETER }
@THIS_CONSTRUCTOR_INITIALIZER { lex' AlexRawToken_THIS_CONSTRUCTOR_INITIALIZER }
@THIS_EXPRESSION { lex' AlexRawToken_THIS_EXPRESSION }
@PROPERTY_DECLARATION { lex' AlexRawToken_PROPERTY_DECLARATION }
@ACCESSOR_LIST { lex' AlexRawToken_ACCESSOR_LIST }
@GET_ACCESSOR_DECLARATION { lex' AlexRawToken_GET_ACCESSOR_DECLARATION }
@SET_ACCESSOR_DECLARATION { lex' AlexRawToken_SET_ACCESSOR_DECLARATION }
@PREDEFINED_TYPE { lex' AlexRawToken_PREDEFINED_TYPE }
@FALSE_LITERAL_EXPRESSION { lex' AlexRawToken_FALSE_LITERAL_EXPRESSION }
@TRUE_LITERAL_EXPRESSION { lex' AlexRawToken_TRUE_LITERAL_EXPRESSION }
@NUMERIC_LITERAL_EXPRESSION { lex' AlexRawToken_NUMERIC_LITERAL_EXPRESSION }

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

     -- ************
     -- *          *
     -- * keywords *
     -- *          *
     -- ************

     | AlexRawToken_KIND
     | AlexRawToken_NULL
     | AlexRawToken_VALUE
     | AlexRawToken_LOCATION
     | AlexRawToken_START_LINE
     | AlexRawToken_START_COLUMN
     | AlexRawToken_END_LINE
     | AlexRawToken_END_COLUMN
     | AlexRawToken_CHILDREN
     | AlexRawToken_COMPILATION_UNIT
     | AlexRawToken_USING_DIRECTIVE
     | AlexRawToken_IDENTIFIER_NAME
     | AlexRawToken_QUALIFIED_NAME
     | AlexRawToken_NAME_EQUALS
     | AlexRawToken_NAMESPACE_DECLARATION
     | AlexRawToken_CLASS_DECLARATION
     | AlexRawToken_SIMPLE_BASE_TYPE
     | AlexRawToken_BASE_LIST
     | AlexRawToken_ARGUMENT_LIST
     | AlexRawToken_OBJECT_CREATION_EXPRESSION
     | AlexRawToken_EQUALS_VALUE_CLAUSE
     | AlexRawToken_VARIABLE_DECLARATOR
     | AlexRawToken_TYPE_ARGUMENT_LIST
     | AlexRawToken_GENERIC_NAME
     | AlexRawToken_VARIABLE_DECLARATION
     | AlexRawToken_FIELD_DECLARATION
     | AlexRawToken_ELEMENT_ACCESS_EXPRESSION
     | AlexRawToken_BRACKETED_ARGUMENT_LIST
     | AlexRawToken_ARGUMENT
     | AlexRawToken_NULLABLE_TYPE
     | AlexRawToken_METHOD_DECLARATION
     | AlexRawToken_PARAMETER_LIST
     | AlexRawToken_BLOCK
     | AlexRawToken_IF_STATEMENT
     | AlexRawToken_EQUALS_EXPRESSION
     | AlexRawToken_NULL_LITERAL_EXPRESSION
     | AlexRawToken_WHILE_STATEMENT
     | AlexRawToken_INVOCATION_EXPRESSION
     | AlexRawToken_SIMPLE_MEMBER_ACCESS_EXPRESSION
     | AlexRawToken_EXPRESSION_STATEMENT
     | AlexRawToken_SIMPLE_ASSIGNMENT_EXPRESSION
     | AlexRawToken_IS_PATTERN_EXPRESSION
     | AlexRawToken_DECLARATION_PATTERN
     | AlexRawToken_SINGLE_VARIABLE_DESIGNATION
     | AlexRawToken_ELSE_CLAUSE
     | AlexRawToken_BREAK_STATEMENT
     | AlexRawToken_RETURN_STATEMENT
     | AlexRawToken_CONSTRUCTOR_DECLARATION
     | AlexRawToken_PARAMETER
     | AlexRawToken_THIS_CONSTRUCTOR_INITIALIZER
     | AlexRawToken_THIS_EXPRESSION
     | AlexRawToken_PROPERTY_DECLARATION
     | AlexRawToken_ACCESSOR_LIST
     | AlexRawToken_GET_ACCESSOR_DECLARATION
     | AlexRawToken_SET_ACCESSOR_DECLARATION
     | AlexRawToken_PREDEFINED_TYPE
     | AlexRawToken_FALSE_LITERAL_EXPRESSION
     | AlexRawToken_TRUE_LITERAL_EXPRESSION
     | AlexRawToken_NUMERIC_LITERAL_EXPRESSION

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

     -- ***************
     -- *             *
     -- * punctuation *
     -- *             *
     -- ***************

     | AlexRawToken_COLON
     | AlexRawToken_COMMA
 
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

