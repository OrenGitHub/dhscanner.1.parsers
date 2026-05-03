{
{-# OPTIONS -Werror=missing-fields #-}

module TsParser( parseProgram ) where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
import Ast
import TsLexer
import Location
import qualified TsParserActions as Actions
import qualified Token
import qualified Common

-- *******************
-- *                 *
-- * general imports *
-- *                 *
-- *******************
import Data.List ( map, stripPrefix, isPrefixOf )
import Data.Map ( empty, fromList )

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

-- ************
-- *          *
-- * location *
-- *          *
-- ************

':' { AlexTokenTag AlexRawToken_COLON _ _ }
',' { AlexTokenTag AlexRawToken_COMMA _ _ }
'-' { AlexTokenTag AlexRawToken_MINUS _ _ }

-- reserved keywords start
'Unknown' { AlexTokenTag AlexRawToken_Unknown _ _ }
'EndOfFileToken' { AlexTokenTag AlexRawToken_EndOfFileToken _ _ }
'SingleLineCommentTrivia' { AlexTokenTag AlexRawToken_SingleLineCommentTrivia _ _ }
'MultiLineCommentTrivia' { AlexTokenTag AlexRawToken_MultiLineCommentTrivia _ _ }
'NewLineTrivia' { AlexTokenTag AlexRawToken_NewLineTrivia _ _ }
'WhitespaceTrivia' { AlexTokenTag AlexRawToken_WhitespaceTrivia _ _ }
'ShebangTrivia' { AlexTokenTag AlexRawToken_ShebangTrivia _ _ }
'ConflictMarkerTrivia' { AlexTokenTag AlexRawToken_ConflictMarkerTrivia _ _ }
'NonTextFileMarkerTrivia' { AlexTokenTag AlexRawToken_NonTextFileMarkerTrivia _ _ }
'NumericLiteral' { AlexTokenTag AlexRawToken_NumericLiteral _ _ }
'BigIntLiteral' { AlexTokenTag AlexRawToken_BigIntLiteral _ _ }
'StringLiteral' { AlexTokenTag AlexRawToken_StringLiteral _ _ }
'JsxText' { AlexTokenTag AlexRawToken_JsxText _ _ }
'JsxTextAllWhiteSpaces' { AlexTokenTag AlexRawToken_JsxTextAllWhiteSpaces _ _ }
'RegularExpressionLiteral' { AlexTokenTag AlexRawToken_RegularExpressionLiteral _ _ }
'NoSubstitutionTemplateLiteral' { AlexTokenTag AlexRawToken_NoSubstitutionTemplateLiteral _ _ }
'TemplateHead' { AlexTokenTag AlexRawToken_TemplateHead _ _ }
'TemplateMiddle' { AlexTokenTag AlexRawToken_TemplateMiddle _ _ }
'TemplateTail' { AlexTokenTag AlexRawToken_TemplateTail _ _ }
'OpenBraceToken' { AlexTokenTag AlexRawToken_OpenBraceToken _ _ }
'CloseBraceToken' { AlexTokenTag AlexRawToken_CloseBraceToken _ _ }
'OpenParenToken' { AlexTokenTag AlexRawToken_OpenParenToken _ _ }
'CloseParenToken' { AlexTokenTag AlexRawToken_CloseParenToken _ _ }
'OpenBracketToken' { AlexTokenTag AlexRawToken_OpenBracketToken _ _ }
'CloseBracketToken' { AlexTokenTag AlexRawToken_CloseBracketToken _ _ }
'DotToken' { AlexTokenTag AlexRawToken_DotToken _ _ }
'DotDotDotToken' { AlexTokenTag AlexRawToken_DotDotDotToken _ _ }
'SemicolonToken' { AlexTokenTag AlexRawToken_SemicolonToken _ _ }
'CommaToken' { AlexTokenTag AlexRawToken_CommaToken _ _ }
'QuestionDotToken' { AlexTokenTag AlexRawToken_QuestionDotToken _ _ }
'LessThanToken' { AlexTokenTag AlexRawToken_LessThanToken _ _ }
'LessThanSlashToken' { AlexTokenTag AlexRawToken_LessThanSlashToken _ _ }
'GreaterThanToken' { AlexTokenTag AlexRawToken_GreaterThanToken _ _ }
'LessThanEqualsToken' { AlexTokenTag AlexRawToken_LessThanEqualsToken _ _ }
'GreaterThanEqualsToken' { AlexTokenTag AlexRawToken_GreaterThanEqualsToken _ _ }
'EqualsEqualsToken' { AlexTokenTag AlexRawToken_EqualsEqualsToken _ _ }
'ExclamationEqualsToken' { AlexTokenTag AlexRawToken_ExclamationEqualsToken _ _ }
'EqualsEqualsEqualsToken' { AlexTokenTag AlexRawToken_EqualsEqualsEqualsToken _ _ }
'ExclamationEqualsEqualsToken' { AlexTokenTag AlexRawToken_ExclamationEqualsEqualsToken _ _ }
'EqualsGreaterThanToken' { AlexTokenTag AlexRawToken_EqualsGreaterThanToken _ _ }
'PlusToken' { AlexTokenTag AlexRawToken_PlusToken _ _ }
'MinusToken' { AlexTokenTag AlexRawToken_MinusToken _ _ }
'AsteriskToken' { AlexTokenTag AlexRawToken_AsteriskToken _ _ }
'AsteriskAsteriskToken' { AlexTokenTag AlexRawToken_AsteriskAsteriskToken _ _ }
'SlashToken' { AlexTokenTag AlexRawToken_SlashToken _ _ }
'PercentToken' { AlexTokenTag AlexRawToken_PercentToken _ _ }
'PlusPlusToken' { AlexTokenTag AlexRawToken_PlusPlusToken _ _ }
'MinusMinusToken' { AlexTokenTag AlexRawToken_MinusMinusToken _ _ }
'LessThanLessThanToken' { AlexTokenTag AlexRawToken_LessThanLessThanToken _ _ }
'GreaterThanGreaterThanToken' { AlexTokenTag AlexRawToken_GreaterThanGreaterThanToken _ _ }
'GreaterThanGreaterThanGreaterThanToken' { AlexTokenTag AlexRawToken_GreaterThanGreaterThanGreaterThanToken _ _ }
'AmpersandToken' { AlexTokenTag AlexRawToken_AmpersandToken _ _ }
'BarToken' { AlexTokenTag AlexRawToken_BarToken _ _ }
'CaretToken' { AlexTokenTag AlexRawToken_CaretToken _ _ }
'ExclamationToken' { AlexTokenTag AlexRawToken_ExclamationToken _ _ }
'TildeToken' { AlexTokenTag AlexRawToken_TildeToken _ _ }
'AmpersandAmpersandToken' { AlexTokenTag AlexRawToken_AmpersandAmpersandToken _ _ }
'BarBarToken' { AlexTokenTag AlexRawToken_BarBarToken _ _ }
'QuestionToken' { AlexTokenTag AlexRawToken_QuestionToken _ _ }
'ColonToken' { AlexTokenTag AlexRawToken_ColonToken _ _ }
'AtToken' { AlexTokenTag AlexRawToken_AtToken _ _ }
'QuestionQuestionToken' { AlexTokenTag AlexRawToken_QuestionQuestionToken _ _ }
'BacktickToken' { AlexTokenTag AlexRawToken_BacktickToken _ _ }
'HashToken' { AlexTokenTag AlexRawToken_HashToken _ _ }
'EqualsToken' { AlexTokenTag AlexRawToken_EqualsToken _ _ }
'PlusEqualsToken' { AlexTokenTag AlexRawToken_PlusEqualsToken _ _ }
'MinusEqualsToken' { AlexTokenTag AlexRawToken_MinusEqualsToken _ _ }
'AsteriskEqualsToken' { AlexTokenTag AlexRawToken_AsteriskEqualsToken _ _ }
'AsteriskAsteriskEqualsToken' { AlexTokenTag AlexRawToken_AsteriskAsteriskEqualsToken _ _ }
'SlashEqualsToken' { AlexTokenTag AlexRawToken_SlashEqualsToken _ _ }
'PercentEqualsToken' { AlexTokenTag AlexRawToken_PercentEqualsToken _ _ }
'LessThanLessThanEqualsToken' { AlexTokenTag AlexRawToken_LessThanLessThanEqualsToken _ _ }
'GreaterThanGreaterThanEqualsToken' { AlexTokenTag AlexRawToken_GreaterThanGreaterThanEqualsToken _ _ }
'GreaterThanGreaterThanGreaterThanEqualsToken' { AlexTokenTag AlexRawToken_GreaterThanGreaterThanGreaterThanEqualsToken _ _ }
'AmpersandEqualsToken' { AlexTokenTag AlexRawToken_AmpersandEqualsToken _ _ }
'BarEqualsToken' { AlexTokenTag AlexRawToken_BarEqualsToken _ _ }
'BarBarEqualsToken' { AlexTokenTag AlexRawToken_BarBarEqualsToken _ _ }
'AmpersandAmpersandEqualsToken' { AlexTokenTag AlexRawToken_AmpersandAmpersandEqualsToken _ _ }
'QuestionQuestionEqualsToken' { AlexTokenTag AlexRawToken_QuestionQuestionEqualsToken _ _ }
'CaretEqualsToken' { AlexTokenTag AlexRawToken_CaretEqualsToken _ _ }
'Identifier' { AlexTokenTag AlexRawToken_Identifier _ _ }
'PrivateIdentifier' { AlexTokenTag AlexRawToken_PrivateIdentifier _ _ }
'BreakKeyword' { AlexTokenTag AlexRawToken_BreakKeyword _ _ }
'CaseKeyword' { AlexTokenTag AlexRawToken_CaseKeyword _ _ }
'CatchKeyword' { AlexTokenTag AlexRawToken_CatchKeyword _ _ }
'ClassKeyword' { AlexTokenTag AlexRawToken_ClassKeyword _ _ }
'ConstKeyword' { AlexTokenTag AlexRawToken_ConstKeyword _ _ }
'ContinueKeyword' { AlexTokenTag AlexRawToken_ContinueKeyword _ _ }
'DebuggerKeyword' { AlexTokenTag AlexRawToken_DebuggerKeyword _ _ }
'DefaultKeyword' { AlexTokenTag AlexRawToken_DefaultKeyword _ _ }
'DeleteKeyword' { AlexTokenTag AlexRawToken_DeleteKeyword _ _ }
'DoKeyword' { AlexTokenTag AlexRawToken_DoKeyword _ _ }
'ElseKeyword' { AlexTokenTag AlexRawToken_ElseKeyword _ _ }
'EnumKeyword' { AlexTokenTag AlexRawToken_EnumKeyword _ _ }
'ExportKeyword' { AlexTokenTag AlexRawToken_ExportKeyword _ _ }
'ExtendsKeyword' { AlexTokenTag AlexRawToken_ExtendsKeyword _ _ }
'FalseKeyword' { AlexTokenTag AlexRawToken_FalseKeyword _ _ }
'FinallyKeyword' { AlexTokenTag AlexRawToken_FinallyKeyword _ _ }
'ForKeyword' { AlexTokenTag AlexRawToken_ForKeyword _ _ }
'FunctionKeyword' { AlexTokenTag AlexRawToken_FunctionKeyword _ _ }
'IfKeyword' { AlexTokenTag AlexRawToken_IfKeyword _ _ }
'ImportKeyword' { AlexTokenTag AlexRawToken_ImportKeyword _ _ }
'InKeyword' { AlexTokenTag AlexRawToken_InKeyword _ _ }
'InstanceOfKeyword' { AlexTokenTag AlexRawToken_InstanceOfKeyword _ _ }
'NewKeyword' { AlexTokenTag AlexRawToken_NewKeyword _ _ }
'NullKeyword' { AlexTokenTag AlexRawToken_NullKeyword _ _ }
'ReturnKeyword' { AlexTokenTag AlexRawToken_ReturnKeyword _ _ }
'SuperKeyword' { AlexTokenTag AlexRawToken_SuperKeyword _ _ }
'SwitchKeyword' { AlexTokenTag AlexRawToken_SwitchKeyword _ _ }
'ThisKeyword' { AlexTokenTag AlexRawToken_ThisKeyword _ _ }
'ThrowKeyword' { AlexTokenTag AlexRawToken_ThrowKeyword _ _ }
'TrueKeyword' { AlexTokenTag AlexRawToken_TrueKeyword _ _ }
'TryKeyword' { AlexTokenTag AlexRawToken_TryKeyword _ _ }
'TypeOfKeyword' { AlexTokenTag AlexRawToken_TypeOfKeyword _ _ }
'VarKeyword' { AlexTokenTag AlexRawToken_VarKeyword _ _ }
'VoidKeyword' { AlexTokenTag AlexRawToken_VoidKeyword _ _ }
'WhileKeyword' { AlexTokenTag AlexRawToken_WhileKeyword _ _ }
'WithKeyword' { AlexTokenTag AlexRawToken_WithKeyword _ _ }
'ImplementsKeyword' { AlexTokenTag AlexRawToken_ImplementsKeyword _ _ }
'InterfaceKeyword' { AlexTokenTag AlexRawToken_InterfaceKeyword _ _ }
'LetKeyword' { AlexTokenTag AlexRawToken_LetKeyword _ _ }
'PackageKeyword' { AlexTokenTag AlexRawToken_PackageKeyword _ _ }
'PrivateKeyword' { AlexTokenTag AlexRawToken_PrivateKeyword _ _ }
'ProtectedKeyword' { AlexTokenTag AlexRawToken_ProtectedKeyword _ _ }
'PublicKeyword' { AlexTokenTag AlexRawToken_PublicKeyword _ _ }
'StaticKeyword' { AlexTokenTag AlexRawToken_StaticKeyword _ _ }
'YieldKeyword' { AlexTokenTag AlexRawToken_YieldKeyword _ _ }
'AbstractKeyword' { AlexTokenTag AlexRawToken_AbstractKeyword _ _ }
'AccessorKeyword' { AlexTokenTag AlexRawToken_AccessorKeyword _ _ }
'AsKeyword' { AlexTokenTag AlexRawToken_AsKeyword _ _ }
'AssertsKeyword' { AlexTokenTag AlexRawToken_AssertsKeyword _ _ }
'AssertKeyword' { AlexTokenTag AlexRawToken_AssertKeyword _ _ }
'AnyKeyword' { AlexTokenTag AlexRawToken_AnyKeyword _ _ }
'AsyncKeyword' { AlexTokenTag AlexRawToken_AsyncKeyword _ _ }
'AwaitKeyword' { AlexTokenTag AlexRawToken_AwaitKeyword _ _ }
'BooleanKeyword' { AlexTokenTag AlexRawToken_BooleanKeyword _ _ }
'ConstructorKeyword' { AlexTokenTag AlexRawToken_ConstructorKeyword _ _ }
'DeclareKeyword' { AlexTokenTag AlexRawToken_DeclareKeyword _ _ }
'GetKeyword' { AlexTokenTag AlexRawToken_GetKeyword _ _ }
'InferKeyword' { AlexTokenTag AlexRawToken_InferKeyword _ _ }
'IntrinsicKeyword' { AlexTokenTag AlexRawToken_IntrinsicKeyword _ _ }
'IsKeyword' { AlexTokenTag AlexRawToken_IsKeyword _ _ }
'KeyOfKeyword' { AlexTokenTag AlexRawToken_KeyOfKeyword _ _ }
'ModuleKeyword' { AlexTokenTag AlexRawToken_ModuleKeyword _ _ }
'NamespaceKeyword' { AlexTokenTag AlexRawToken_NamespaceKeyword _ _ }
'NeverKeyword' { AlexTokenTag AlexRawToken_NeverKeyword _ _ }
'OutKeyword' { AlexTokenTag AlexRawToken_OutKeyword _ _ }
'ReadonlyKeyword' { AlexTokenTag AlexRawToken_ReadonlyKeyword _ _ }
'RequireKeyword' { AlexTokenTag AlexRawToken_RequireKeyword _ _ }
'NumberKeyword' { AlexTokenTag AlexRawToken_NumberKeyword _ _ }
'ObjectKeyword' { AlexTokenTag AlexRawToken_ObjectKeyword _ _ }
'SatisfiesKeyword' { AlexTokenTag AlexRawToken_SatisfiesKeyword _ _ }
'SetKeyword' { AlexTokenTag AlexRawToken_SetKeyword _ _ }
'StringKeyword' { AlexTokenTag AlexRawToken_StringKeyword _ _ }
'SymbolKeyword' { AlexTokenTag AlexRawToken_SymbolKeyword _ _ }
'TypeKeyword' { AlexTokenTag AlexRawToken_TypeKeyword _ _ }
'UndefinedKeyword' { AlexTokenTag AlexRawToken_UndefinedKeyword _ _ }
'UniqueKeyword' { AlexTokenTag AlexRawToken_UniqueKeyword _ _ }
'UnknownKeyword' { AlexTokenTag AlexRawToken_UnknownKeyword _ _ }
'UsingKeyword' { AlexTokenTag AlexRawToken_UsingKeyword _ _ }
'FromKeyword' { AlexTokenTag AlexRawToken_FromKeyword _ _ }
'GlobalKeyword' { AlexTokenTag AlexRawToken_GlobalKeyword _ _ }
'BigIntKeyword' { AlexTokenTag AlexRawToken_BigIntKeyword _ _ }
'OverrideKeyword' { AlexTokenTag AlexRawToken_OverrideKeyword _ _ }
'OfKeyword' { AlexTokenTag AlexRawToken_OfKeyword _ _ }
'QualifiedName' { AlexTokenTag AlexRawToken_QualifiedName _ _ }
'ComputedPropertyName' { AlexTokenTag AlexRawToken_ComputedPropertyName _ _ }
'TypeParameter' { AlexTokenTag AlexRawToken_TypeParameter _ _ }
'Parameter' { AlexTokenTag AlexRawToken_Parameter _ _ }
'Decorator' { AlexTokenTag AlexRawToken_Decorator _ _ }
'PropertySignature' { AlexTokenTag AlexRawToken_PropertySignature _ _ }
'PropertyDeclaration' { AlexTokenTag AlexRawToken_PropertyDeclaration _ _ }
'MethodSignature' { AlexTokenTag AlexRawToken_MethodSignature _ _ }
'MethodDeclaration' { AlexTokenTag AlexRawToken_MethodDeclaration _ _ }
'ClassStaticBlockDeclaration' { AlexTokenTag AlexRawToken_ClassStaticBlockDeclaration _ _ }
'Constructor' { AlexTokenTag AlexRawToken_Constructor _ _ }
'GetAccessor' { AlexTokenTag AlexRawToken_GetAccessor _ _ }
'SetAccessor' { AlexTokenTag AlexRawToken_SetAccessor _ _ }
'CallSignature' { AlexTokenTag AlexRawToken_CallSignature _ _ }
'ConstructSignature' { AlexTokenTag AlexRawToken_ConstructSignature _ _ }
'IndexSignature' { AlexTokenTag AlexRawToken_IndexSignature _ _ }
'TypePredicate' { AlexTokenTag AlexRawToken_TypePredicate _ _ }
'TypeReference' { AlexTokenTag AlexRawToken_TypeReference _ _ }
'FunctionType' { AlexTokenTag AlexRawToken_FunctionType _ _ }
'ConstructorType' { AlexTokenTag AlexRawToken_ConstructorType _ _ }
'TypeQuery' { AlexTokenTag AlexRawToken_TypeQuery _ _ }
'TypeLiteral' { AlexTokenTag AlexRawToken_TypeLiteral _ _ }
'ArrayType' { AlexTokenTag AlexRawToken_ArrayType _ _ }
'TupleType' { AlexTokenTag AlexRawToken_TupleType _ _ }
'OptionalType' { AlexTokenTag AlexRawToken_OptionalType _ _ }
'RestType' { AlexTokenTag AlexRawToken_RestType _ _ }
'UnionType' { AlexTokenTag AlexRawToken_UnionType _ _ }
'IntersectionType' { AlexTokenTag AlexRawToken_IntersectionType _ _ }
'ConditionalType' { AlexTokenTag AlexRawToken_ConditionalType _ _ }
'InferType' { AlexTokenTag AlexRawToken_InferType _ _ }
'ParenthesizedType' { AlexTokenTag AlexRawToken_ParenthesizedType _ _ }
'ThisType' { AlexTokenTag AlexRawToken_ThisType _ _ }
'TypeOperator' { AlexTokenTag AlexRawToken_TypeOperator _ _ }
'IndexedAccessType' { AlexTokenTag AlexRawToken_IndexedAccessType _ _ }
'MappedType' { AlexTokenTag AlexRawToken_MappedType _ _ }
'LiteralType' { AlexTokenTag AlexRawToken_LiteralType _ _ }
'NamedTupleMember' { AlexTokenTag AlexRawToken_NamedTupleMember _ _ }
'TemplateLiteralType' { AlexTokenTag AlexRawToken_TemplateLiteralType _ _ }
'TemplateLiteralTypeSpan' { AlexTokenTag AlexRawToken_TemplateLiteralTypeSpan _ _ }
'ImportType' { AlexTokenTag AlexRawToken_ImportType _ _ }
'ObjectBindingPattern' { AlexTokenTag AlexRawToken_ObjectBindingPattern _ _ }
'ArrayBindingPattern' { AlexTokenTag AlexRawToken_ArrayBindingPattern _ _ }
'BindingElement' { AlexTokenTag AlexRawToken_BindingElement _ _ }
'ArrayLiteralExpression' { AlexTokenTag AlexRawToken_ArrayLiteralExpression _ _ }
'ObjectLiteralExpression' { AlexTokenTag AlexRawToken_ObjectLiteralExpression _ _ }
'PropertyAccessExpression' { AlexTokenTag AlexRawToken_PropertyAccessExpression _ _ }
'ElementAccessExpression' { AlexTokenTag AlexRawToken_ElementAccessExpression _ _ }
'CallExpression' { AlexTokenTag AlexRawToken_CallExpression _ _ }
'NewExpression' { AlexTokenTag AlexRawToken_NewExpression _ _ }
'TaggedTemplateExpression' { AlexTokenTag AlexRawToken_TaggedTemplateExpression _ _ }
'TypeAssertionExpression' { AlexTokenTag AlexRawToken_TypeAssertionExpression _ _ }
'ParenthesizedExpression' { AlexTokenTag AlexRawToken_ParenthesizedExpression _ _ }
'FunctionExpression' { AlexTokenTag AlexRawToken_FunctionExpression _ _ }
'ArrowFunction' { AlexTokenTag AlexRawToken_ArrowFunction _ _ }
'DeleteExpression' { AlexTokenTag AlexRawToken_DeleteExpression _ _ }
'TypeOfExpression' { AlexTokenTag AlexRawToken_TypeOfExpression _ _ }
'VoidExpression' { AlexTokenTag AlexRawToken_VoidExpression _ _ }
'AwaitExpression' { AlexTokenTag AlexRawToken_AwaitExpression _ _ }
'PrefixUnaryExpression' { AlexTokenTag AlexRawToken_PrefixUnaryExpression _ _ }
'PostfixUnaryExpression' { AlexTokenTag AlexRawToken_PostfixUnaryExpression _ _ }
'BinaryExpression' { AlexTokenTag AlexRawToken_BinaryExpression _ _ }
'ConditionalExpression' { AlexTokenTag AlexRawToken_ConditionalExpression _ _ }
'TemplateExpression' { AlexTokenTag AlexRawToken_TemplateExpression _ _ }
'YieldExpression' { AlexTokenTag AlexRawToken_YieldExpression _ _ }
'SpreadElement' { AlexTokenTag AlexRawToken_SpreadElement _ _ }
'ClassExpression' { AlexTokenTag AlexRawToken_ClassExpression _ _ }
'OmittedExpression' { AlexTokenTag AlexRawToken_OmittedExpression _ _ }
'ExpressionWithTypeArguments' { AlexTokenTag AlexRawToken_ExpressionWithTypeArguments _ _ }
'AsExpression' { AlexTokenTag AlexRawToken_AsExpression _ _ }
'NonNullExpression' { AlexTokenTag AlexRawToken_NonNullExpression _ _ }
'MetaProperty' { AlexTokenTag AlexRawToken_MetaProperty _ _ }
'SyntheticExpression' { AlexTokenTag AlexRawToken_SyntheticExpression _ _ }
'SatisfiesExpression' { AlexTokenTag AlexRawToken_SatisfiesExpression _ _ }
'TemplateSpan' { AlexTokenTag AlexRawToken_TemplateSpan _ _ }
'SemicolonClassElement' { AlexTokenTag AlexRawToken_SemicolonClassElement _ _ }
'Block' { AlexTokenTag AlexRawToken_Block _ _ }
'EmptyStatement' { AlexTokenTag AlexRawToken_EmptyStatement _ _ }
'VariableStatement' { AlexTokenTag AlexRawToken_VariableStatement _ _ }
'ExpressionStatement' { AlexTokenTag AlexRawToken_ExpressionStatement _ _ }
'IfStatement' { AlexTokenTag AlexRawToken_IfStatement _ _ }
'DoStatement' { AlexTokenTag AlexRawToken_DoStatement _ _ }
'WhileStatement' { AlexTokenTag AlexRawToken_WhileStatement _ _ }
'ForStatement' { AlexTokenTag AlexRawToken_ForStatement _ _ }
'ForInStatement' { AlexTokenTag AlexRawToken_ForInStatement _ _ }
'ForOfStatement' { AlexTokenTag AlexRawToken_ForOfStatement _ _ }
'ContinueStatement' { AlexTokenTag AlexRawToken_ContinueStatement _ _ }
'BreakStatement' { AlexTokenTag AlexRawToken_BreakStatement _ _ }
'ReturnStatement' { AlexTokenTag AlexRawToken_ReturnStatement _ _ }
'WithStatement' { AlexTokenTag AlexRawToken_WithStatement _ _ }
'SwitchStatement' { AlexTokenTag AlexRawToken_SwitchStatement _ _ }
'LabeledStatement' { AlexTokenTag AlexRawToken_LabeledStatement _ _ }
'ThrowStatement' { AlexTokenTag AlexRawToken_ThrowStatement _ _ }
'TryStatement' { AlexTokenTag AlexRawToken_TryStatement _ _ }
'DebuggerStatement' { AlexTokenTag AlexRawToken_DebuggerStatement _ _ }
'VariableDeclaration' { AlexTokenTag AlexRawToken_VariableDeclaration _ _ }
'VariableDeclarationList' { AlexTokenTag AlexRawToken_VariableDeclarationList _ _ }
'FunctionDeclaration' { AlexTokenTag AlexRawToken_FunctionDeclaration _ _ }
'ClassDeclaration' { AlexTokenTag AlexRawToken_ClassDeclaration _ _ }
'InterfaceDeclaration' { AlexTokenTag AlexRawToken_InterfaceDeclaration _ _ }
'TypeAliasDeclaration' { AlexTokenTag AlexRawToken_TypeAliasDeclaration _ _ }
'EnumDeclaration' { AlexTokenTag AlexRawToken_EnumDeclaration _ _ }
'ModuleDeclaration' { AlexTokenTag AlexRawToken_ModuleDeclaration _ _ }
'ModuleBlock' { AlexTokenTag AlexRawToken_ModuleBlock _ _ }
'CaseBlock' { AlexTokenTag AlexRawToken_CaseBlock _ _ }
'NamespaceExportDeclaration' { AlexTokenTag AlexRawToken_NamespaceExportDeclaration _ _ }
'ImportEqualsDeclaration' { AlexTokenTag AlexRawToken_ImportEqualsDeclaration _ _ }
'ImportDeclaration' { AlexTokenTag AlexRawToken_ImportDeclaration _ _ }
'ImportClause' { AlexTokenTag AlexRawToken_ImportClause _ _ }
'NamespaceImport' { AlexTokenTag AlexRawToken_NamespaceImport _ _ }
'NamedImports' { AlexTokenTag AlexRawToken_NamedImports _ _ }
'ImportSpecifier' { AlexTokenTag AlexRawToken_ImportSpecifier _ _ }
'ExportAssignment' { AlexTokenTag AlexRawToken_ExportAssignment _ _ }
'ExportDeclaration' { AlexTokenTag AlexRawToken_ExportDeclaration _ _ }
'NamedExports' { AlexTokenTag AlexRawToken_NamedExports _ _ }
'NamespaceExport' { AlexTokenTag AlexRawToken_NamespaceExport _ _ }
'ExportSpecifier' { AlexTokenTag AlexRawToken_ExportSpecifier _ _ }
'MissingDeclaration' { AlexTokenTag AlexRawToken_MissingDeclaration _ _ }
'ExternalModuleReference' { AlexTokenTag AlexRawToken_ExternalModuleReference _ _ }
'JsxElement' { AlexTokenTag AlexRawToken_JsxElement _ _ }
'JsxSelfClosingElement' { AlexTokenTag AlexRawToken_JsxSelfClosingElement _ _ }
'JsxOpeningElement' { AlexTokenTag AlexRawToken_JsxOpeningElement _ _ }
'JsxClosingElement' { AlexTokenTag AlexRawToken_JsxClosingElement _ _ }
'JsxFragment' { AlexTokenTag AlexRawToken_JsxFragment _ _ }
'JsxOpeningFragment' { AlexTokenTag AlexRawToken_JsxOpeningFragment _ _ }
'JsxClosingFragment' { AlexTokenTag AlexRawToken_JsxClosingFragment _ _ }
'JsxAttribute' { AlexTokenTag AlexRawToken_JsxAttribute _ _ }
'JsxAttributes' { AlexTokenTag AlexRawToken_JsxAttributes _ _ }
'JsxSpreadAttribute' { AlexTokenTag AlexRawToken_JsxSpreadAttribute _ _ }
'JsxExpression' { AlexTokenTag AlexRawToken_JsxExpression _ _ }
'JsxNamespacedName' { AlexTokenTag AlexRawToken_JsxNamespacedName _ _ }
'CaseClause' { AlexTokenTag AlexRawToken_CaseClause _ _ }
'DefaultClause' { AlexTokenTag AlexRawToken_DefaultClause _ _ }
'HeritageClause' { AlexTokenTag AlexRawToken_HeritageClause _ _ }
'CatchClause' { AlexTokenTag AlexRawToken_CatchClause _ _ }
'ImportAttributes' { AlexTokenTag AlexRawToken_ImportAttributes _ _ }
'ImportAttribute' { AlexTokenTag AlexRawToken_ImportAttribute _ _ }
'AssertClause' { AlexTokenTag AlexRawToken_AssertClause _ _ }
'AssertEntry' { AlexTokenTag AlexRawToken_AssertEntry _ _ }
'ImportTypeAssertionContainer' { AlexTokenTag AlexRawToken_ImportTypeAssertionContainer _ _ }
'PropertyAssignment' { AlexTokenTag AlexRawToken_PropertyAssignment _ _ }
'ShorthandPropertyAssignment' { AlexTokenTag AlexRawToken_ShorthandPropertyAssignment _ _ }
'SpreadAssignment' { AlexTokenTag AlexRawToken_SpreadAssignment _ _ }
'EnumMember' { AlexTokenTag AlexRawToken_EnumMember _ _ }
'SourceFile' { AlexTokenTag AlexRawToken_SourceFile _ _ }
'Bundle' { AlexTokenTag AlexRawToken_Bundle _ _ }
'JSDocTypeExpression' { AlexTokenTag AlexRawToken_JSDocTypeExpression _ _ }
'JSDocNameReference' { AlexTokenTag AlexRawToken_JSDocNameReference _ _ }
'JSDocMemberName' { AlexTokenTag AlexRawToken_JSDocMemberName _ _ }
'JSDocAllType' { AlexTokenTag AlexRawToken_JSDocAllType _ _ }
'JSDocUnknownType' { AlexTokenTag AlexRawToken_JSDocUnknownType _ _ }
'JSDocNullableType' { AlexTokenTag AlexRawToken_JSDocNullableType _ _ }
'JSDocNonNullableType' { AlexTokenTag AlexRawToken_JSDocNonNullableType _ _ }
'JSDocOptionalType' { AlexTokenTag AlexRawToken_JSDocOptionalType _ _ }
'JSDocFunctionType' { AlexTokenTag AlexRawToken_JSDocFunctionType _ _ }
'JSDocVariadicType' { AlexTokenTag AlexRawToken_JSDocVariadicType _ _ }
'JSDocNamepathType' { AlexTokenTag AlexRawToken_JSDocNamepathType _ _ }
'JSDoc' { AlexTokenTag AlexRawToken_JSDoc _ _ }
'JSDocComment' { AlexTokenTag AlexRawToken_JSDocComment _ _ }
'JSDocText' { AlexTokenTag AlexRawToken_JSDocText _ _ }
'JSDocTypeLiteral' { AlexTokenTag AlexRawToken_JSDocTypeLiteral _ _ }
'JSDocSignature' { AlexTokenTag AlexRawToken_JSDocSignature _ _ }
'JSDocLink' { AlexTokenTag AlexRawToken_JSDocLink _ _ }
'JSDocLinkCode' { AlexTokenTag AlexRawToken_JSDocLinkCode _ _ }
'JSDocLinkPlain' { AlexTokenTag AlexRawToken_JSDocLinkPlain _ _ }
'JSDocTag' { AlexTokenTag AlexRawToken_JSDocTag _ _ }
'JSDocAugmentsTag' { AlexTokenTag AlexRawToken_JSDocAugmentsTag _ _ }
'JSDocImplementsTag' { AlexTokenTag AlexRawToken_JSDocImplementsTag _ _ }
'JSDocAuthorTag' { AlexTokenTag AlexRawToken_JSDocAuthorTag _ _ }
'JSDocDeprecatedTag' { AlexTokenTag AlexRawToken_JSDocDeprecatedTag _ _ }
'JSDocClassTag' { AlexTokenTag AlexRawToken_JSDocClassTag _ _ }
'JSDocPublicTag' { AlexTokenTag AlexRawToken_JSDocPublicTag _ _ }
'JSDocPrivateTag' { AlexTokenTag AlexRawToken_JSDocPrivateTag _ _ }
'JSDocProtectedTag' { AlexTokenTag AlexRawToken_JSDocProtectedTag _ _ }
'JSDocReadonlyTag' { AlexTokenTag AlexRawToken_JSDocReadonlyTag _ _ }
'JSDocOverrideTag' { AlexTokenTag AlexRawToken_JSDocOverrideTag _ _ }
'JSDocCallbackTag' { AlexTokenTag AlexRawToken_JSDocCallbackTag _ _ }
'JSDocOverloadTag' { AlexTokenTag AlexRawToken_JSDocOverloadTag _ _ }
'JSDocEnumTag' { AlexTokenTag AlexRawToken_JSDocEnumTag _ _ }
'JSDocParameterTag' { AlexTokenTag AlexRawToken_JSDocParameterTag _ _ }
'JSDocReturnTag' { AlexTokenTag AlexRawToken_JSDocReturnTag _ _ }
'JSDocThisTag' { AlexTokenTag AlexRawToken_JSDocThisTag _ _ }
'JSDocTypeTag' { AlexTokenTag AlexRawToken_JSDocTypeTag _ _ }
'JSDocTemplateTag' { AlexTokenTag AlexRawToken_JSDocTemplateTag _ _ }
'JSDocTypedefTag' { AlexTokenTag AlexRawToken_JSDocTypedefTag _ _ }
'JSDocSeeTag' { AlexTokenTag AlexRawToken_JSDocSeeTag _ _ }
'JSDocPropertyTag' { AlexTokenTag AlexRawToken_JSDocPropertyTag _ _ }
'JSDocThrowsTag' { AlexTokenTag AlexRawToken_JSDocThrowsTag _ _ }
'JSDocSatisfiesTag' { AlexTokenTag AlexRawToken_JSDocSatisfiesTag _ _ }
'JSDocImportTag' { AlexTokenTag AlexRawToken_JSDocImportTag _ _ }
'SyntaxList' { AlexTokenTag AlexRawToken_SyntaxList _ _ }
'NotEmittedStatement' { AlexTokenTag AlexRawToken_NotEmittedStatement _ _ }
'PartiallyEmittedExpression' { AlexTokenTag AlexRawToken_PartiallyEmittedExpression _ _ }
'CommaListExpression' { AlexTokenTag AlexRawToken_CommaListExpression _ _ }
'SyntheticReferenceExpression' { AlexTokenTag AlexRawToken_SyntheticReferenceExpression _ _ }
'Count' { AlexTokenTag AlexRawToken_Count _ _ }
'FirstAssignment' { AlexTokenTag AlexRawToken_FirstAssignment _ _ }
'LastAssignment' { AlexTokenTag AlexRawToken_LastAssignment _ _ }
'FirstCompoundAssignment' { AlexTokenTag AlexRawToken_FirstCompoundAssignment _ _ }
'LastCompoundAssignment' { AlexTokenTag AlexRawToken_LastCompoundAssignment _ _ }
'FirstReservedWord' { AlexTokenTag AlexRawToken_FirstReservedWord _ _ }
'LastReservedWord' { AlexTokenTag AlexRawToken_LastReservedWord _ _ }
'FirstKeyword' { AlexTokenTag AlexRawToken_FirstKeyword _ _ }
'LastKeyword' { AlexTokenTag AlexRawToken_LastKeyword _ _ }
'FirstFutureReservedWord' { AlexTokenTag AlexRawToken_FirstFutureReservedWord _ _ }
'LastFutureReservedWord' { AlexTokenTag AlexRawToken_LastFutureReservedWord _ _ }
'FirstTypeNode' { AlexTokenTag AlexRawToken_FirstTypeNode _ _ }
'LastTypeNode' { AlexTokenTag AlexRawToken_LastTypeNode _ _ }
'FirstPunctuation' { AlexTokenTag AlexRawToken_FirstPunctuation _ _ }
'LastPunctuation' { AlexTokenTag AlexRawToken_LastPunctuation _ _ }
'FirstToken' { AlexTokenTag AlexRawToken_FirstToken _ _ }
'LastToken' { AlexTokenTag AlexRawToken_LastToken _ _ }
'FirstTriviaToken' { AlexTokenTag AlexRawToken_FirstTriviaToken _ _ }
'LastTriviaToken' { AlexTokenTag AlexRawToken_LastTriviaToken _ _ }
'FirstLiteralToken' { AlexTokenTag AlexRawToken_FirstLiteralToken _ _ }
'LastLiteralToken' { AlexTokenTag AlexRawToken_LastLiteralToken _ _ }
'FirstTemplateToken' { AlexTokenTag AlexRawToken_FirstTemplateToken _ _ }
'LastTemplateToken' { AlexTokenTag AlexRawToken_LastTemplateToken _ _ }
'FirstBinaryOperator' { AlexTokenTag AlexRawToken_FirstBinaryOperator _ _ }
'LastBinaryOperator' { AlexTokenTag AlexRawToken_LastBinaryOperator _ _ }
'FirstStatement' { AlexTokenTag AlexRawToken_FirstStatement _ _ }
'LastStatement' { AlexTokenTag AlexRawToken_LastStatement _ _ }
'FirstNode' { AlexTokenTag AlexRawToken_FirstNode _ _ }
'FirstJSDocNode' { AlexTokenTag AlexRawToken_FirstJSDocNode _ _ }
'LastJSDocNode' { AlexTokenTag AlexRawToken_LastJSDocNode _ _ }
'FirstJSDocTagNode' { AlexTokenTag AlexRawToken_FirstJSDocTagNode _ _ }
'LastJSDocTagNode' { AlexTokenTag AlexRawToken_LastJSDocTagNode _ _ }
-- reserved keywords end

-- ****************************
-- *                          *
-- * integers and identifiers *
-- *                          *
-- ****************************

INT { AlexTokenTag (AlexRawToken_INT  i) _ _ }
STR { AlexTokenTag (AlexRawToken_STR  s) _ _ }
ID  { AlexTokenTag (AlexRawToken_ID  id) _ _ }

-- *************************
-- *                       *
-- * grammar specification *
-- *                       *
-- *************************
%%

-- **********************
-- *                    *
-- * parametrized lists *
-- *                    *
-- **********************
listof(a):          a { [$1] } | a listof(a)                                       { $1:$2 }
barlistof(a):       a { [$1] } | a ',' 'BarToken'   loc '(' ')' ',' barlistof(a)   { $1:$8 }
ampersandlistof(a): a { [$1] } | a 'AmpersandToken' loc '(' ')' ampersandlistof(a) { $1:$6 }

-- **********************
-- *                    *
-- * parametrized lists *
-- *                    *
-- **********************
commalistof(a): a { [$1] } | a ',' commalistof(a) { $1:$3 }
possibly_empty_commalistof(a): { [] } | commalistof(a) { $1 }

-- ********************************************************
-- *                                                      *
-- * parametrized list with optional trailing comma       *
-- *                                                      *
-- ********************************************************
possibly_empty_commalistof_with_optional_trailing_comma(a): { [] } | commalistof_with_optional_trailing_comma(a) { $1 }
commalistof_with_optional_trailing_comma(a): a commalistof_with_optional_trailing_comma_rest(a) { $1:$2 }
commalistof_with_optional_trailing_comma_rest(a): ',' a commalistof_with_optional_trailing_comma_rest(a) { $2:$3 } | ',' { [] } | { [] }

-- ******************
-- *                *
-- * optional rules *
-- *                *
-- ******************
optional(a): { Nothing } | a { Just $1 }

-- direct translation to dhscanner Ast.Root
program:
commalistof(stmt) { Actions.root $1 } |
',' commalistof(stmt) { Actions.root $2 }

stmt:
stmtIf { $1 } |
stmtExp { $1 } |
stmtTry { $1 } |
stmtFunc { $1 } |
stmtImport { $1 } |
stmt_property    { $1 } |
stmtClass        { $1 } |
stmtReturn       { $1 } |
stmtThrow        { $1 } |
stmtDecvar       { $1 }

-- direct translation to dhscanner Ast.StmtIf
stmtIf:
'IfStatement' loc
'('
    ifKeyword
    openParenToken
    exp
    closeParenToken
    stmtOrBlock
    optional(elsePart)
')'
{
    Actions.stmtIf $2 $6 $8 $9
}

-- helpers related to stmtIf
elsePart: elseKeyword stmtOrBlock { $2 }
stmtOrBlock: stmt { [$1] } | block { $1 }

-- direct translation to dhscanner Ast.StmtExp
stmtExp: 'ExpressionStatement' loc '(' expOrStmtAssign ')' { $4 }

-- helpers related to stmtExp
expOrStmtAssign: exp { Ast.StmtExp $1 } | stmtAssign { $1 }

-- direct translation to dhscanner Ast.StmtTry
stmtTry:
'TryStatement' loc
'('
    tryKeyword
    block
    catchPart
')'
{
    Actions.stmtTry $2 $5 $6
}

-- helpers related to stmtTry
catchPart:
'CatchClause' loc
'('
    catchKeyword
    openParenToken
    'VariableDeclaration' loc '(' identifier optional(type_hint) ')'
    closeParenToken
    block
')'
{
    $13
}

-- instrumented as dhscanner Ast.StmtExp
stmtThrow: 'ThrowStatement' loc '(' throwKeyword exp ')' { Actions.stmtThrow $2 $5 }

-- direct translation to dhscanner Ast.StmtClass
stmtClass:
'InterfaceDeclaration' loc
'('
    interfaceKeyword
    identifier
    optional(extends)
    commalistof(stmt)
')'
{
    Actions.stmtClass $5
}

stmtFunc:
'FunctionDeclaration' loc
'('
    functionKeyword
    identifier
    openParenToken
    parameters
    closeParenToken
    optional(type_hint)
    optional(block)
')'
{
    Actions.stmtFunc $2 $5 $7 $10
}

-- helpers related to stmtFunc
identifier:
'Identifier' loc
'('
    ID
')'
{
    Token.Named
    {
        Token.content = tokIDValue $4,
        Token.location = $2
    }
}

parameters:
possibly_empty_commalistof(parameterChunk) { concat $1 }

parameterChunk:
parameterChunk1 { $1 } |
parameterChunk2 { $1 } |
parameterChunk3 { $1 }

parameterChunk1:
'Parameter' loc
'('
    identifier
    optional(questionToken)
    optional(type_hint)
    optional(default_value)
')'
{
    Actions.parameterChunk1 $4 $6
}

parameterChunk2:
'Parameter' loc
'('
    objectBindingPattern
')'
{
    []
}

parameterChunk3:
'Parameter' loc
'('
    objectBindingPattern
    colonToken
    'TypeLiteral' loc
    '('
        commalistof(property_signature_as_param)
    ')'
')'
{
    $9
}

-- helpers related to stmtFunc
property_signature_as_param:
'PropertySignature' loc
'('
    identifier
    type_hint
')'
{
    Ast.Param
    {
        Ast.paramName = Token.ParamName $4,
        Ast.paramNominalType = case $5 of { Just t -> Just (Actions.varify t); _ -> Nothing },
        Ast.paramSerialIdx = 156
    }
}

-- helpers related to stmtFunc
type_hint: colonToken type { $2 }

-- helpers related to stmtFunc
type:
expressionWithTypeArguments { Nothing } |
indexedAccessType { Nothing } |
union_type { Nothing } |
intersection_type { Nothing } |
parenthesized_type { Nothing } |
type_operator { Nothing } |
array_type { Nothing } |
internal_type optional(generics) { $1 }

-- helpers related to type
expressionWithTypeArguments: 'ExpressionWithTypeArguments' loc '(' type ')' { $4 }

-- helpers related to type
indexedAccessType:
'IndexedAccessType' loc
'('
    type
    openBracketToken
    internal_type
    closeBracketToken
')'
{
    $4
}

array_type:
'ArrayType' loc
'('
    type
    openBracketToken
    closeBracketToken
')'
{
    Nothing
}

-- helpers related to type
union_type: 'UnionType' loc '(' barlistof(type) ')' { Nothing }
intersection_type: 'IntersectionType' loc '(' ampersandlistof(type) ')' { Nothing }
parenthesized_type: 'ParenthesizedType' loc '(' openParenToken type closeParenToken ')' { $5 }
type_operator: 'TypeOperator' loc '(' type ')' { $4 }

internal_type:
booleanKeyword { Nothing } |
anyKeyword { Nothing } |
unknownKeyword { Nothing } |
undefinedKeyword { Nothing } |
stringKeyword { Nothing } |
numberKeyword { Nothing } |
voidKeyword { Nothing } |
identifier { Just $1 } |
typeReference { $1 } |
literalType { Nothing } |
typeLiteral { Nothing }

-- helpers related to type
generics: firstBinaryOperator commalistof(type) greaterThanToken { Nothing }
typeReference: 'TypeReference' loc '(' type ')' { $4 }
typeLiteral: 'TypeLiteral' loc '(' optional(commalistof(stmt_property)) ')' { Nothing }

literalType:
'LiteralType' loc '(' stringLiteral ')' { Nothing } |
'LiteralType' loc '(' nullKeyword   ')' { Nothing }


throwKeyword:        'ThrowKeyword'        loc '(' ')' { Nothing }
importKeyword:       'ImportKeyword'       loc '(' ')' { Nothing }
interfaceKeyword:    'InterfaceKeyword'    loc '(' ')' { Nothing }
instanceOfKeyword:   'InstanceOfKeyword'   loc '(' ')' { Nothing }
nullKeyword:         'NullKeyword'         loc '(' ')' { $2 }
trueKeyword:         'TrueKeyword'         loc '(' ')' { $2 }
falseKeyword:        'FalseKeyword'        loc '(' ')' { $2 }
ifKeyword:           'IfKeyword'           loc '(' ')' { Nothing }
functionKeyword:     'FunctionKeyword'     loc '(' ')' { Nothing }
inKeyword:           'InKeyword'           loc '(' ')' { Nothing }
anyKeyword:          'AnyKeyword'          loc '(' ')' { Nothing }
commaToken:          'CommaToken'          loc '(' ')' { Nothing }
booleanKeyword:      'BooleanKeyword'      loc '(' ')' { Nothing }
newKeyword:          'NewKeyword'          loc '(' ')' { Nothing }
unknownKeyword:      'UnknownKeyword'      loc '(' ')' { Nothing }
deleteKeyword:       'DeleteKeyword'       loc '(' ')' { Nothing }
typeOfKeyword:       'TypeOfKeyword'       loc '(' ')' { Nothing }
returnKeyword:       'ReturnKeyword'       loc '(' ')' { Nothing }
slashToken:          'SlashToken'          loc '(' ')' { Nothing }
exclamationToken:    'ExclamationToken'    loc '(' ')' { Nothing }
undefinedKeyword:    'UndefinedKeyword'    loc '(' ')' { Nothing }
templateHead:        'TemplateHead'        loc '(' ')' { Nothing }
templateMiddle:      'TemplateMiddle'      loc '(' ')' { Nothing }
lastTemplateToken:   'LastTemplateToken'   loc '(' ')' { Nothing }
dotToken:            'DotToken'            loc '(' ')' { Nothing }
questionDotToken:    'QuestionDotToken'    loc '(' ')' { Nothing }
barBarToken:         'BarBarToken'         loc '(' ')' { Nothing }
stringKeyword:       'StringKeyword'       loc '(' ')' { Nothing }
numberKeyword:       'NumberKeyword'       loc '(' ')' { Nothing }
voidKeyword:         'VoidKeyword'         loc '(' ')' { Nothing }
awaitKeyword:        'AwaitKeyword'        loc '(' ')' { Nothing }
fromKeyword:         'FromKeyword'         loc '(' ')' { Nothing }
extendsKeyword:      'ExtendsKeyword'      loc '(' ')' { Nothing }
questionToken:       'QuestionToken'       loc '(' ')' { Nothing }
openParenToken:      'OpenParenToken'      loc '(' ')' { Nothing }
openBracketToken:    'OpenBracketToken'    loc '(' ')' { Nothing }
closeBracketToken:   'CloseBracketToken'   loc '(' ')' { Nothing }
closeParenToken:     'CloseParenToken'     loc '(' ')' { Nothing }
asKeyword:           'AsKeyword'           loc '(' ')' { Nothing }
asteriskToken:       'AsteriskToken'       loc '(' ')' { Nothing }
plusToken:           'PlusToken'           loc '(' ')' { Nothing }
colonToken:          'ColonToken'          loc '(' ')' { Nothing }
tryKeyword:          'TryKeyword'          loc '(' ')' { Nothing }
elseKeyword:         'ElseKeyword'         loc '(' ')' { Nothing }
minusToken:          'MinusToken'          loc '(' ')' { Nothing }
catchKeyword:        'CatchKeyword'        loc '(' ')' { Nothing }
firstAssignment:     'FirstAssignment'     loc '(' ')' { Nothing }
firstBinaryOperator: 'FirstBinaryOperator' loc '(' ')' { Nothing }
greaterThanToken:    'GreaterThanToken'    loc '(' ')' { Nothing }
greaterThanEqualsToken: 'GreaterThanEqualsToken' loc '(' ')' { Nothing }
questionQuestionToken: 'QuestionQuestionToken' loc '(' ')' { Nothing }
equalsGreaterThanToken: 'EqualsGreaterThanToken' loc '(' ')' { Nothing }
ampAmpToken:         'AmpersandAmpersandToken' loc '(' ')' { Nothing }
eqEqEqToken:         'EqualsEqualsEqualsToken' loc '(' ')' { Nothing }
exclamationEqEqToken: 'ExclamationEqualsEqualsToken' loc '(' ')' { Nothing }
dotDotDotToken: 'DotDotDotToken' loc '(' ')' { Nothing }

importee: asteriskToken { Nothing }

importSpecifier:
'ImportSpecifier' loc
'('
    identifier optional(alias)
')'
{
    case $5 of { Just a -> a; _ -> $4 }
}


namedImports:
'NamedImports' loc
'('
    commalistof(importSpecifier)
')'
{
    $4
}

importClauseStuff_1:
namedImports
{
    $1
}

importClauseStuff_2:
identifier { [$1] }

importClauseStuff:
importClauseStuff_1 { $1 } |
importClauseStuff_2 { $1 }

importClause:
'ImportClause' loc
'('
    listof(importClauseStuff)
')'
{
    concat $4
}

alias: asKeyword identifier { $2 }

namespaceImport:
'NamespaceImport' loc
'('
    importee
    optional(alias)
')'
{
    Nothing
}

stringLiteral:
'StringLiteral' loc '(' STR ')'
{
    Token.ConstStr
    {
        Token.constStrValue = unquote (tokSTRValue $4),
        Token.constStrLocation = $2
    }
}

stmtImport:
'ImportDeclaration' loc
'('
    importKeyword
    optional(importClause)
    optional(fromKeyword)
    stringLiteral
')'
{
    Actions.stmtImport (getAdditionalRepoInfo $1) $2 $5 $7
}

bindingElement:
'BindingElement' loc
'('
    identifier
')'
{
    Actions.varify $4
}

objectBindingPattern:
'ObjectBindingPattern' loc
'('
    commalistof(bindingElement)
')'
{
    $4
}

decvarLhs:
identifier optional(type_hint) { [Actions.varify $1] } |
objectBindingPattern           { $1 }

stmtDecvar:
'VariableDeclarationList' loc
'('
    'VariableDeclaration' loc '(' decvarLhs firstAssignment exp ')'
')'
{
    Actions.stmtDecvar $2 $7 $9
}

extends:
'HeritageClause' loc
'('
    extendsKeyword
    commalistof(type)
')'
{
    Nothing
}

-- ***************
-- *             *
-- * data member *
-- *             *
-- ***************
stmt_property:
'PropertySignature' loc
'('
    identifier optional(questionToken) optional(type_hint) optional(commaToken)
')'
{
    Ast.StmtVardec $ Ast.StmtVardecContent
    {
        Ast.stmtVardecName = Token.VarName $4,
        Ast.stmtVardecNominalType = Just (Actions.varify $4),
        Ast.stmtVardecInitValue = Nothing,
        Ast.stmtVardecLocation = $2
    }
}

block:
'Block' loc
'('
    commalistof(stmt)
')'
{
    $4
}

lambdaBody:
block { $1 } |
exp { [ Ast.StmtExp $1 ] }

default_value:
firstAssignment exp
{
    $2
}

stmtReturn:
'ReturnStatement' loc
'('
    returnKeyword
    optional(exp)
')'
{
    Actions.stmtReturn $2 $5
}

expArrowFunction:
'ArrowFunction' loc
'('
    openParenToken
    parameters
    closeParenToken
    optional(type_hint)
    equalsGreaterThanToken
    lambdaBody
')'
{
    Actions.expArrowFunction $2 $5 $9
}

expCall:
'CallExpression' loc
'('
    exp
    optional(generics)
    openParenToken
    possibly_empty_commalistof(exp)
    closeParenToken
')'
{
    Actions.expCall $2 $4 $7
}
|
'CallExpression' loc
'('
    importKeyword
    openParenToken
    possibly_empty_commalistof(exp)
    closeParenToken
')'
{
    Actions.instrumentationCall "import" $2 $6
}

-- ***********
-- *         *
-- * exp str *
-- *         *
-- ***********
exp_str:
stringLiteral
{
    Ast.ExpStr $ Ast.ExpStrContent $1
}

operator:
inKeyword            { Nothing } |
firstBinaryOperator  { Nothing } |
instanceOfKeyword    { Nothing } |
barBarToken          { Nothing } |
eqEqEqToken          { Nothing } |
ampAmpToken          { Nothing } |
asteriskToken        { Nothing } |
plusToken            { Nothing } |
slashToken           { Nothing } |
greaterThanToken     { Nothing } |
greaterThanEqualsToken { Nothing } |
questionQuestionToken { Nothing } |
exclamationEqEqToken { Nothing }

-- *************
-- *           *
-- * exp binop *
-- *           *
-- *************
expBinop:
'BinaryExpression' loc
'('
    exp
    operator
    exp
')'
{
    Actions.expBinop $2 $4 $6
}

-- ***************
-- *             *
-- * stmt assign *
-- *             *
-- ***************
stmtAssign:
'BinaryExpression' loc
'('
    var
    firstAssignment
    exp
')'
{
    Actions.stmtAssign $4 $6
}

varField:
'PropertyAccessExpression' loc
'('
    exp
    dotToken
    identifier
')'
{
    Actions.varField $2 $4 $6
}
|
'PropertyAccessExpression' loc
'('
    exp
    questionDotToken
    identifier
')'
{
    Actions.varField $2 $4 $6
}

varSubscript:
'ElementAccessExpression' loc
'('
    exp
    openBracketToken
    exp
    closeBracketToken
')'
{
    Actions.varSubscript $2 $4 $6
}
|
'ElementAccessExpression' loc
'('
    exp
    questionDotToken
    openBracketToken
    exp
    closeBracketToken
')'
{
    Actions.varSubscript $2 $4 $7
}

var_simple:
identifier
{
    Actions.varify $1
}

var:
var_simple   { $1 } |
varField     { $1 } |
varSubscript { $1 }

-- ***********
-- *         *
-- * exp var *
-- *         *
-- ***********
exp_var:
var
{
    Ast.ExpVar $ Ast.ExpVarContent $1
}

-- ***********
-- *         *
-- * exp var *
-- *         *
-- ***********
exp_meta:
'MetaProperty' loc
'('
    'ImportKeyword' loc '(' ')' dotToken identifier
')'
{
    Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
    {
        Token.content = "meta",
        Token.location = $2
    }
}

template_span:
'TemplateSpan' loc '(' exp templateMiddle    ')' { $4 } |
'TemplateSpan' loc '(' exp lastTemplateToken ')' { $4 }

-- ***********
-- *         *
-- * fstring *
-- *         *
-- ***********
fstring:
'TemplateExpression' loc
'('
    templateHead
    commalistof(template_span)
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "fstring",
            Token.location = $2
        },
        Ast.args = $5,
        Ast.expCallLocation = $2
    }
}

unary_operator:
exclamationToken { Nothing } |
minusToken { Nothing }

-- ************
-- *          *
-- * exp unop *
-- *          *
-- ************
exp_unop:
'PrefixUnaryExpression' loc
'('
    unary_operator
    exp
')'
{
    $5
}

-- *************
-- *           *
-- * exp paren *
-- *           *
-- *************
exp_paren:
'ParenthesizedExpression' loc
'('
    openParenToken
    exp
    closeParenToken
')'
{
    $5
}

-- **************
-- *            *
-- * exp typeof *
-- *            *
-- **************
exp_typeof:
'TypeOfExpression' loc
'('
    typeOfKeyword
    exp
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "typeof",
            Token.location = $2
        },
        Ast.args = [],
        Ast.expCallLocation = $2
    }
}

-- ***************
-- *             *
-- * exp ternary *
-- *             *
-- ***************
exp_ternary:
'ConditionalExpression' loc
'('
    exp
    questionToken
    exp
    colonToken
    exp
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "ternary",
            Token.location = $2
        },
        Ast.args = [],
        Ast.expCallLocation = $2
    }
}

stmt_method:
'MethodDeclaration' loc
'('
    identifier
    openParenToken
    parameters
    closeParenToken
    block
')'
{
    Ast.StmtMethodContent
    {
        Ast.stmtMethodReturnType = Just (Actions.varify (Token.Named "any" $2)),
        Ast.stmtMethodName = Token.MethodName $4,
        Ast.stmtMethodParams = $6,
        Ast.stmtMethodBody = $8,
        Ast.stmtMethodLocation = $2,
        Ast.hostingClassName = Token.ClassName (Token.Named "host" $2),
        Ast.hostingClassSupers = []
    }
}

shorthandElement:
identifier
{
    Ast.StmtMethodContent
    {
        Ast.stmtMethodReturnType = Just (Actions.varify $1),
        Ast.stmtMethodName = Token.MethodName $1,
        Ast.stmtMethodParams = [],
        Ast.stmtMethodBody = [],
        Ast.stmtMethodLocation = Token.location $1,
        Ast.hostingClassName = Token.ClassName $1,
        Ast.hostingClassSupers = []
    } 
}

shorthandPropertyAssignment_1:
'ShorthandPropertyAssignment' loc
'('
    shorthandElement
')'
{
    $4
}

shorthandPropertyAssignment_2:
stmt_method { $1 }


shorthandPropertyAssignment:
shorthandPropertyAssignment_1 { $1 } |
shorthandPropertyAssignment_2 { $1 }

-- instrumented as dhscanner Ast.ExpCall
expDelete:
'DeleteExpression' loc
'('
    deleteKeyword
    exp
')'
{
    Actions.expDelete $2 $5
}

-- **********
-- *        *
-- * exp as *
-- *        *
-- **********
exp_as:
'AsExpression' loc
'('
    exp
    asKeyword
    type
')'
{
    $4
}

expTrue: trueKeyword { Actions.expBool True $1 }
expFalse: falseKeyword { Actions.expBool False $1 }
expBool: expTrue  { $1 } | expFalse { $1 }

expNull: nullKeyword { Actions.expNull $1 }

-- instrumented as dhscanner Ast.ExpCall
expNew:
'NewExpression' loc
'('
    newKeyword
    type
    openParenToken
    optional(commalistof(exp))
    closeParenToken
')'
{
    Actions.expNew $2 $5 $7
}

-- *************
-- *           *
-- * exp regex *
-- *           *
-- *************
exp_regex:
'RegularExpressionLiteral' loc '(' ')'
{
    Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt
    {
        Token.constIntValue = 888,
        Token.constIntLocation = $2
    }
}

exp_int:
'FirstLiteralToken' loc '(' ')'
{
    Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt 888 $2
}

exp_jsx:
'JsxElement' loc
'('
    'JsxOpeningElement' loc
    '('
        firstBinaryOperator
        identifier
        greaterThanToken
    ')'
    'JsxExpression' loc
    '('
        exp
    ')'
    'JsxClosingElement' loc
    '('
        firstBinaryOperator
        slashToken
        identifier
        greaterThanToken
    ')'
')'
{
    $14
}

-- *************
-- *           *
-- * exp await *
-- *           *
-- *************
exp_await:
'AwaitExpression' loc
'('
    awaitKeyword
    exp
')'
{
    $5
}

-- instrumented as dhscanner Ast.ExpCall
property:
property_1 { $1 } |
property_2 { $1 }

property_1:
'PropertyAssignment' loc
'('
    exp
    colonToken
    exp
')'
{
    Actions.property $2 $4 $6
}

property_2:
'PropertyAssignment' loc
'('
    'ComputedPropertyName' loc
    '('
        openBracketToken
        exp
        closeBracketToken
    ')'
    colonToken
    exp
')'
{
    Actions.property $2 $8 $12
}

spread_exp:
'SpreadAssignment' loc
'('
    dotDotDotToken
    exp
')'
{
    $5
}


property_assignment:
property { $1 } |
shorthandPropertyAssignment { lambdame' $1 } |
spread_exp { $1 }

-- ************
-- *          *
-- * exp dict *
-- *          *
-- ************
exp_dict:
'ObjectLiteralExpression' loc
'('
    possibly_empty_commalistof(property_assignment)
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named "dictify" $2,
        Ast.args = $4,
        Ast.expCallLocation = $2
    }
}

-- *************
-- *           *
-- * exp array *
-- *           *
-- *************
exp_array:
'ArrayLiteralExpression' loc
'('
    openBracketToken
    possibly_empty_commalistof_with_optional_trailing_comma(exp)
    closeBracketToken
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named "arrayify" $2,
        Ast.args = $5,
        Ast.expCallLocation = $2
    }
}

exp_non_null:
'NonNullExpression' loc
'('
    exp
    exclamationToken
')'
{
    $4
}

exp:
exp_str        { $1 } |
exp_int        { $1 } |
expNew         { $1 } |
exp_dict       { $1 } |
exp_await      { $1 } |
expBool        { $1 } |
expNull        { $1 } |
fstring        { $1 } |
expCall        { $1 } |
exp_meta       { $1 } |
exp_array      { $1 } |
exp_ternary    { $1 } |
exp_var        { $1 } |
exp_as         { $1 } |
exp_paren      { $1 } |
exp_unop       { $1 } |
expDelete      { $1 } |
exp_typeof     { $1 } |
expBinop       { $1 } |
exp_regex      { $1 } |
exp_non_null   { $1 } |
exp_jsx        { $1 } |
expArrowFunction { $1 }

loc:
'[' INT ':' INT '-' INT ':' INT ']'
{
    Location
    {
        Location.filename = getFilename $1,
        lineStart = fromIntegral (tokIntValue $2),
        colStart = fromIntegral (tokIntValue $4),
        lineEnd = fromIntegral (tokIntValue $6),
        colEnd = fromIntegral (tokIntValue $8)
    }
}

{

unquote :: String -> String
unquote s = let n = length s in take (n-2) (drop 1 s)

lambdame' :: Ast.StmtMethodContent -> Ast.Exp
lambdame' m = let
    p = Token.ParamName (Token.getMethodNameToken (Ast.stmtMethodName m))
    in Ast.ExpLambda $ Ast.ExpLambdaContent {
        Ast.expLambdaParams = [(Ast.Param p Nothing 174)] ++ (Ast.stmtMethodParams m),
        Ast.expLambdaBody = Ast.stmtMethodBody m,
        Ast.expLambdaLocation = Ast.stmtMethodLocation m
    }

lambdame :: [ Ast.StmtMethodContent ] -> [ Ast.Exp ]
lambdame = Data.List.map lambdame'

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
parseProgram :: Common.SourceCodeFilePath -> Common.SourceCodeContent -> Common.AdditionalRepoInfo -> Either String Ast.Root
parseProgram (Common.SourceCodeFilePath fp) (Common.SourceCodeContent content) additionalInfo = runAlex' parse fp additionalInfo content
}
