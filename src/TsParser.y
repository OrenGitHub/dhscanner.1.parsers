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
import qualified Token

-- *******************
-- *                 *
-- * general imports *
-- *                 *
-- *******************
import Data.Maybe
import Data.Either
import Data.List ( map )
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

-- ************
-- *          *
-- * location *
-- *          *
-- ************

':'    { AlexTokenTag AlexRawToken_COLON _ }
','    { AlexTokenTag AlexRawToken_COMMA _ }
'-'    { AlexTokenTag AlexRawToken_MINUS _ }

-- *********************
-- *                   *
-- * reserved keywords *
-- *                   *
-- *********************

'Unknown' { AlexTokenTag AlexRawToken_Unknown _ }
'EndOfFileToken' { AlexTokenTag AlexRawToken_EndOfFileToken _ }
'SingleLineCommentTrivia' { AlexTokenTag AlexRawToken_SingleLineCommentTrivia _ }
'MultiLineCommentTrivia' { AlexTokenTag AlexRawToken_MultiLineCommentTrivia _ }
'NewLineTrivia' { AlexTokenTag AlexRawToken_NewLineTrivia _ }
'WhitespaceTrivia' { AlexTokenTag AlexRawToken_WhitespaceTrivia _ }
'ShebangTrivia' { AlexTokenTag AlexRawToken_ShebangTrivia _ }
'ConflictMarkerTrivia' { AlexTokenTag AlexRawToken_ConflictMarkerTrivia _ }
'NonTextFileMarkerTrivia' { AlexTokenTag AlexRawToken_NonTextFileMarkerTrivia _ }
'NumericLiteral' { AlexTokenTag AlexRawToken_NumericLiteral _ }
'BigIntLiteral' { AlexTokenTag AlexRawToken_BigIntLiteral _ }
'StringLiteral' { AlexTokenTag AlexRawToken_StringLiteral _ }
'JsxText' { AlexTokenTag AlexRawToken_JsxText _ }
'JsxTextAllWhiteSpaces' { AlexTokenTag AlexRawToken_JsxTextAllWhiteSpaces _ }
'RegularExpressionLiteral' { AlexTokenTag AlexRawToken_RegularExpressionLiteral _ }
'NoSubstitutionTemplateLiteral' { AlexTokenTag AlexRawToken_NoSubstitutionTemplateLiteral _ }
'TemplateHead' { AlexTokenTag AlexRawToken_TemplateHead _ }
'TemplateMiddle' { AlexTokenTag AlexRawToken_TemplateMiddle _ }
'TemplateTail' { AlexTokenTag AlexRawToken_TemplateTail _ }
'OpenBraceToken' { AlexTokenTag AlexRawToken_OpenBraceToken _ }
'CloseBraceToken' { AlexTokenTag AlexRawToken_CloseBraceToken _ }
'OpenParenToken' { AlexTokenTag AlexRawToken_OpenParenToken _ }
'CloseParenToken' { AlexTokenTag AlexRawToken_CloseParenToken _ }
'OpenBracketToken' { AlexTokenTag AlexRawToken_OpenBracketToken _ }
'CloseBracketToken' { AlexTokenTag AlexRawToken_CloseBracketToken _ }
'DotToken' { AlexTokenTag AlexRawToken_DotToken _ }
'DotDotDotToken' { AlexTokenTag AlexRawToken_DotDotDotToken _ }
'SemicolonToken' { AlexTokenTag AlexRawToken_SemicolonToken _ }
'CommaToken' { AlexTokenTag AlexRawToken_CommaToken _ }
'QuestionDotToken' { AlexTokenTag AlexRawToken_QuestionDotToken _ }
'LessThanToken' { AlexTokenTag AlexRawToken_LessThanToken _ }
'LessThanSlashToken' { AlexTokenTag AlexRawToken_LessThanSlashToken _ }
'GreaterThanToken' { AlexTokenTag AlexRawToken_GreaterThanToken _ }
'LessThanEqualsToken' { AlexTokenTag AlexRawToken_LessThanEqualsToken _ }
'GreaterThanEqualsToken' { AlexTokenTag AlexRawToken_GreaterThanEqualsToken _ }
'EqualsEqualsToken' { AlexTokenTag AlexRawToken_EqualsEqualsToken _ }
'ExclamationEqualsToken' { AlexTokenTag AlexRawToken_ExclamationEqualsToken _ }
'EqualsEqualsEqualsToken' { AlexTokenTag AlexRawToken_EqualsEqualsEqualsToken _ }
'ExclamationEqualsEqualsToken' { AlexTokenTag AlexRawToken_ExclamationEqualsEqualsToken _ }
'EqualsGreaterThanToken' { AlexTokenTag AlexRawToken_EqualsGreaterThanToken _ }
'PlusToken' { AlexTokenTag AlexRawToken_PlusToken _ }
'MinusToken' { AlexTokenTag AlexRawToken_MinusToken _ }
'AsteriskToken' { AlexTokenTag AlexRawToken_AsteriskToken _ }
'AsteriskAsteriskToken' { AlexTokenTag AlexRawToken_AsteriskAsteriskToken _ }
'SlashToken' { AlexTokenTag AlexRawToken_SlashToken _ }
'PercentToken' { AlexTokenTag AlexRawToken_PercentToken _ }
'PlusPlusToken' { AlexTokenTag AlexRawToken_PlusPlusToken _ }
'MinusMinusToken' { AlexTokenTag AlexRawToken_MinusMinusToken _ }
'LessThanLessThanToken' { AlexTokenTag AlexRawToken_LessThanLessThanToken _ }
'GreaterThanGreaterThanToken' { AlexTokenTag AlexRawToken_GreaterThanGreaterThanToken _ }
'GreaterThanGreaterThanGreaterThanToken' { AlexTokenTag AlexRawToken_GreaterThanGreaterThanGreaterThanToken _ }
'AmpersandToken' { AlexTokenTag AlexRawToken_AmpersandToken _ }
'BarToken' { AlexTokenTag AlexRawToken_BarToken _ }
'CaretToken' { AlexTokenTag AlexRawToken_CaretToken _ }
'ExclamationToken' { AlexTokenTag AlexRawToken_ExclamationToken _ }
'TildeToken' { AlexTokenTag AlexRawToken_TildeToken _ }
'AmpersandAmpersandToken' { AlexTokenTag AlexRawToken_AmpersandAmpersandToken _ }
'BarBarToken' { AlexTokenTag AlexRawToken_BarBarToken _ }
'QuestionToken' { AlexTokenTag AlexRawToken_QuestionToken _ }
'ColonToken' { AlexTokenTag AlexRawToken_ColonToken _ }
'AtToken' { AlexTokenTag AlexRawToken_AtToken _ }
'QuestionQuestionToken' { AlexTokenTag AlexRawToken_QuestionQuestionToken _ }
'BacktickToken' { AlexTokenTag AlexRawToken_BacktickToken _ }
'HashToken' { AlexTokenTag AlexRawToken_HashToken _ }
'EqualsToken' { AlexTokenTag AlexRawToken_EqualsToken _ }
'PlusEqualsToken' { AlexTokenTag AlexRawToken_PlusEqualsToken _ }
'MinusEqualsToken' { AlexTokenTag AlexRawToken_MinusEqualsToken _ }
'AsteriskEqualsToken' { AlexTokenTag AlexRawToken_AsteriskEqualsToken _ }
'AsteriskAsteriskEqualsToken' { AlexTokenTag AlexRawToken_AsteriskAsteriskEqualsToken _ }
'SlashEqualsToken' { AlexTokenTag AlexRawToken_SlashEqualsToken _ }
'PercentEqualsToken' { AlexTokenTag AlexRawToken_PercentEqualsToken _ }
'LessThanLessThanEqualsToken' { AlexTokenTag AlexRawToken_LessThanLessThanEqualsToken _ }
'GreaterThanGreaterThanEqualsToken' { AlexTokenTag AlexRawToken_GreaterThanGreaterThanEqualsToken _ }
'GreaterThanGreaterThanGreaterThanEqualsToken' { AlexTokenTag AlexRawToken_GreaterThanGreaterThanGreaterThanEqualsToken _ }
'AmpersandEqualsToken' { AlexTokenTag AlexRawToken_AmpersandEqualsToken _ }
'BarEqualsToken' { AlexTokenTag AlexRawToken_BarEqualsToken _ }
'BarBarEqualsToken' { AlexTokenTag AlexRawToken_BarBarEqualsToken _ }
'AmpersandAmpersandEqualsToken' { AlexTokenTag AlexRawToken_AmpersandAmpersandEqualsToken _ }
'QuestionQuestionEqualsToken' { AlexTokenTag AlexRawToken_QuestionQuestionEqualsToken _ }
'CaretEqualsToken' { AlexTokenTag AlexRawToken_CaretEqualsToken _ }
'Identifier' { AlexTokenTag AlexRawToken_Identifier _ }
'PrivateIdentifier' { AlexTokenTag AlexRawToken_PrivateIdentifier _ }
'BreakKeyword' { AlexTokenTag AlexRawToken_BreakKeyword _ }
'CaseKeyword' { AlexTokenTag AlexRawToken_CaseKeyword _ }
'CatchKeyword' { AlexTokenTag AlexRawToken_CatchKeyword _ }
'ClassKeyword' { AlexTokenTag AlexRawToken_ClassKeyword _ }
'ConstKeyword' { AlexTokenTag AlexRawToken_ConstKeyword _ }
'ContinueKeyword' { AlexTokenTag AlexRawToken_ContinueKeyword _ }
'DebuggerKeyword' { AlexTokenTag AlexRawToken_DebuggerKeyword _ }
'DefaultKeyword' { AlexTokenTag AlexRawToken_DefaultKeyword _ }
'DeleteKeyword' { AlexTokenTag AlexRawToken_DeleteKeyword _ }
'DoKeyword' { AlexTokenTag AlexRawToken_DoKeyword _ }
'ElseKeyword' { AlexTokenTag AlexRawToken_ElseKeyword _ }
'EnumKeyword' { AlexTokenTag AlexRawToken_EnumKeyword _ }
'ExportKeyword' { AlexTokenTag AlexRawToken_ExportKeyword _ }
'ExtendsKeyword' { AlexTokenTag AlexRawToken_ExtendsKeyword _ }
'FalseKeyword' { AlexTokenTag AlexRawToken_FalseKeyword _ }
'FinallyKeyword' { AlexTokenTag AlexRawToken_FinallyKeyword _ }
'ForKeyword' { AlexTokenTag AlexRawToken_ForKeyword _ }
'FunctionKeyword' { AlexTokenTag AlexRawToken_FunctionKeyword _ }
'IfKeyword' { AlexTokenTag AlexRawToken_IfKeyword _ }
'ImportKeyword' { AlexTokenTag AlexRawToken_ImportKeyword _ }
'InKeyword' { AlexTokenTag AlexRawToken_InKeyword _ }
'InstanceOfKeyword' { AlexTokenTag AlexRawToken_InstanceOfKeyword _ }
'NewKeyword' { AlexTokenTag AlexRawToken_NewKeyword _ }
'NullKeyword' { AlexTokenTag AlexRawToken_NullKeyword _ }
'ReturnKeyword' { AlexTokenTag AlexRawToken_ReturnKeyword _ }
'SuperKeyword' { AlexTokenTag AlexRawToken_SuperKeyword _ }
'SwitchKeyword' { AlexTokenTag AlexRawToken_SwitchKeyword _ }
'ThisKeyword' { AlexTokenTag AlexRawToken_ThisKeyword _ }
'ThrowKeyword' { AlexTokenTag AlexRawToken_ThrowKeyword _ }
'TrueKeyword' { AlexTokenTag AlexRawToken_TrueKeyword _ }
'TryKeyword' { AlexTokenTag AlexRawToken_TryKeyword _ }
'TypeOfKeyword' { AlexTokenTag AlexRawToken_TypeOfKeyword _ }
'VarKeyword' { AlexTokenTag AlexRawToken_VarKeyword _ }
'VoidKeyword' { AlexTokenTag AlexRawToken_VoidKeyword _ }
'WhileKeyword' { AlexTokenTag AlexRawToken_WhileKeyword _ }
'WithKeyword' { AlexTokenTag AlexRawToken_WithKeyword _ }
'ImplementsKeyword' { AlexTokenTag AlexRawToken_ImplementsKeyword _ }
'InterfaceKeyword' { AlexTokenTag AlexRawToken_InterfaceKeyword _ }
'LetKeyword' { AlexTokenTag AlexRawToken_LetKeyword _ }
'PackageKeyword' { AlexTokenTag AlexRawToken_PackageKeyword _ }
'PrivateKeyword' { AlexTokenTag AlexRawToken_PrivateKeyword _ }
'ProtectedKeyword' { AlexTokenTag AlexRawToken_ProtectedKeyword _ }
'PublicKeyword' { AlexTokenTag AlexRawToken_PublicKeyword _ }
'StaticKeyword' { AlexTokenTag AlexRawToken_StaticKeyword _ }
'YieldKeyword' { AlexTokenTag AlexRawToken_YieldKeyword _ }
'AbstractKeyword' { AlexTokenTag AlexRawToken_AbstractKeyword _ }
'AccessorKeyword' { AlexTokenTag AlexRawToken_AccessorKeyword _ }
'AsKeyword' { AlexTokenTag AlexRawToken_AsKeyword _ }
'AssertsKeyword' { AlexTokenTag AlexRawToken_AssertsKeyword _ }
'AssertKeyword' { AlexTokenTag AlexRawToken_AssertKeyword _ }
'AnyKeyword' { AlexTokenTag AlexRawToken_AnyKeyword _ }
'AsyncKeyword' { AlexTokenTag AlexRawToken_AsyncKeyword _ }
'AwaitKeyword' { AlexTokenTag AlexRawToken_AwaitKeyword _ }
'BooleanKeyword' { AlexTokenTag AlexRawToken_BooleanKeyword _ }
'ConstructorKeyword' { AlexTokenTag AlexRawToken_ConstructorKeyword _ }
'DeclareKeyword' { AlexTokenTag AlexRawToken_DeclareKeyword _ }
'GetKeyword' { AlexTokenTag AlexRawToken_GetKeyword _ }
'InferKeyword' { AlexTokenTag AlexRawToken_InferKeyword _ }
'IntrinsicKeyword' { AlexTokenTag AlexRawToken_IntrinsicKeyword _ }
'IsKeyword' { AlexTokenTag AlexRawToken_IsKeyword _ }
'KeyOfKeyword' { AlexTokenTag AlexRawToken_KeyOfKeyword _ }
'ModuleKeyword' { AlexTokenTag AlexRawToken_ModuleKeyword _ }
'NamespaceKeyword' { AlexTokenTag AlexRawToken_NamespaceKeyword _ }
'NeverKeyword' { AlexTokenTag AlexRawToken_NeverKeyword _ }
'OutKeyword' { AlexTokenTag AlexRawToken_OutKeyword _ }
'ReadonlyKeyword' { AlexTokenTag AlexRawToken_ReadonlyKeyword _ }
'RequireKeyword' { AlexTokenTag AlexRawToken_RequireKeyword _ }
'NumberKeyword' { AlexTokenTag AlexRawToken_NumberKeyword _ }
'ObjectKeyword' { AlexTokenTag AlexRawToken_ObjectKeyword _ }
'SatisfiesKeyword' { AlexTokenTag AlexRawToken_SatisfiesKeyword _ }
'SetKeyword' { AlexTokenTag AlexRawToken_SetKeyword _ }
'StringKeyword' { AlexTokenTag AlexRawToken_StringKeyword _ }
'SymbolKeyword' { AlexTokenTag AlexRawToken_SymbolKeyword _ }
'TypeKeyword' { AlexTokenTag AlexRawToken_TypeKeyword _ }
'UndefinedKeyword' { AlexTokenTag AlexRawToken_UndefinedKeyword _ }
'UniqueKeyword' { AlexTokenTag AlexRawToken_UniqueKeyword _ }
'UnknownKeyword' { AlexTokenTag AlexRawToken_UnknownKeyword _ }
'UsingKeyword' { AlexTokenTag AlexRawToken_UsingKeyword _ }
'FromKeyword' { AlexTokenTag AlexRawToken_FromKeyword _ }
'GlobalKeyword' { AlexTokenTag AlexRawToken_GlobalKeyword _ }
'BigIntKeyword' { AlexTokenTag AlexRawToken_BigIntKeyword _ }
'OverrideKeyword' { AlexTokenTag AlexRawToken_OverrideKeyword _ }
'OfKeyword' { AlexTokenTag AlexRawToken_OfKeyword _ }
'QualifiedName' { AlexTokenTag AlexRawToken_QualifiedName _ }
'ComputedPropertyName' { AlexTokenTag AlexRawToken_ComputedPropertyName _ }
'TypeParameter' { AlexTokenTag AlexRawToken_TypeParameter _ }
'Parameter' { AlexTokenTag AlexRawToken_Parameter _ }
'Decorator' { AlexTokenTag AlexRawToken_Decorator _ }
'PropertySignature' { AlexTokenTag AlexRawToken_PropertySignature _ }
'PropertyDeclaration' { AlexTokenTag AlexRawToken_PropertyDeclaration _ }
'MethodSignature' { AlexTokenTag AlexRawToken_MethodSignature _ }
'MethodDeclaration' { AlexTokenTag AlexRawToken_MethodDeclaration _ }
'ClassStaticBlockDeclaration' { AlexTokenTag AlexRawToken_ClassStaticBlockDeclaration _ }
'Constructor' { AlexTokenTag AlexRawToken_Constructor _ }
'GetAccessor' { AlexTokenTag AlexRawToken_GetAccessor _ }
'SetAccessor' { AlexTokenTag AlexRawToken_SetAccessor _ }
'CallSignature' { AlexTokenTag AlexRawToken_CallSignature _ }
'ConstructSignature' { AlexTokenTag AlexRawToken_ConstructSignature _ }
'IndexSignature' { AlexTokenTag AlexRawToken_IndexSignature _ }
'TypePredicate' { AlexTokenTag AlexRawToken_TypePredicate _ }
'TypeReference' { AlexTokenTag AlexRawToken_TypeReference _ }
'FunctionType' { AlexTokenTag AlexRawToken_FunctionType _ }
'ConstructorType' { AlexTokenTag AlexRawToken_ConstructorType _ }
'TypeQuery' { AlexTokenTag AlexRawToken_TypeQuery _ }
'TypeLiteral' { AlexTokenTag AlexRawToken_TypeLiteral _ }
'ArrayType' { AlexTokenTag AlexRawToken_ArrayType _ }
'TupleType' { AlexTokenTag AlexRawToken_TupleType _ }
'OptionalType' { AlexTokenTag AlexRawToken_OptionalType _ }
'RestType' { AlexTokenTag AlexRawToken_RestType _ }
'UnionType' { AlexTokenTag AlexRawToken_UnionType _ }
'IntersectionType' { AlexTokenTag AlexRawToken_IntersectionType _ }
'ConditionalType' { AlexTokenTag AlexRawToken_ConditionalType _ }
'InferType' { AlexTokenTag AlexRawToken_InferType _ }
'ParenthesizedType' { AlexTokenTag AlexRawToken_ParenthesizedType _ }
'ThisType' { AlexTokenTag AlexRawToken_ThisType _ }
'TypeOperator' { AlexTokenTag AlexRawToken_TypeOperator _ }
'IndexedAccessType' { AlexTokenTag AlexRawToken_IndexedAccessType _ }
'MappedType' { AlexTokenTag AlexRawToken_MappedType _ }
'LiteralType' { AlexTokenTag AlexRawToken_LiteralType _ }
'NamedTupleMember' { AlexTokenTag AlexRawToken_NamedTupleMember _ }
'TemplateLiteralType' { AlexTokenTag AlexRawToken_TemplateLiteralType _ }
'TemplateLiteralTypeSpan' { AlexTokenTag AlexRawToken_TemplateLiteralTypeSpan _ }
'ImportType' { AlexTokenTag AlexRawToken_ImportType _ }
'ObjectBindingPattern' { AlexTokenTag AlexRawToken_ObjectBindingPattern _ }
'ArrayBindingPattern' { AlexTokenTag AlexRawToken_ArrayBindingPattern _ }
'BindingElement' { AlexTokenTag AlexRawToken_BindingElement _ }
'ArrayLiteralExpression' { AlexTokenTag AlexRawToken_ArrayLiteralExpression _ }
'ObjectLiteralExpression' { AlexTokenTag AlexRawToken_ObjectLiteralExpression _ }
'PropertyAccessExpression' { AlexTokenTag AlexRawToken_PropertyAccessExpression _ }
'ElementAccessExpression' { AlexTokenTag AlexRawToken_ElementAccessExpression _ }
'CallExpression' { AlexTokenTag AlexRawToken_CallExpression _ }
'NewExpression' { AlexTokenTag AlexRawToken_NewExpression _ }
'TaggedTemplateExpression' { AlexTokenTag AlexRawToken_TaggedTemplateExpression _ }
'TypeAssertionExpression' { AlexTokenTag AlexRawToken_TypeAssertionExpression _ }
'ParenthesizedExpression' { AlexTokenTag AlexRawToken_ParenthesizedExpression _ }
'FunctionExpression' { AlexTokenTag AlexRawToken_FunctionExpression _ }
'ArrowFunction' { AlexTokenTag AlexRawToken_ArrowFunction _ }
'DeleteExpression' { AlexTokenTag AlexRawToken_DeleteExpression _ }
'TypeOfExpression' { AlexTokenTag AlexRawToken_TypeOfExpression _ }
'VoidExpression' { AlexTokenTag AlexRawToken_VoidExpression _ }
'AwaitExpression' { AlexTokenTag AlexRawToken_AwaitExpression _ }
'PrefixUnaryExpression' { AlexTokenTag AlexRawToken_PrefixUnaryExpression _ }
'PostfixUnaryExpression' { AlexTokenTag AlexRawToken_PostfixUnaryExpression _ }
'BinaryExpression' { AlexTokenTag AlexRawToken_BinaryExpression _ }
'ConditionalExpression' { AlexTokenTag AlexRawToken_ConditionalExpression _ }
'TemplateExpression' { AlexTokenTag AlexRawToken_TemplateExpression _ }
'YieldExpression' { AlexTokenTag AlexRawToken_YieldExpression _ }
'SpreadElement' { AlexTokenTag AlexRawToken_SpreadElement _ }
'ClassExpression' { AlexTokenTag AlexRawToken_ClassExpression _ }
'OmittedExpression' { AlexTokenTag AlexRawToken_OmittedExpression _ }
'ExpressionWithTypeArguments' { AlexTokenTag AlexRawToken_ExpressionWithTypeArguments _ }
'AsExpression' { AlexTokenTag AlexRawToken_AsExpression _ }
'NonNullExpression' { AlexTokenTag AlexRawToken_NonNullExpression _ }
'MetaProperty' { AlexTokenTag AlexRawToken_MetaProperty _ }
'SyntheticExpression' { AlexTokenTag AlexRawToken_SyntheticExpression _ }
'SatisfiesExpression' { AlexTokenTag AlexRawToken_SatisfiesExpression _ }
'TemplateSpan' { AlexTokenTag AlexRawToken_TemplateSpan _ }
'SemicolonClassElement' { AlexTokenTag AlexRawToken_SemicolonClassElement _ }
'Block' { AlexTokenTag AlexRawToken_Block _ }
'EmptyStatement' { AlexTokenTag AlexRawToken_EmptyStatement _ }
'VariableStatement' { AlexTokenTag AlexRawToken_VariableStatement _ }
'ExpressionStatement' { AlexTokenTag AlexRawToken_ExpressionStatement _ }
'IfStatement' { AlexTokenTag AlexRawToken_IfStatement _ }
'DoStatement' { AlexTokenTag AlexRawToken_DoStatement _ }
'WhileStatement' { AlexTokenTag AlexRawToken_WhileStatement _ }
'ForStatement' { AlexTokenTag AlexRawToken_ForStatement _ }
'ForInStatement' { AlexTokenTag AlexRawToken_ForInStatement _ }
'ForOfStatement' { AlexTokenTag AlexRawToken_ForOfStatement _ }
'ContinueStatement' { AlexTokenTag AlexRawToken_ContinueStatement _ }
'BreakStatement' { AlexTokenTag AlexRawToken_BreakStatement _ }
'ReturnStatement' { AlexTokenTag AlexRawToken_ReturnStatement _ }
'WithStatement' { AlexTokenTag AlexRawToken_WithStatement _ }
'SwitchStatement' { AlexTokenTag AlexRawToken_SwitchStatement _ }
'LabeledStatement' { AlexTokenTag AlexRawToken_LabeledStatement _ }
'ThrowStatement' { AlexTokenTag AlexRawToken_ThrowStatement _ }
'TryStatement' { AlexTokenTag AlexRawToken_TryStatement _ }
'DebuggerStatement' { AlexTokenTag AlexRawToken_DebuggerStatement _ }
'VariableDeclaration' { AlexTokenTag AlexRawToken_VariableDeclaration _ }
'VariableDeclarationList' { AlexTokenTag AlexRawToken_VariableDeclarationList _ }
'FunctionDeclaration' { AlexTokenTag AlexRawToken_FunctionDeclaration _ }
'ClassDeclaration' { AlexTokenTag AlexRawToken_ClassDeclaration _ }
'InterfaceDeclaration' { AlexTokenTag AlexRawToken_InterfaceDeclaration _ }
'TypeAliasDeclaration' { AlexTokenTag AlexRawToken_TypeAliasDeclaration _ }
'EnumDeclaration' { AlexTokenTag AlexRawToken_EnumDeclaration _ }
'ModuleDeclaration' { AlexTokenTag AlexRawToken_ModuleDeclaration _ }
'ModuleBlock' { AlexTokenTag AlexRawToken_ModuleBlock _ }
'CaseBlock' { AlexTokenTag AlexRawToken_CaseBlock _ }
'NamespaceExportDeclaration' { AlexTokenTag AlexRawToken_NamespaceExportDeclaration _ }
'ImportEqualsDeclaration' { AlexTokenTag AlexRawToken_ImportEqualsDeclaration _ }
'ImportDeclaration' { AlexTokenTag AlexRawToken_ImportDeclaration _ }
'ImportClause' { AlexTokenTag AlexRawToken_ImportClause _ }
'NamespaceImport' { AlexTokenTag AlexRawToken_NamespaceImport _ }
'NamedImports' { AlexTokenTag AlexRawToken_NamedImports _ }
'ImportSpecifier' { AlexTokenTag AlexRawToken_ImportSpecifier _ }
'ExportAssignment' { AlexTokenTag AlexRawToken_ExportAssignment _ }
'ExportDeclaration' { AlexTokenTag AlexRawToken_ExportDeclaration _ }
'NamedExports' { AlexTokenTag AlexRawToken_NamedExports _ }
'NamespaceExport' { AlexTokenTag AlexRawToken_NamespaceExport _ }
'ExportSpecifier' { AlexTokenTag AlexRawToken_ExportSpecifier _ }
'MissingDeclaration' { AlexTokenTag AlexRawToken_MissingDeclaration _ }
'ExternalModuleReference' { AlexTokenTag AlexRawToken_ExternalModuleReference _ }
'JsxElement' { AlexTokenTag AlexRawToken_JsxElement _ }
'JsxSelfClosingElement' { AlexTokenTag AlexRawToken_JsxSelfClosingElement _ }
'JsxOpeningElement' { AlexTokenTag AlexRawToken_JsxOpeningElement _ }
'JsxClosingElement' { AlexTokenTag AlexRawToken_JsxClosingElement _ }
'JsxFragment' { AlexTokenTag AlexRawToken_JsxFragment _ }
'JsxOpeningFragment' { AlexTokenTag AlexRawToken_JsxOpeningFragment _ }
'JsxClosingFragment' { AlexTokenTag AlexRawToken_JsxClosingFragment _ }
'JsxAttribute' { AlexTokenTag AlexRawToken_JsxAttribute _ }
'JsxAttributes' { AlexTokenTag AlexRawToken_JsxAttributes _ }
'JsxSpreadAttribute' { AlexTokenTag AlexRawToken_JsxSpreadAttribute _ }
'JsxExpression' { AlexTokenTag AlexRawToken_JsxExpression _ }
'JsxNamespacedName' { AlexTokenTag AlexRawToken_JsxNamespacedName _ }
'CaseClause' { AlexTokenTag AlexRawToken_CaseClause _ }
'DefaultClause' { AlexTokenTag AlexRawToken_DefaultClause _ }
'HeritageClause' { AlexTokenTag AlexRawToken_HeritageClause _ }
'CatchClause' { AlexTokenTag AlexRawToken_CatchClause _ }
'ImportAttributes' { AlexTokenTag AlexRawToken_ImportAttributes _ }
'ImportAttribute' { AlexTokenTag AlexRawToken_ImportAttribute _ }
'AssertClause' { AlexTokenTag AlexRawToken_AssertClause _ }
'AssertEntry' { AlexTokenTag AlexRawToken_AssertEntry _ }
'ImportTypeAssertionContainer' { AlexTokenTag AlexRawToken_ImportTypeAssertionContainer _ }
'PropertyAssignment' { AlexTokenTag AlexRawToken_PropertyAssignment _ }
'ShorthandPropertyAssignment' { AlexTokenTag AlexRawToken_ShorthandPropertyAssignment _ }
'SpreadAssignment' { AlexTokenTag AlexRawToken_SpreadAssignment _ }
'EnumMember' { AlexTokenTag AlexRawToken_EnumMember _ }
'SourceFile' { AlexTokenTag AlexRawToken_SourceFile _ }
'Bundle' { AlexTokenTag AlexRawToken_Bundle _ }
'JSDocTypeExpression' { AlexTokenTag AlexRawToken_JSDocTypeExpression _ }
'JSDocNameReference' { AlexTokenTag AlexRawToken_JSDocNameReference _ }
'JSDocMemberName' { AlexTokenTag AlexRawToken_JSDocMemberName _ }
'JSDocAllType' { AlexTokenTag AlexRawToken_JSDocAllType _ }
'JSDocUnknownType' { AlexTokenTag AlexRawToken_JSDocUnknownType _ }
'JSDocNullableType' { AlexTokenTag AlexRawToken_JSDocNullableType _ }
'JSDocNonNullableType' { AlexTokenTag AlexRawToken_JSDocNonNullableType _ }
'JSDocOptionalType' { AlexTokenTag AlexRawToken_JSDocOptionalType _ }
'JSDocFunctionType' { AlexTokenTag AlexRawToken_JSDocFunctionType _ }
'JSDocVariadicType' { AlexTokenTag AlexRawToken_JSDocVariadicType _ }
'JSDocNamepathType' { AlexTokenTag AlexRawToken_JSDocNamepathType _ }
'JSDoc' { AlexTokenTag AlexRawToken_JSDoc _ }
'JSDocComment' { AlexTokenTag AlexRawToken_JSDocComment _ }
'JSDocText' { AlexTokenTag AlexRawToken_JSDocText _ }
'JSDocTypeLiteral' { AlexTokenTag AlexRawToken_JSDocTypeLiteral _ }
'JSDocSignature' { AlexTokenTag AlexRawToken_JSDocSignature _ }
'JSDocLink' { AlexTokenTag AlexRawToken_JSDocLink _ }
'JSDocLinkCode' { AlexTokenTag AlexRawToken_JSDocLinkCode _ }
'JSDocLinkPlain' { AlexTokenTag AlexRawToken_JSDocLinkPlain _ }
'JSDocTag' { AlexTokenTag AlexRawToken_JSDocTag _ }
'JSDocAugmentsTag' { AlexTokenTag AlexRawToken_JSDocAugmentsTag _ }
'JSDocImplementsTag' { AlexTokenTag AlexRawToken_JSDocImplementsTag _ }
'JSDocAuthorTag' { AlexTokenTag AlexRawToken_JSDocAuthorTag _ }
'JSDocDeprecatedTag' { AlexTokenTag AlexRawToken_JSDocDeprecatedTag _ }
'JSDocClassTag' { AlexTokenTag AlexRawToken_JSDocClassTag _ }
'JSDocPublicTag' { AlexTokenTag AlexRawToken_JSDocPublicTag _ }
'JSDocPrivateTag' { AlexTokenTag AlexRawToken_JSDocPrivateTag _ }
'JSDocProtectedTag' { AlexTokenTag AlexRawToken_JSDocProtectedTag _ }
'JSDocReadonlyTag' { AlexTokenTag AlexRawToken_JSDocReadonlyTag _ }
'JSDocOverrideTag' { AlexTokenTag AlexRawToken_JSDocOverrideTag _ }
'JSDocCallbackTag' { AlexTokenTag AlexRawToken_JSDocCallbackTag _ }
'JSDocOverloadTag' { AlexTokenTag AlexRawToken_JSDocOverloadTag _ }
'JSDocEnumTag' { AlexTokenTag AlexRawToken_JSDocEnumTag _ }
'JSDocParameterTag' { AlexTokenTag AlexRawToken_JSDocParameterTag _ }
'JSDocReturnTag' { AlexTokenTag AlexRawToken_JSDocReturnTag _ }
'JSDocThisTag' { AlexTokenTag AlexRawToken_JSDocThisTag _ }
'JSDocTypeTag' { AlexTokenTag AlexRawToken_JSDocTypeTag _ }
'JSDocTemplateTag' { AlexTokenTag AlexRawToken_JSDocTemplateTag _ }
'JSDocTypedefTag' { AlexTokenTag AlexRawToken_JSDocTypedefTag _ }
'JSDocSeeTag' { AlexTokenTag AlexRawToken_JSDocSeeTag _ }
'JSDocPropertyTag' { AlexTokenTag AlexRawToken_JSDocPropertyTag _ }
'JSDocThrowsTag' { AlexTokenTag AlexRawToken_JSDocThrowsTag _ }
'JSDocSatisfiesTag' { AlexTokenTag AlexRawToken_JSDocSatisfiesTag _ }
'JSDocImportTag' { AlexTokenTag AlexRawToken_JSDocImportTag _ }
'SyntaxList' { AlexTokenTag AlexRawToken_SyntaxList _ }
'NotEmittedStatement' { AlexTokenTag AlexRawToken_NotEmittedStatement _ }
'PartiallyEmittedExpression' { AlexTokenTag AlexRawToken_PartiallyEmittedExpression _ }
'CommaListExpression' { AlexTokenTag AlexRawToken_CommaListExpression _ }
'SyntheticReferenceExpression' { AlexTokenTag AlexRawToken_SyntheticReferenceExpression _ }
'Count' { AlexTokenTag AlexRawToken_Count _ }
'FirstAssignment' { AlexTokenTag AlexRawToken_FirstAssignment _ }
'LastAssignment' { AlexTokenTag AlexRawToken_LastAssignment _ }
'FirstCompoundAssignment' { AlexTokenTag AlexRawToken_FirstCompoundAssignment _ }
'LastCompoundAssignment' { AlexTokenTag AlexRawToken_LastCompoundAssignment _ }
'FirstReservedWord' { AlexTokenTag AlexRawToken_FirstReservedWord _ }
'LastReservedWord' { AlexTokenTag AlexRawToken_LastReservedWord _ }
'FirstKeyword' { AlexTokenTag AlexRawToken_FirstKeyword _ }
'LastKeyword' { AlexTokenTag AlexRawToken_LastKeyword _ }
'FirstFutureReservedWord' { AlexTokenTag AlexRawToken_FirstFutureReservedWord _ }
'LastFutureReservedWord' { AlexTokenTag AlexRawToken_LastFutureReservedWord _ }
'FirstTypeNode' { AlexTokenTag AlexRawToken_FirstTypeNode _ }
'LastTypeNode' { AlexTokenTag AlexRawToken_LastTypeNode _ }
'FirstPunctuation' { AlexTokenTag AlexRawToken_FirstPunctuation _ }
'LastPunctuation' { AlexTokenTag AlexRawToken_LastPunctuation _ }
'FirstToken' { AlexTokenTag AlexRawToken_FirstToken _ }
'LastToken' { AlexTokenTag AlexRawToken_LastToken _ }
'FirstTriviaToken' { AlexTokenTag AlexRawToken_FirstTriviaToken _ }
'LastTriviaToken' { AlexTokenTag AlexRawToken_LastTriviaToken _ }
'FirstLiteralToken' { AlexTokenTag AlexRawToken_FirstLiteralToken _ }
'LastLiteralToken' { AlexTokenTag AlexRawToken_LastLiteralToken _ }
'FirstTemplateToken' { AlexTokenTag AlexRawToken_FirstTemplateToken _ }
'LastTemplateToken' { AlexTokenTag AlexRawToken_LastTemplateToken _ }
'FirstBinaryOperator' { AlexTokenTag AlexRawToken_FirstBinaryOperator _ }
'LastBinaryOperator' { AlexTokenTag AlexRawToken_LastBinaryOperator _ }
'FirstStatement' { AlexTokenTag AlexRawToken_FirstStatement _ }
'LastStatement' { AlexTokenTag AlexRawToken_LastStatement _ }
'FirstNode' { AlexTokenTag AlexRawToken_FirstNode _ }
'FirstJSDocNode' { AlexTokenTag AlexRawToken_FirstJSDocNode _ }
'LastJSDocNode' { AlexTokenTag AlexRawToken_LastJSDocNode _ }
'FirstJSDocTagNode' { AlexTokenTag AlexRawToken_FirstJSDocTagNode _ }
'LastJSDocTagNode' { AlexTokenTag AlexRawToken_LastJSDocTagNode _ }

-- ****************************
-- *                          *
-- * integers and identifiers *
-- *                          *
-- ****************************

INT    { AlexTokenTag (AlexRawToken_INT  i) _ }
STR    { AlexTokenTag (AlexRawToken_STR  s) _ }
ID     { AlexTokenTag (AlexRawToken_ID  id) _ }

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
barlistof(a):       a { [$1] } | a 'BarToken'       loc '(' ')' barlistof(a)       { $1:$6 }
ampersandlistof(a): a { [$1] } | a 'AmpersandToken' loc '(' ')' ampersandlistof(a) { $1:$6 }

-- **********************
-- *                    *
-- * parametrized lists *
-- *                    *
-- **********************
commalistof(a): a { [$1] } | a ',' commalistof(a) { $1:$3 }
possibly_empty_listof(a): { [] } | commalistof(a) { $1 }

-- ******************
-- *                *
-- * optional rules *
-- *                *
-- ******************
optional(a): { Nothing } | a { Just $1 }

-- *********************
-- *                   *
-- * Ast root: program *
-- *                   *
-- *********************
program: commalistof(stmt)
{
    Ast.Root
    {
        Ast.filename = "DDD",
        stmts = $1
    }
}

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

parameter:
'Parameter' loc
'('
    identifier
    optional(type_hint)
')'
{
    Ast.Param
    {
        Ast.paramName = Token.ParamName $4,
        Ast.paramNominalType = Token.NominalTy (Token.Named "any" $2),
        Ast.paramSerialIdx = 156
    }
}

throwKeyword:        'ThrowKeyword'        loc '(' ')' { Nothing }
importKeyword:       'ImportKeyword'       loc '(' ')' { Nothing }
interfaceKeyword:    'InterfaceKeyword'    loc '(' ')' { Nothing }
nullKeyword:         'NullKeyword'         loc '(' ')' { $2 }
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
exclamationToken:    'ExclamationToken'    loc '(' ')' { Nothing }
undefinedKeyword:    'UndefinedKeyword'    loc '(' ')' { Nothing }
templateHead:        'TemplateHead'        loc '(' ')' { Nothing }
templateMiddle:      'TemplateMiddle'      loc '(' ')' { Nothing }
lastTemplateToken:   'LastTemplateToken'   loc '(' ')' { Nothing }
dotToken:            'DotToken'            loc '(' ')' { Nothing }
barBarToken:         'BarBarToken'         loc '(' ')' { Nothing }
stringKeyword:       'StringKeyword'       loc '(' ')' { Nothing }
voidKeyword:         'VoidKeyword'         loc '(' ')' { Nothing }
fromKeyword:         'FromKeyword'         loc '(' ')' { Nothing }
extendsKeyword:      'ExtendsKeyword'      loc '(' ')' { Nothing }
questionToken:       'QuestionToken'       loc '(' ')' { Nothing }
openParenToken:      'OpenParenToken'      loc '(' ')' { Nothing }
openBracketToken:    'OpenBracketToken'    loc '(' ')' { Nothing }
closeBracketToken:   'CloseBracketToken'   loc '(' ')' { Nothing }
closeParenToken:     'CloseParenToken'     loc '(' ')' { Nothing }
asKeyword:           'AsKeyword'           loc '(' ')' { Nothing }
asteriskToken:       'AsteriskToken'       loc '(' ')' { Nothing }
colonToken:          'ColonToken'          loc '(' ')' { Nothing }
firstAssignment:     'FirstAssignment'     loc '(' ')' { Nothing }
firstBinaryOperator: 'FirstBinaryOperator' loc '(' ')' { Nothing }
greaterThanToken:    'GreaterThanToken'    loc '(' ')' { Nothing }
ampAmpToken:         'AmpersandAmpersandToken' loc '(' ')' { Nothing }
eqEqEqToken:         'EqualsEqualsEqualsToken' loc '(' ')' { Nothing }
exclamationEqEqToken: 'ExclamationEqualsEqualsToken' loc '(' ')' { Nothing }

importee: asteriskToken { Nothing }

importSpecifier:
'ImportSpecifier' loc
'('
    identifier
')'
{
    Nothing
}


namedImports:
'NamedImports' loc
'('
    commalistof(importSpecifier)
')'
{
    Nothing
}

importClauseStuff:
namedImports
{
    Nothing
}

importClause:
'ImportClause' loc
'('
    importClauseStuff
')'
{
    Nothing
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
        Token.constStrValue = tokSTRValue $4,
        Token.constStrLocation = $2
    }
}

-- ***************
-- *             *
-- * stmt import *
-- *             *
-- ***************
stmt_import:
'ImportDeclaration' loc
'('
    importKeyword
    importClause
    fromKeyword
    stringLiteral
')'
{
    Ast.StmtImport $ Ast.StmtImportContent
    {
        Ast.stmtImportSource = "Zuchmir",
        Ast.stmtImportFromSource = Just "Moish",
        Ast.stmtImportAlias = Just "MM",
        Ast.stmtImportLocation = $2
    }
}

literalType:
'LiteralType' loc '(' stringLiteral ')' { Nothing } |
'LiteralType' loc '(' nullKeyword   ')' { Nothing }

typeLiteral:
'TypeLiteral' loc
'('
    optional(listof(stmt_property))
')'
{
    Nothing
}

internal_type:
booleanKeyword   { Nothing } |
anyKeyword       { Nothing } |
unknownKeyword   { Nothing } |
undefinedKeyword { Nothing } |
stringKeyword    { Nothing } |
voidKeyword      { Nothing } |
identifier       { Nothing } |
typeReference    { Nothing } |
literalType      { Nothing } |
typeLiteral      { Nothing }

generics:
firstBinaryOperator
commalistof(type)
greaterThanToken
{
    Nothing
}

typeReference: 'TypeReference' loc '(' type ')'
{
    Nothing
}

type_hint: colonToken type
{
    Nothing
}

-- ***************
-- *             *
-- * stmt decvar *
-- *             *
-- ***************
stmt_decvar_1:
'VariableDeclarationList' loc
'('
    'VariableDeclaration' loc '(' identifier optional(type_hint) firstAssignment exp ')'
')'
{
    Ast.StmtVardec $ Ast.StmtVardecContent
    {
        Ast.stmtVardecName = Token.VarName $7,
        Ast.stmtVardecNominalType = Token.NominalTy $7,
        Ast.stmtVardecInitValue = Just $10,
        Ast.stmtVardecLocation = $2
    }
}

bindingElement:
'BindingElement' loc
'('
    identifier
')'
{
    Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $4
}

objectBindingPattern:
'ObjectBindingPattern' loc
'('
    commalistof(bindingElement)
')'
{
    $4
}

-- ***************
-- *             *
-- * stmt decvar *
-- *             *
-- ***************
stmt_decvar_2:
'VariableDeclarationList' loc
'('
    'VariableDeclaration' loc
    '('
        objectBindingPattern
        firstAssignment
        exp
    ')'
')'
{
    Ast.StmtBlock $ Ast.StmtBlockContent (assignify $7 $9) $2
}

stmt_decvar:
stmt_decvar_1 { $1 } |
stmt_decvar_2 { $1 }

expressionWithTypeArguments:
'ExpressionWithTypeArguments' loc
'('
    type
')'
{
    Nothing
}

indexedAccessType:
'IndexedAccessType' loc
'('
    type
    openBracketToken
    internal_type
    closeBracketToken
')'
{
    Nothing
}

union_type:
'UnionType' loc
'('
    barlistof(type)
')'
{
    Nothing
}

intersection_type:
'IntersectionType' loc
'('
    ampersandlistof(type)
')'
{
    Nothing
}

parenthesized_type:
'ParenthesizedType' loc
'('
    openParenToken
    type
    closeParenToken
')'
{
    Nothing
}

type_operator:
'TypeOperator' loc
'('
    type
')'
{
    Nothing
}

type:
expressionWithTypeArguments      { Nothing } |
indexedAccessType                { Nothing } |
union_type                       { Nothing } |
intersection_type                { Nothing } |
parenthesized_type               { Nothing } |
type_operator                    { Nothing } |
internal_type optional(generics) { Nothing }

extends:
'HeritageClause' loc
'('
    extendsKeyword
    commalistof(type)
')'
{
}

-- ******************
-- *                *
-- * stmt interface *
-- *                *
-- ******************
stmt_interface:
'InterfaceDeclaration' loc
'('
    interfaceKeyword
    identifier
    optional(extends)
    listof(stmt)
')'
{
    Ast.StmtClass $ Ast.StmtClassContent
    {
        Ast.stmtClassName = Token.ClassName $5,
        Ast.stmtClassSupers = [],
        Ast.stmtClassDataMembers = Ast.DataMembers Data.Map.empty,
        Ast.stmtClassMethods = Ast.Methods Data.Map.empty
    }
}

-- **************
-- *            *
-- * stmt class *
-- *            *
-- **************
stmt_class:
stmt_interface { $1 }

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
        Ast.stmtVardecNominalType = Token.NominalTy $4,
        Ast.stmtVardecInitValue = Nothing,
        Ast.stmtVardecLocation = $2
    }
}

body:
'Block' loc
'('
    commalistof(stmt)
')'
{
    $4
}

param:
'Parameter' loc
'('
    identifier
    optional(type_hint)
')'
{
    Ast.Param
    {
        Ast.paramName = Token.ParamName $4,
        Ast.paramNominalType = Token.NominalTy $ Token.Named
        {
            Token.content = "any",
            Token.location = $2
        },
        Ast.paramSerialIdx = 156
    }
}

-- *****************
-- *               *
-- * stmt function *
-- *               *
-- *****************
stmt_function:
'FunctionDeclaration' loc
'('
    functionKeyword
    identifier
    openParenToken
    optional(commalistof(param))
    closeParenToken
    optional(type_hint)
    optional(body)
')'
{
    Ast.StmtFunc $ Ast.StmtFuncContent
    {
        Ast.stmtFuncReturnType = Token.NominalTy $ Token.Named
        {
            Token.content = "any",
            Token.location = $2
        },
        Ast.stmtFuncName = Token.FuncName $5,
        Ast.stmtFuncParams = case $7 of { Nothing -> []; Just params -> params },
        Ast.stmtFuncBody = case $10 of { Nothing -> []; Just stmts -> stmts },
        Ast.stmtFuncAnnotations = [],
        Ast.stmtFuncLocation = $2
    }
}

-- ***********
-- *         *
-- * stmt if *
-- *         *
-- ***********
stmt_if_1:
'IfStatement' loc
'('
    ifKeyword
    openParenToken
    exp
    closeParenToken
    body
')'
{
    Ast.StmtIf $ Ast.StmtIfContent
    {
        Ast.stmtIfCond = $6,
        Ast.stmtIfBody = [],
        Ast.stmtElseBody = [],
        Ast.stmtIfLocation = $2
    }
}

-- ***********
-- *         *
-- * stmt if *
-- *         *
-- ***********
stmt_if_2:
'IfStatement' loc
'('
    ifKeyword
    openParenToken
    exp
    closeParenToken
    stmt
')'
{
    Ast.StmtIf $ Ast.StmtIfContent
    {
        Ast.stmtIfCond = $6,
        Ast.stmtIfBody = [$8],
        Ast.stmtElseBody = [],
        Ast.stmtIfLocation = $2
    }
}

stmt_if:
stmt_if_1 { $1 } |
stmt_if_2 { $1 }

-- ************
-- *          *
-- * stmt exp *
-- *          *
-- ************
stmt_exp:
'ExpressionStatement' loc
'('
    exp
')'
{
    Ast.StmtExp $4
}

-- ***************
-- *             *
-- * stmt return *
-- *             *
-- ***************
stmt_return:
'ReturnStatement' loc
'('
    returnKeyword
    optional(exp)
')'
{
    Ast.StmtReturn $ Ast.StmtReturnContent
    {
        Ast.stmtReturnValue = Nothing,
        Ast.stmtReturnLocation = $2
    }
}

stmt_throw:
'ThrowStatement' loc
'('
    throwKeyword
    exp
')'
{
    Ast.StmtExp $5
}

-- ********
-- *      *
-- * stmt *
-- *      *
-- ********
stmt:
stmt_if          { $1 } |
stmt_exp         { $1 } |
stmt_import      { $1 } |
stmt_function    { $1 } |
stmt_property    { $1 } |
stmt_class       { $1 } |
stmt_return      { $1 } |
stmt_throw       { $1 } |
stmt_decvar      { $1 }

-- ******************
-- *                *
-- * exp arrow func *
-- *                *
-- ******************
exp_arrow_func:
'ArrowFunction' loc '(' ')'
{
    Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt
    {
        Token.constIntValue = 999,
        Token.constIntLocation = $2
    }
}

-- ************
-- *          *
-- * exp call *
-- *          *
-- ************
exp_call_1:
'CallExpression' loc
'('
    exp
    openParenToken
    commalistof(exp)
    closeParenToken
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = $4,
        Ast.args = $6,
        Ast.expCallLocation = $2
    }
}

-- ************
-- *          *
-- * exp call *
-- *          *
-- ************
exp_call_2:
'CallExpression' loc
'('
    exp
    openParenToken
    closeParenToken
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = $4,
        Ast.args = [],
        Ast.expCallLocation = $2
    }
}

exp_call:
exp_call_1 { $1 } |
exp_call_2 { $1 }

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
firstAssignment      { Nothing } |
barBarToken          { Nothing } |
eqEqEqToken          { Nothing } |
ampAmpToken          { Nothing } |
exclamationEqEqToken { Nothing }

-- *************
-- *           *
-- * exp binop *
-- *           *
-- *************
exp_binop:
'BinaryExpression' loc
'('
    exp
    operator
    exp
')'
{
    Ast.ExpBinop $ Ast.ExpBinopContent
    {
        Ast.expBinopLeft = $4,
        Ast.expBinopRight = $6,
        Ast.expBinopOperator = Ast.PLUS,
        Ast.expBinopLocation = $2
    }
}

var_field:
'PropertyAccessExpression' loc
'('
    exp
    dotToken
    identifier
')'
{
    Ast.VarField $ Ast.VarFieldContent
    {
        Ast.varFieldLhs = $4,
        Ast.varFieldName = Token.FieldName $6,
        Ast.varFieldLocation = $2
    }
}

var_subscript:
'ElementAccessExpression' loc
'('
    exp
    openBracketToken
    exp
    closeBracketToken
')'
{
    Ast.VarSubscript $ Ast.VarSubscriptContent
    {
        Ast.varSubscriptLhs = $4,
        Ast.varSubscriptIdx = $6,
        Ast.varSubscriptLocation = $2
    }
}

var_simple:
identifier
{
    Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $1
}

var:
var_simple    { $1 } |
var_field     { $1 } |
var_subscript { $1 }

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
    listof(template_span)
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
exclamationToken { Nothing }

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
    possibly_empty_listof(parameter)
    closeParenToken
    body
')'
{
    Ast.StmtMethodContent
    {
        Ast.stmtMethodReturnType = Token.NominalTy (Token.Named "any" $2),
        Ast.stmtMethodName = Token.MethdName $4,
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
        Ast.stmtMethodReturnType = Token.NominalTy $1,
        Ast.stmtMethodName = Token.MethdName $1,
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

-- **********************
-- *                    *
-- * exp object literal *
-- *                    *
-- **********************
exp_objliteral:
'ObjectLiteralExpression' loc
'('
    commalistof(shorthandPropertyAssignment)
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named "dictify" $2,
        Ast.args = lambdame $4,
        Ast.expCallLocation = $2
    }
}

-- **************
-- *            *
-- * exp delete *
-- *            *
-- **************
exp_delete:
'DeleteExpression' loc
'('
    deleteKeyword
    exp
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "delete",
            Token.location = $2
        },
        Ast.args = [],
        Ast.expCallLocation = $2
    }
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

-- ************
-- *          *
-- * exp bool *
-- *          *
-- ************
exp_true:
'TrueKeyword' loc '(' ')'
{
    Ast.ExpBool $ Ast.ExpBoolContent $ Token.ConstBool
    {
        Token.constBoolValue = True,
        Token.constBoolLocation = $2
    }
}

-- ************
-- *          *
-- * exp bool *
-- *          *
-- ************
exp_false:
'FalseKeyword' loc '(' ')'
{
    Ast.ExpBool $ Ast.ExpBoolContent $ Token.ConstBool
    {
        Token.constBoolValue = False,
        Token.constBoolLocation = $2
    }
}

-- ************
-- *          *
-- * exp bool *
-- *          *
-- ************
exp_bool:
exp_true  { $1 } |
exp_false { $1 }

-- ************
-- *          *
-- * exp null *
-- *          *
-- ************
exp_null:
nullKeyword
{
    Ast.ExpInt $ Ast.ExpIntContent $ Token.ConstInt
    {
        Token.constIntValue = 888,
        Token.constIntLocation = $1
    }
}

-- ***********
-- *         *
-- * exp new *
-- *         *
-- ***********
exp_new:
'NewExpression' loc
'('
    newKeyword
    type
    openParenToken
    optional(commalistof(exp))
    closeParenToken
')'
{
    Ast.ExpCall $ Ast.ExpCallContent
    {
        Ast.callee = Ast.ExpVar $ Ast.ExpVarContent $ Ast.VarSimple $ Ast.VarSimpleContent $ Token.VarName $ Token.Named
        {
            Token.content = "new",
            Token.location = $2
        },
        Ast.args = case $7 of { Nothing -> []; Just exps -> exps },
        Ast.expCallLocation = $2
    }
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

exp_ty_assert:
'TypeAssertionExpression' loc
'('
    firstBinaryOperator
    typeReference
    greaterThanToken
    exp
')'
{
    $7
}

-- *******
-- *     *
-- * exp *
-- *     *
-- *******
exp:
exp_str        { $1 } |
exp_int        { $1 } |
exp_new        { $1 } |
exp_bool       { $1 } |
exp_null       { $1 } |
fstring        { $1 } |
exp_objliteral { $1 } |
exp_call       { $1 } |
exp_meta       { $1 } |
exp_ternary    { $1 } |
exp_var        { $1 } |
exp_as         { $1 } |
exp_paren      { $1 } |
exp_unop       { $1 } |
exp_delete     { $1 } |
exp_typeof     { $1 } |
exp_binop      { $1 } |
exp_regex      { $1 } |
exp_ty_assert  { $1 } |
exp_arrow_func { $1 }

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

assignify' :: Ast.Var -> Exp -> Ast.Stmt
assignify' v e = Ast.StmtAssign (Ast.StmtAssignContent v e)

assignify :: [ Ast.Var ] -> Exp -> [ Ast.Stmt ]
assignify [] _ = []
assignify (v:vs) e = (assignify' v e):(assignify vs e)

lambdame' :: Ast.StmtMethodContent -> Ast.Exp
lambdame' m = Ast.ExpLambda $ Ast.ExpLambdaContent {
    Ast.expLambdaParams = Ast.stmtMethodParams m,
    Ast.expLambdaBody = Ast.stmtMethodBody m,
    Ast.expLambdaLocation = Ast.stmtMethodLocation m
} 

lambdame :: [ Ast.StmtMethodContent ] -> [ Ast.Exp ]
lambdame = Data.List.map lambdame'

getFuncNameAttr :: [ Either (Either Token.FuncName [ Ast.Param ] ) (Either Token.NominalTy [ Ast.Stmt ] ) ] -> Maybe Token.FuncName
getFuncNameAttr = undefined

getFuncReturnType :: [ Either (Either Token.FuncName [ Ast.Param ] ) (Either Token.NominalTy [ Ast.Stmt ] ) ] -> Maybe Token.NominalTy
getFuncReturnType = undefined

getFuncBody :: [ Either (Either Token.FuncName [ Ast.Param ] ) (Either Token.NominalTy [ Ast.Stmt ] ) ] -> Maybe [ Ast.Stmt ]
getFuncBody = undefined

getFuncParams :: [ Either (Either Token.FuncName [ Ast.Param ] ) (Either Token.NominalTy [ Ast.Stmt ] ) ] -> Maybe [ Ast.Param ]
getFuncParams = undefined

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
