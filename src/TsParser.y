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
import Data.Map ( fromList )

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
listof(a):      a { [$1] } | a listof(a) { $1:$2 }
commalistof(a): a { [$1] } | a 'CommaToken' loc '(' ')' commalistof(a) { $1:$6 }

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
program: listof(stmt)
{
    Ast.Root
    {
        Ast.filename = "DDD",
        stmts = []
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

importKeyword: 'ImportKeyword' loc '(' ')' { Nothing }
fromKeyword:   'FromKeyword'   loc '(' ')' { Nothing }
asKeyword:     'AsKeyword'     loc '(' ')' { Nothing }
asteriskToken: 'AsteriskToken' loc '(' ')' { Nothing }

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
'StringLiteral' loc '(' ')'
{
    "MOMO"
}

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
        Ast.stmtImportName = "Zuchmir",
        Ast.stmtImportAlias = "Moish",
        Ast.stmtImportLocation = $2
    }
}

stmt:
stmt_import { $1 }

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
