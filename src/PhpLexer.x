{
-- | 
-- * The lexer takes an input source file, and returns a stream of
-- [tokens](https://en.wikipedia.org/wiki/Lexical_analysis#Token).
--
-- * The token data type ( `AlexTokenTag` ) contains the following content:
--
--     * source filename and exact location
--     * token lexical kind
--     * associated metadata (like `AlexRawToken_ID`
--     which holds the underlying identifier name)
-- 
-- * The /single/ client of the lexer code is the `parser` module,
--
--     * it consumes the tokens' stream and gradually builds the [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree)
--     * when doing so, the parser only inspects the /lexical kind/ of the token
--     * it /completely ignores/ any other infromation in the token
--

{-# OPTIONS -w  #-}

module PhpLexer

where

import Prelude hiding (lex)
import Control.Monad ( liftM )
import Data.List
import Location
}

-- ***********
-- *         *
-- * wrapper *
-- *         *
-- ***********
%wrapper "monadUserState"

-- ***********************
-- *                     *
-- * regular expressions *
-- *                     *
-- ***********************

-- ***************
-- *             *
-- * parentheses *
-- *             *
-- ***************
@LPAREN = \(
@RPAREN = \)
@LBRACK = \[
@RBRACK = \]

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************
@COLON  = ":"
@SLASH  = "\"
@HYPHEN = \- 
@OR     = \|

-- ************
-- *          *
-- * keywords *
-- *          *
-- ************
@KW_ARG             = "Arg"
@KW_VAR             = "var"
@KW_ARGS            = "args"
@KW_NAME            = "name"
@KW_USES            = "uses"
@KW_EXPR            = "expr"
@KW_MAME            = "Name"
@KW_NULL            = "null"
@KW_TYPE            = "type"
@KW_LEFT            = "left"
@KW_LOOP            = "loop"
@KW_INIT            = "init"
@KW_COND            = "cond"
@KW_EXPRS           = "exprs"
@KW_VALUE           = "value"
@KW_RIGHT           = "right"
@KW_STMTS           = "stmts"
@KW_ARRAY           = "array"
@KW_PARAM           = "Param"
@KW_STMT_IF         = "Stmt_If"
@KW_STMT_ELSE       = "Stmt_Else"
@KW_STMT_ELIF       = "Stmt_ElseIf"
@KW_USE_ITEM        = "UseItem"
@KW_STMT_FOR        = "Stmt_For"
@KW_STMT_NOP        = "Stmt_Nop"
@KW_STMT_SWITCH     = "Stmt_Switch"
@KW_STMT_CASE       = "Stmt_Case"
@KW_STMT_FOREACH    = "Stmt_Foreach"
@KW_STMT_USE        = "Stmt_Use"
@KW_STMT_ECHO       = "Stmt_Echo"
@KW_STMT_UNSET      = "Stmt_Unset"
@KW_EXPR_VAR        = "Expr_Variable"
@KW_EXPR_NEW        = "Expr_New"
@KW_EXPR_EXIT       = "Expr_Exit"
@KW_EXPR_IMPORT     = "Expr_Include"
@KW_EXPR_TERNARY    = "Expr_Ternary"
@KW_EXPR_LAMBDA     = "Expr_Closure"
@KW_EXPR_CAST       = "Expr_Cast_Double"
@KW_EXPR_CAST2      = "Expr_Cast_Int"
@KW_EXPR_CAST3      = "Expr_Cast_String"
@KW_EXPR_CAST4      = "Expr_Cast_Bool"
@KW_EXPR_ASSIGN     = "Expr_Assign"
@KW_EXPR_ISSET      = "Expr_Isset"
@KW_EXPR_ARRAY      = "Expr_Array"
@KW_EXPR_EMPTY      = "Expr_Empty"
@KW_EXPR_ARRAY3     = "ArrayItem"
@KW_EXPR_ARRAY2     = "Expr_ArrayDimFetch"
@KW_EXPR_CALL       = "Expr_FuncCall"
@KW_EXPR_SCALL      = "Expr_StaticCall"
@KW_EXPR_MCALL      = "Expr_MethodCall"
@KW_STMT_EXPR       = "Stmt_Expression"
@KW_SCALAR_INT      = "Scalar_Int"
@KW_SCALAR_FSTRING  = "Scalar_InterpolatedString"
@KW_SCALAR_FILE     = "Scalar_MagicConst_File"
@KW_IDENTIFIER      = "Identifier"
@KW_RETURN_TYPE     = "returnType"
@KW_STMT_RETURN     = "Stmt_Return"
@KW_STMT_CLASS      = "Stmt_Class"
@KW_STMT_CONT       = "Stmt_Continue"
@KW_STMT_BREAK      = "Stmt_Break"
@KW_STMT_PROPERTY   = "Stmt_Property"
@KW_STMT_CLASSMETH  = "Stmt_ClassMethod"
@KW_STMT_FUNCTION   = "Stmt_Function"
@KW_EXPR_CONST_GET  = "Expr_ConstFetch"
@KW_EXPR_PROP_GET   = "Expr_PropertyFetch"
@KW_EXPR_PROP_GET2  = "Expr_StaticPropertyFetch"
@KW_EXPR_BINOP_LT   = "Expr_BinaryOp_Smaller"
@KW_EXPR_BINOP_GT   = "Expr_BinaryOp_Greater"
@KW_EXPR_BINOP_EQ   = "Expr_BinaryOp_Equal"
@KW_EXPR_BINOP_IS   = "Expr_BinaryOp_Identical"
@KW_EXPR_BINOP_ISNOT = "Expr_BinaryOp_NotIdentical"
@KW_EXPR_UNOP_NOT   = "Expr_BooleanNot"
@KW_EXPR_BINOP_PLUS = "Expr_BinaryOp_Plus"
@KW_EXPR_BINOP_MINUS = "Expr_BinaryOp_Minus"
@KW_EXPR_UNOP_MINUS = "Expr_UnaryMinus"
@KW_EXPR_BINOP_CONCAT = "Expr_BinaryOp_Concat"
@KW_EXPR_BINOP_OR   = "Expr_BinaryOp_BooleanOr"
@KW_EXPR_BINOP_AND  = "Expr_BinaryOp_BooleanAnd"

-- ************
-- *          *
-- * integers *
-- *          *
-- ************
@DIGIT = 0-9
@INT   = @DIGIT+

-- ***************
-- *             *
-- * identifiers *
-- *             *
-- ***************
@LETTER = [A-Za-z_]
@LETTER_OR_DIGIT = @LETTER | @DIGIT
@ID = @LETTER(@LETTER_OR_DIGIT*)

-- ***********
-- *         *
-- * strings *
-- *         *
-- ***********
@NON_WHITE = . # $white
@LOC = (@INT)(":")(@INT)
@DASH = ($white)(\-)($white)
@RANGE = (@LOC)(@DASH)(@LOC)
$no_newline = [ $printable $white ] # \n 
@STR1 = "Scalar_String"(@LBRACK)(@RANGE)(@RBRACK)(@LPAREN)($white+)("value: ")($no_newline*)\n($white+)(@RPAREN)
@STR2 = "InterpolatedStringPart"(@LBRACK)(@RANGE)(@RBRACK)(@LPAREN)($white+)("value: ")($no_newline*)\n($white+)(@RPAREN)
@STR=(@STR1)|(@STR2)

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

@LPAREN    { lex' AlexRawToken_LPAREN }
@RPAREN    { lex' AlexRawToken_RPAREN }
@LBRACK    { lex' AlexRawToken_LBRACK }
@RBRACK    { lex' AlexRawToken_RBRACK }

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************

@COLON     { lex' AlexRawToken_COLON  }
@SLASH     { lex' AlexRawToken_SLASH  }
@HYPHEN    { lex' AlexRawToken_HYPHEN }
@OR        { lex' AlexRawToken_OR     }

-- ************
-- *          *
-- * keywords *
-- *          *
-- ************

@KW_ARG             { lex' AlexRawToken_ARG             }
@KW_VAR             { lex' AlexRawToken_VAR             }
@KW_ARGS            { lex' AlexRawToken_ARGS            }
@KW_NAME            { lex' AlexRawToken_NAME            }
@KW_USES            { lex' AlexRawToken_USES            }
@KW_EXPR            { lex' AlexRawToken_EXPR            }
@KW_MAME            { lex' AlexRawToken_MAME            }
@KW_NULL            { lex' AlexRawToken_NULL            }
@KW_TYPE            { lex' AlexRawToken_TYPE            }
@KW_LEFT            { lex' AlexRawToken_LEFT            }
@KW_LOOP            { lex' AlexRawToken_LOOP            }
@KW_INIT            { lex' AlexRawToken_INIT            }
@KW_COND            { lex' AlexRawToken_COND            }
@KW_EXPRS           { lex' AlexRawToken_EXPRS           }
@KW_VALUE           { lex' AlexRawToken_VALUE           }
@KW_RIGHT           { lex' AlexRawToken_RIGHT           }
@KW_STMTS           { lex' AlexRawToken_STMTS           }
@KW_ARRAY           { lex' AlexRawToken_ARRAY           }
@KW_PARAM           { lex' AlexRawToken_PARAM           }
@KW_STMT_IF         { lex' AlexRawToken_STMT_IF         }
@KW_STMT_ELSE       { lex' AlexRawToken_STMT_ELSE       }
@KW_STMT_ELIF       { lex' AlexRawToken_STMT_ELIF       }
@KW_USE_ITEM        { lex' AlexRawToken_USE_ITEM        }
@KW_STMT_FOR        { lex' AlexRawToken_STMT_FOR        }
@KW_STMT_NOP        { lex' AlexRawToken_STMT_NOP        }
@KW_STMT_SWITCH     { lex' AlexRawToken_STMT_SWITCH     }
@KW_STMT_CASE       { lex' AlexRawToken_STMT_CASE       }
@KW_STMT_FOREACH    { lex' AlexRawToken_STMT_FOREACH    }
@KW_STMT_USE        { lex' AlexRawToken_STMT_USE        }
@KW_STMT_ECHO       { lex' AlexRawToken_STMT_ECHO       }
@KW_STMT_UNSET      { lex' AlexRawToken_STMT_UNSET      }
@KW_EXPR_VAR        { lex' AlexRawToken_EXPR_VAR        }
@KW_EXPR_NEW        { lex' AlexRawToken_EXPR_NEW        }
@KW_EXPR_EXIT       { lex' AlexRawToken_EXPR_EXIT       }
@KW_EXPR_IMPORT     { lex' AlexRawToken_EXPR_IMPORT     }
@KW_EXPR_TERNARY    { lex' AlexRawToken_EXPR_TERNARY    }
@KW_EXPR_LAMBDA     { lex' AlexRawToken_EXPR_LAMBDA     }
@KW_EXPR_CAST       { lex' AlexRawToken_EXPR_CAST       }
@KW_EXPR_CAST4      { lex' AlexRawToken_EXPR_CAST4      }
@KW_EXPR_CAST3      { lex' AlexRawToken_EXPR_CAST3      }
@KW_EXPR_CAST2      { lex' AlexRawToken_EXPR_CAST2      }
@KW_EXPR_ASSIGN     { lex' AlexRawToken_EXPR_ASSIGN     }
@KW_EXPR_ISSET      { lex' AlexRawToken_EXPR_ISSET      }
@KW_EXPR_ARRAY      { lex' AlexRawToken_EXPR_ARRAY      }
@KW_EXPR_EMPTY      { lex' AlexRawToken_EXPR_EMPTY      }
@KW_EXPR_ARRAY2     { lex' AlexRawToken_EXPR_ARRAY2     }
@KW_EXPR_ARRAY3     { lex' AlexRawToken_EXPR_ARRAY3     }
@KW_EXPR_CALL       { lex' AlexRawToken_EXPR_CALL       }
@KW_EXPR_MCALL      { lex' AlexRawToken_EXPR_MCALL      }
@KW_EXPR_SCALL      { lex' AlexRawToken_EXPR_SCALL      }
@KW_STMT_EXPR       { lex' AlexRawToken_STMT_EXPR       }
@KW_SCALAR_INT      { lex' AlexRawToken_SCALAR_INT      }
@KW_SCALAR_FSTRING  { lex' AlexRawToken_SCALAR_FSTRING  }
@KW_SCALAR_FILE     { lex' AlexRawToken_SCALAR_FILE     }
@KW_IDENTIFIER      { lex' AlexRawToken_IDENTIFIER      }
@KW_RETURN_TYPE     { lex' AlexRawToken_RETURN_TYPE     }
@KW_STMT_RETURN     { lex' AlexRawToken_STMT_RETURN     }
@KW_STMT_CLASS      { lex' AlexRawToken_STMT_CLASS      }
@KW_STMT_CONT       { lex' AlexRawToken_STMT_CONT       }
@KW_STMT_BREAK      { lex' AlexRawToken_STMT_BREAK      }
@KW_STMT_PROPERTY   { lex' AlexRawToken_STMT_PROPERTY   }
@KW_STMT_CLASSMETH  { lex' AlexRawToken_STMT_CLASSMETH  }
@KW_STMT_FUNCTION   { lex' AlexRawToken_STMT_FUNCTION   }
@KW_EXPR_CONST_GET  { lex' AlexRawToken_EXPR_CONST_GET  }
@KW_EXPR_PROP_GET   { lex' AlexRawToken_EXPR_PROP_GET   }
@KW_EXPR_PROP_GET2  { lex' AlexRawToken_EXPR_PROP_GET2  }
@KW_EXPR_BINOP_LT   { lex' AlexRawToken_EXPR_BINOP_LT   }
@KW_EXPR_BINOP_GT   { lex' AlexRawToken_EXPR_BINOP_GT   }
@KW_EXPR_BINOP_EQ   { lex' AlexRawToken_EXPR_BINOP_EQ   }
@KW_EXPR_BINOP_IS   { lex' AlexRawToken_EXPR_BINOP_IS   }
@KW_EXPR_BINOP_ISNOT { lex' AlexRawToken_EXPR_BINOP_ISNOT }
@KW_EXPR_BINOP_PLUS { lex' AlexRawToken_EXPR_BINOP_PLUS }
@KW_EXPR_BINOP_MINUS { lex' AlexRawToken_EXPR_BINOP_MINUS }
@KW_EXPR_UNOP_MINUS { lex' AlexRawToken_EXPR_UNOP_MINUS }
@KW_EXPR_BINOP_CONCAT { lex' AlexRawToken_EXPR_BINOP_CONCAT }
@KW_EXPR_BINOP_OR   { lex' AlexRawToken_EXPR_BINOP_OR   }
@KW_EXPR_BINOP_AND  { lex' AlexRawToken_EXPR_BINOP_AND  }
@KW_EXPR_UNOP_NOT   { lex' AlexRawToken_EXPR_UNOP_NOT   }

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

@ID        { lex  AlexRawToken_ID                 }
@INT       { lex (AlexRawToken_INT . round. read) }
@STR       { lex AlexRawToken_STR                 }

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

     = AlexRawToken_INT Int         -- ^ locations and numbers
     | AlexRawToken_ID String       -- ^ including consant strings
     | AlexRawToken_STR String      -- ^ constant strings
      
     | AlexRawToken_LPAREN          -- ^ Parentheses __(__
     | AlexRawToken_RPAREN          -- ^ Parentheses __)__
     | AlexRawToken_LBRACK          -- ^ Parentheses __[__
     | AlexRawToken_RBRACK          -- ^ Parentheses __]__
 
     | AlexRawToken_ARG             -- ^ Reserved Keyword
     | AlexRawToken_VAR             -- ^ Reserved Keyword
     | AlexRawToken_ARGS            -- ^ Reserved Keyword
     | AlexRawToken_NAME            -- ^ Reserved Keyword
     | AlexRawToken_USES            -- ^ Reserved Keyword
     | AlexRawToken_EXPR            -- ^ Reserved Keyword
     | AlexRawToken_MAME            -- ^ Reserved Keyword
     | AlexRawToken_NULL            -- ^ Reserved Keyword
     | AlexRawToken_TYPE            -- ^ Reserved Keyword
     | AlexRawToken_LEFT            -- ^ Reserved Keyword
     | AlexRawToken_LOOP            -- ^ Reserved Keyword
     | AlexRawToken_INIT            -- ^ Reserved Keyword
     | AlexRawToken_COND            -- ^ Reserved Keyword
     | AlexRawToken_EXPRS           -- ^ Reserved Keyword
     | AlexRawToken_VALUE           -- ^ Reserved Keyword
     | AlexRawToken_RIGHT           -- ^ Reserved Keyword
     | AlexRawToken_STMTS           -- ^ Reserved Keyword
     | AlexRawToken_ARRAY           -- ^ Reserved Keyword
     | AlexRawToken_PARAM           -- ^ Reserved Keyword
     | AlexRawToken_STMT_IF         -- ^ Reserved Keyword
     | AlexRawToken_STMT_ELSE       -- ^ Reserved Keyword
     | AlexRawToken_STMT_ELIF       -- ^ Reserved Keyword
     | AlexRawToken_USE_ITEM        -- ^ Reserved Keyword
     | AlexRawToken_STMT_FOR        -- ^ Reserved Keyword
     | AlexRawToken_STMT_NOP        -- ^ Reserved Keyword
     | AlexRawToken_STMT_SWITCH     -- ^ Reserved Keyword
     | AlexRawToken_STMT_CASE       -- ^ Reserved Keyword
     | AlexRawToken_STMT_FOREACH    -- ^ Reserved Keyword
     | AlexRawToken_STMT_USE        -- ^ Reserved Keyword
     | AlexRawToken_STMT_ECHO       -- ^ Reserved Keyword
     | AlexRawToken_STMT_UNSET      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_VAR        -- ^ Reserved Keyword
     | AlexRawToken_EXPR_NEW        -- ^ Reserved Keyword
     | AlexRawToken_EXPR_EXIT       -- ^ Reserved Keyword
     | AlexRawToken_EXPR_IMPORT     -- ^ Reserved Keyword
     | AlexRawToken_EXPR_TERNARY    -- ^ Reserved Keyword
     | AlexRawToken_EXPR_LAMBDA     -- ^ Reserved Keyword
     | AlexRawToken_EXPR_CAST       -- ^ Reserved Keyword
     | AlexRawToken_EXPR_CAST4      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_CAST3      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_CAST2      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_ASSIGN     -- ^ Reserved Keyword
     | AlexRawToken_EXPR_ISSET      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_ARRAY      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_EMPTY      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_ARRAY2     -- ^ Reserved Keyword
     | AlexRawToken_EXPR_ARRAY3     -- ^ Reserved Keyword
     | AlexRawToken_EXPR_CALL       -- ^ Reserved Keyword
     | AlexRawToken_EXPR_MCALL      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_SCALL      -- ^ Reserved Keyword
     | AlexRawToken_STMT_EXPR       -- ^ Reserved Keyword
     | AlexRawToken_SCALAR_INT      -- ^ Reserved Keyword
     | AlexRawToken_SCALAR_FSTRING  -- ^ Reserved Keyword
     | AlexRawToken_SCALAR_FILE     -- ^ Reserved Keyword
     | AlexRawToken_SCALAR_STR      -- ^ Reserved Keyword
     | AlexRawToken_IDENTIFIER      -- ^ Reserved Keyword
     | AlexRawToken_STMT_RETURN     -- ^ Reserved Keyword
     | AlexRawToken_RETURN_TYPE     -- ^ Reserved Keyword
     | AlexRawToken_STMT_CLASS      -- ^ Reserved Keyword
     | AlexRawToken_STMT_CONT       -- ^ Reserved Keyword
     | AlexRawToken_STMT_BREAK      -- ^ Reserved Keyword
     | AlexRawToken_STMT_PROPERTY   -- ^ Reserved Keyword
     | AlexRawToken_STMT_CLASSMETH  -- ^ Reserved Keyword
     | AlexRawToken_STMT_FUNCTION   -- ^ Reserved Keyword
     | AlexRawToken_EXPR_CONST_GET  -- ^ Reserved Keyword
     | AlexRawToken_EXPR_PROP_GET   -- ^ Reserved Keyword
     | AlexRawToken_EXPR_PROP_GET2  -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_LT   -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_GT   -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_EQ   -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_IS   -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_ISNOT -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_PLUS  -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_MINUS -- ^ Reserved Keyword
     | AlexRawToken_EXPR_UNOP_MINUS  -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_CONCAT -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_OR   -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_AND  -- ^ Reserved Keyword
     | AlexRawToken_EXPR_UNOP_NOT   -- ^ Reserved Keyword

     | AlexRawToken_COLON           -- ^ Punctuation __:__
     | AlexRawToken_SLASH           -- ^ Punctuation __:__
     | AlexRawToken_HYPHEN          -- ^ Punctuation __-__
     | AlexRawToken_OR              -- ^ Punctuation __-__

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

-- **************
-- * alexError' *
-- **************
alexError' :: Location -> Alex a
alexError' location = alexError $ "ERROR[" ++ show location ++ "]\n"

-- ************
-- *          *
-- * filename *
-- *          *
-- ************
getFilename :: AlexTokenTag -> String
getFilename = Location.filename . location

-- ************
-- *          *
-- * location *
-- *          *
-- ************
location :: AlexTokenTag -> Location
location = tokenLoc

-- ***************
-- *             *
-- * tokIntValue *
-- *             *
-- ***************
tokIntValue :: AlexTokenTag -> Int
tokIntValue t = case (tokenRaw t) of { AlexRawToken_INT i -> i; _ -> 0; }

-- **************
-- *            *
-- * tokIDValue *
-- *            *
-- **************
tokIDValue :: AlexTokenTag -> String
tokIDValue t = case (tokenRaw t) of { AlexRawToken_ID s -> s; _ -> ""; }

-- **************
-- *            *
-- * findString *
-- *            *
-- **************
findString :: String -> String -> Maybe Int
findString needle haystack = Data.List.findIndex (Data.List.isPrefixOf needle) (Data.List.tails haystack)

-- ******************
-- *                *
-- * tokStrLocation *
-- *                *
-- ******************
tokStrLocation' :: AlexTokenTag -> String -> Maybe Location
tokStrLocation' t s = case (findString "[" s) of
    Nothing -> Nothing
    Just i1 -> let s1 = drop (i1 + 1) s in case (findString ":" s1) of
        Nothing -> Nothing
        Just i2 -> let { startLine = read (take i2 s1); s2 = (drop (i2 + 1) s1) } in case (findString " - " s2) of
            Nothing -> Nothing
            Just i3 -> let { startCol = read (take i3 s2); s3 = drop (i3 + 3) s2 } in case (findString ":" s3) of
                Nothing -> Nothing
                Just i4 -> let { endLine = read (take i4 s3); s4 = drop (i4 + 1) s3 } in case (findString "]" s4) of
                    Nothing -> Nothing
                    Just i5 -> let { endCol = read (take i5 s4) } in Just $ Location {
                        Location.filename = Location.filename (tokenLoc t),
                        Location.lineStart = startLine,
                        Location.lineEnd = endLine,
                        Location.colStart = startCol,
                        Location.colEnd = endCol
                    }

-- ******************
-- *                *
-- * tokStrLocation *
-- *                *
-- ******************
tokStrLocation :: AlexTokenTag -> Maybe Location
tokStrLocation t = case (tokenRaw t) of { (AlexRawToken_STR s) -> tokStrLocation' t s; _ -> Nothing }

-- ***************
-- *             *
-- * tokStrValue *
-- *             *
-- ***************
tokStrValue'' :: AlexTokenTag -> String -> Maybe String
tokStrValue'' t s = case (findString "value: " s) of
    Nothing -> Nothing
    Just i1 -> let s1 = drop (i1 + 7) s in case (findString "\n" s1) of
        Nothing -> Nothing
        Just i2 -> Just (take i2 s1)

-- ***************
-- *             *
-- * tokStrValue *
-- *             *
-- ***************
tokStrValue' :: AlexTokenTag -> String -> Maybe (String,Location)
tokStrValue' t s = case (tokStrLocation t) of 
    Nothing -> Nothing
    Just location -> case (tokStrValue'' t s) of
        Nothing -> Nothing
        Just constStrValue -> Just (constStrValue,location)

-- ***************
-- *             *
-- * tokStrValue *
-- *             *
-- ***************
tokStrValue :: AlexTokenTag -> Maybe (String,Location)
tokStrValue t = case (tokenRaw t) of { (AlexRawToken_STR s) -> tokStrValue' t s; _ -> Nothing; }

-- ************
-- *          *
-- * runAlex' *
-- *          *
-- ************
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)
}
