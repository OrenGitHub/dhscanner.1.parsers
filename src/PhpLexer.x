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
@KW_USE_ITEM        = "UseItem"
@KW_STMT_FOR        = "Stmt_For"
@KW_STMT_USE        = "Stmt_Use"
@KW_STMT_ECHO       = "Stmt_Echo"
@KW_EXPR_VAR        = "Expr_Variable"
@KW_EXPR_NEW        = "Expr_New"
@KW_EXPR_LAMBDA     = "Expr_Closure"
@KW_EXPR_ASSIGN     = "Expr_Assign"
@KW_EXPR_ARRAY      = "Expr_Array"
@KW_EXPR_CALL       = "Expr_FuncCall"
@KW_EXPR_SCALL      = "Expr_StaticCall"
@KW_EXPR_MCALL      = "Expr_MethodCall"
@KW_STMT_EXPR       = "Stmt_Expression"
@KW_SCALAR_INT      = "Scalar_Int"
@KW_SCALAR_STR      = "Scalar_String"
@KW_IDENTIFIER      = "Identifier"
@KW_RETURN_TYPE     = "returnType"
@KW_STMT_RETURN     = "Stmt_Return"
@KW_STMT_CLASS      = "Stmt_Class"
@KW_STMT_PROPERTY   = "Stmt_Property"
@KW_STMT_CLASSMETH  = "Stmt_ClassMethod"
@KW_STMT_FUNCTION   = "Stmt_Function"
@KW_EXPR_CONST_GET  = "Expr_ConstFetch"
@KW_EXPR_PROP_GET   = "Expr_PropertyFetch"
@KW_EXPR_BINOP_LT   = "Expr_BinaryOp_Smaller"
@KW_EXPR_BINOP_PLUS = "Expr_BinaryOp_Plus"

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
@REST = "/token"|"/status"|"/frontend/public/pwned"|"You've been pwned !"|"Everything seems fine"|"/upload/profile/photo"|"/uploads/v1/user/profile.png"|"saved profile picture to /uploads/v1/user/profile.png"|"/test"|"999 666 MMM"|"/ghsa_97m3"|"DDD 444 111"

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
@KW_USE_ITEM        { lex' AlexRawToken_USE_ITEM        }
@KW_STMT_FOR        { lex' AlexRawToken_STMT_FOR        }
@KW_STMT_USE        { lex' AlexRawToken_STMT_USE        }
@KW_STMT_ECHO       { lex' AlexRawToken_STMT_ECHO       }
@KW_EXPR_VAR        { lex' AlexRawToken_EXPR_VAR        }
@KW_EXPR_NEW        { lex' AlexRawToken_EXPR_NEW        }
@KW_EXPR_LAMBDA     { lex' AlexRawToken_EXPR_LAMBDA     }
@KW_EXPR_ASSIGN     { lex' AlexRawToken_EXPR_ASSIGN     }
@KW_EXPR_ARRAY      { lex' AlexRawToken_EXPR_ARRAY      }
@KW_EXPR_CALL       { lex' AlexRawToken_EXPR_CALL       }
@KW_EXPR_MCALL      { lex' AlexRawToken_EXPR_MCALL      }
@KW_EXPR_SCALL      { lex' AlexRawToken_EXPR_SCALL      }
@KW_STMT_EXPR       { lex' AlexRawToken_STMT_EXPR       }
@KW_SCALAR_INT      { lex' AlexRawToken_SCALAR_INT      }
@KW_SCALAR_STR      { lex' AlexRawToken_SCALAR_STR      }
@KW_IDENTIFIER      { lex' AlexRawToken_IDENTIFIER      }
@KW_RETURN_TYPE     { lex' AlexRawToken_RETURN_TYPE     }
@KW_STMT_RETURN     { lex' AlexRawToken_STMT_RETURN     }
@KW_STMT_CLASS      { lex' AlexRawToken_STMT_CLASS      }
@KW_STMT_PROPERTY   { lex' AlexRawToken_STMT_PROPERTY   }
@KW_STMT_CLASSMETH  { lex' AlexRawToken_STMT_CLASSMETH  }
@KW_STMT_FUNCTION   { lex' AlexRawToken_STMT_FUNCTION   }
@KW_EXPR_CONST_GET  { lex' AlexRawToken_EXPR_CONST_GET  }
@KW_EXPR_PROP_GET   { lex' AlexRawToken_EXPR_PROP_GET   }
@KW_EXPR_BINOP_LT   { lex' AlexRawToken_EXPR_BINOP_LT   }
@KW_EXPR_BINOP_PLUS { lex' AlexRawToken_EXPR_BINOP_PLUS }

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
@REST      { lex AlexRawToken_REST                }

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
     | AlexRawToken_REST String     -- ^ constant strings with non ID chars
      
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
     | AlexRawToken_USE_ITEM        -- ^ Reserved Keyword
     | AlexRawToken_STMT_FOR        -- ^ Reserved Keyword
     | AlexRawToken_STMT_USE        -- ^ Reserved Keyword
     | AlexRawToken_STMT_ECHO       -- ^ Reserved Keyword
     | AlexRawToken_EXPR_VAR        -- ^ Reserved Keyword
     | AlexRawToken_EXPR_NEW        -- ^ Reserved Keyword
     | AlexRawToken_EXPR_LAMBDA     -- ^ Reserved Keyword
     | AlexRawToken_EXPR_ASSIGN     -- ^ Reserved Keyword
     | AlexRawToken_EXPR_ARRAY      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_CALL       -- ^ Reserved Keyword
     | AlexRawToken_EXPR_MCALL      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_SCALL      -- ^ Reserved Keyword
     | AlexRawToken_STMT_EXPR       -- ^ Reserved Keyword
     | AlexRawToken_SCALAR_INT      -- ^ Reserved Keyword
     | AlexRawToken_SCALAR_STR      -- ^ Reserved Keyword
     | AlexRawToken_IDENTIFIER      -- ^ Reserved Keyword
     | AlexRawToken_STMT_RETURN     -- ^ Reserved Keyword
     | AlexRawToken_RETURN_TYPE     -- ^ Reserved Keyword
     | AlexRawToken_STMT_CLASS      -- ^ Reserved Keyword
     | AlexRawToken_STMT_PROPERTY   -- ^ Reserved Keyword
     | AlexRawToken_STMT_CLASSMETH  -- ^ Reserved Keyword
     | AlexRawToken_STMT_FUNCTION   -- ^ Reserved Keyword
     | AlexRawToken_EXPR_CONST_GET  -- ^ Reserved Keyword
     | AlexRawToken_EXPR_PROP_GET   -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_LT   -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_PLUS -- ^ Reserved Keyword

     | AlexRawToken_COLON           -- ^ Punctuation __:__
     | AlexRawToken_SLASH           -- ^ Punctuation __:__
     | AlexRawToken_HYPHEN          -- ^ Punctuation __-__

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
-- * tokIDValue *
-- *            *
-- **************
tokStrValue :: AlexTokenTag -> String
tokStrValue t = case (tokenRaw t) of { AlexRawToken_REST s -> s; _ -> ""; }

-- ************
-- *          *
-- * runAlex' *
-- *          *
-- ************
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)
}
