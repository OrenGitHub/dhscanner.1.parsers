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

module PyLexer

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
@LBRACE = \{
@RBRACE = \}

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************
@COLON  = ":"
@COMMA  = ","
@HYPHEN = \- 

-- ************
-- *          *
-- * keywords *
-- *          *
-- ************
@KW_ID              = id
@KW_CTX             = ctx
@KW_EQ              = Eq
@KW_OR              = Or
@KW_NOT             = Not
@KW_ADD             = Add
@KW_END             = \"end\"
@KW_RAW             = \"raw\"
@KW_LOC             = \"loc\"
@KW_ARG             = arg
@KW_VAR             = "var"
@KW_NULL            = null
@KW_KIND            = \"kind\"
@KW_TAIL            = \"tail\"
@KW_COL             = col_offset
@KW_ECOL            = end_col_offset
@KW_LOAD            = Load
@KW_STORE           = Store
@KW_LINENO          = lineno
@KW_ELINENO         = end_lineno
@KW_TARGET          = target
@KW_TARGETS         = targets
@KW_DEFAULTS        = defaults
@KW_KW_DEFAULTS     = kw_defaults
@KW_TRUE            = true
@KW_FUNC            = func
@KW_ARGS            = args
@KW_ATTR            = attr
@KW_ATTR2           = Attribute
@KW_ARGS2           = arguments
@KW_ARGS3           = posonlyargs
@KW_ARGS4           = kwonlyargs
@KW_EXPR            = expr
@KW_CALL            = Call
@KW_NAME2           = Name
@KW_TYPE            = \"type\"
@KW_LEFT            = \"left\"
@KW_LOOP            = "loop"
@KW_INIT            = \"init\"
@KW_COND            = "cond"
@KW_BODY            = body
@KW_TEST            = test
@KW_LEVEL           = level
@KW_NAME            = name
@KW_ASNAME          = asname
@KW_NAMES           = names
@KW_ALIAS           = alias
@KW_ORELSE          = orelse
@KW_KEYWORD         = keyword
@KW_KEYWORDS        = keywords
@KW_IMPORT          = Import
@KW_FSTRING         = JoinedStr
@KW_IMPORTF         = ImportFrom
@KW_CONVERSION      = conversion
@KW_FORMATTED_VAL   = FormattedValue
@KW_ASSIGN          = Assign
@KW_ASSIGN2         = AugAssign
@KW_MODULE          = Module
@KW_MODULE2         = module
@KW_UPDATE          = \"update\"
@KW_QUASIS          = \"quasis\"
@KW_FALSE           = False
@KW_ITEMS           = items
@KW_WITH            = With
@KW_WITH2           = withitem
@KW_CTX_MANAGER     = context_expr
@KW_START           = \"start\"
@KW_EXPRS           = "exprs"
@KW_VALUE           = value
@KW_VALUES          = values
@KW_ARRAY           = array
@KW_PARAM           = Param
@KW_COMPARE         = Compare
@KW_COMPARE3        = BoolOp
@KW_COMPARE2        = comparators
@KW_DECORATORS      = decorator_list
@KW_TYPE_PARAMS     = type_params
@KW_LEFT            = left
@KW_OPERATOR        = op
@KW_OPERATOR2       = ops
@KW_OPERAND         = operand
@KW_ARGUMENT        = \"argument\"
@KW_ARGUMENTS       = \"arguments\"
@KW_CALLEE          = \"callee\"
@KW_ASYNC           = \"async\"
@KW_GENERATOR       = \"generator\"
@KW_SRC_TYPE        = \"sourceType\"
@KW_EXPRESSION      = \"expression\"
@KW_EXPRESSIONS     = \"expressions\"
@KW_DECLARATIONS    = \"declarations\"
@KW_ALTERNATE       = \"alternate\"
@KW_CONSEQUENT      = \"consequent\"
@KW_STMT_EXPR       = Expr
@KW_EXPR_VAR        = "Expr_Variable"
@KW_SCALAR_INT      = "Scalar_Int"
@KW_IDENTIFIER      = \"Identifier\"
@KW_RETURN_TYPE     = "returnType"
@KW_TEMPLATE_LI     = \"TemplateLiteral\"
@KW_TEMPLATE_EL     = \"TemplateElement\"
@KW_STMT_FUNCTION   = FunctionDef
@KW_FUNCTION_DEC    = \"FunctionDeclaration\"
@KW_EXPR_CONST      = Constant
@KW_EXPR_UNOP       = UnaryOp
@KW_EXPR_BINOP_PLUS = "Expr_BinaryOp_Plus"
@KW_VAR_DECLARATION = \"VariableDeclaration\"
@KW_VAR_DECLARATOR  = \"VariableDeclarator\"

-- **************
-- *            *
-- * statements *
-- *            *
-- **************

@KW_STMT_IF     = If
@KW_STMT_FOR    = \"ForStatement\"
@KW_STMT_BLOCK  = \"BlockStatement\"
@KW_STMT_RETURN = Return
@KW_STMT_EXP    = \"ExpressionStatement\"

-- ***************
-- *             *
-- * expressions *
-- *             *
-- ***************

@KW_EXPR_NEW    = \"NewExpression\"

-- *************
-- *           *
-- * operators *
-- *           *
-- *************

@KW_OP_LT       = \"\<\"
@KW_OP_EQ       = \"==\"
@KW_OP_ASSIGN   = "="
@KW_OP_TIMES    = \"\*\"

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
@LETTER = [A-Za-z_\.\ \"\/\-\=]
@LETTER_OR_DIGIT = @LETTER | @DIGIT
@SQUOTE = \'
@ID = (@SQUOTE)(@LETTER_OR_DIGIT+)(@SQUOTE)

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
@LBRACE    { lex' AlexRawToken_LBRACE }
@RBRACE    { lex' AlexRawToken_RBRACE }

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************

@COLON     { lex' AlexRawToken_COLON  }
@COMMA     { lex' AlexRawToken_COMMA  }
@HYPHEN    { lex' AlexRawToken_HYPHEN }

-- ************
-- *          *
-- * keywords *
-- *          *
-- ************

@KW_ID              { lex' AlexRawToken_KWID            }
@KW_CTX             { lex' AlexRawToken_CTX             }
@KW_OR              { lex' AlexRawToken_OR              }
@KW_EQ              { lex' AlexRawToken_EQ              }
@KW_NOT             { lex' AlexRawToken_NOT             }
@KW_ADD             { lex' AlexRawToken_ADD             }
@KW_END             { lex' AlexRawToken_END             }
@KW_RAW             { lex' AlexRawToken_RAW             }
@KW_LOC             { lex' AlexRawToken_LOC             }
@KW_ARG             { lex' AlexRawToken_ARG             }
@KW_VAR             { lex' AlexRawToken_VAR             }
@KW_NULL            { lex' AlexRawToken_NULL            }
@KW_TAIL            { lex' AlexRawToken_TAIL            }
@KW_KIND            { lex' AlexRawToken_KIND            }
@KW_TEST            { lex' AlexRawToken_TEST            }
@KW_COL             { lex' AlexRawToken_COL             }
@KW_ECOL            { lex' AlexRawToken_ECOL            }
@KW_LOAD            { lex' AlexRawToken_LOAD            }
@KW_STORE           { lex' AlexRawToken_STORE           }
@KW_LINENO          { lex' AlexRawToken_LINE            }
@KW_TARGET          { lex' AlexRawToken_TARGET          }
@KW_TARGETS         { lex' AlexRawToken_TARGETS         }
@KW_DEFAULTS        { lex' AlexRawToken_DEFAULTS        }
@KW_KW_DEFAULTS     { lex' AlexRawToken_KW_DEFAULTS     }
@KW_ELINENO         { lex' AlexRawToken_ELINE           }
@KW_TRUE            { lex' AlexRawToken_TRUE            }
@KW_FUNC            { lex' AlexRawToken_FUNC            }
@KW_ARGS            { lex' AlexRawToken_ARGS            }
@KW_ATTR            { lex' AlexRawToken_ATTR            }
@KW_ATTR2           { lex' AlexRawToken_ATTR2           }
@KW_ARGS2           { lex' AlexRawToken_ARGS2           }
@KW_ARGS3           { lex' AlexRawToken_ARGS3           }
@KW_ARGS4           { lex' AlexRawToken_ARGS4           }
@KW_EXPR            { lex' AlexRawToken_EXPR            }
@KW_NAME            { lex' AlexRawToken_NAME            }
@KW_TYPE            { lex' AlexRawToken_TYPE            }
@KW_LEFT            { lex' AlexRawToken_LEFT            }
@KW_LOOP            { lex' AlexRawToken_LOOP            }
@KW_INIT            { lex' AlexRawToken_INIT            }
@KW_COND            { lex' AlexRawToken_COND            }
@KW_BODY            { lex' AlexRawToken_BODY            }
@KW_LEVEL           { lex' AlexRawToken_LEVEL           }
@KW_NAME            { lex' AlexRawToken_NAME            }
@KW_CALL            { lex' AlexRawToken_CALL            }
@KW_NAME2           { lex' AlexRawToken_NAME2           }
@KW_ASNAME          { lex' AlexRawToken_ASNAME          }
@KW_NAMES           { lex' AlexRawToken_NAMES           }
@KW_ALIAS           { lex' AlexRawToken_ALIAS           }
@KW_ORELSE          { lex' AlexRawToken_ORELSE          }
@KW_KEYWORD         { lex' AlexRawToken_KEYWORD         }
@KW_KEYWORDS        { lex' AlexRawToken_KEYWORDS        }
@KW_IMPORT          { lex' AlexRawToken_IMPORT          }
@KW_CONVERSION      { lex' AlexRawToken_CONVERSION      }
@KW_FSTRING         { lex' AlexRawToken_FSTRING         }
@KW_IMPORTF         { lex' AlexRawToken_IMPORTF         }
@KW_FORMATTED_VAL   { lex' AlexRawToken_FORMATTED_VAL   }
@KW_ASSIGN          { lex' AlexRawToken_ASSIGN          }
@KW_ASSIGN2         { lex' AlexRawToken_ASSIGN2         }
@KW_MODULE          { lex' AlexRawToken_MODULE          }
@KW_MODULE2         { lex' AlexRawToken_MODULE2         }
@KW_UPDATE          { lex' AlexRawToken_UPDATE          }
@KW_QUASIS          { lex' AlexRawToken_QUASIS          }
@KW_FALSE           { lex' AlexRawToken_FALSE           }
@KW_ITEMS           { lex' AlexRawToken_ITEMS           }
@KW_WITH            { lex' AlexRawToken_WITH            }
@KW_WITH2           { lex' AlexRawToken_WITH2           }
@KW_CTX_MANAGER     { lex' AlexRawToken_CTX_MANAGER     }
@KW_START           { lex' AlexRawToken_START           }
@KW_EXPRS           { lex' AlexRawToken_EXPRS           }
@KW_VALUE           { lex' AlexRawToken_VALUE           }
@KW_VALUES          { lex' AlexRawToken_VALUES          }
@KW_ARRAY           { lex' AlexRawToken_ARRAY           }
@KW_PARAM           { lex' AlexRawToken_PARAM           }
@KW_COMPARE         { lex' AlexRawToken_COMPARE         }
@KW_DECORATORS      { lex' AlexRawToken_DECORATORS      }
@KW_TYPE_PARAMS     { lex' AlexRawToken_TYPE_PARAMS     }
@KW_COMPARE3        { lex' AlexRawToken_COMPARE3        }
@KW_COMPARE2        { lex' AlexRawToken_COMPARE2        }
@KW_OPERATOR        { lex' AlexRawToken_OPERATOR        }
@KW_OPERATOR2       { lex' AlexRawToken_OPERATOR2       }
@KW_OPERAND         { lex' AlexRawToken_OPERAND         }
@KW_ALTERNATE       { lex' AlexRawToken_ALTERNATE       }
@KW_CONSEQUENT      { lex' AlexRawToken_CONSEQUENT      }
@KW_ARGUMENT        { lex' AlexRawToken_ARGUMENT        }
@KW_ARGUMENTS       { lex' AlexRawToken_ARGUMENTS       }
@KW_CALLEE          { lex' AlexRawToken_CALLEE          }
@KW_ASYNC           { lex' AlexRawToken_ASYNC           }
@KW_EXPRESSION      { lex' AlexRawToken_EXPRESSION      }
@KW_EXPRESSIONS     { lex' AlexRawToken_EXPRESSIONS     }
@KW_DECLARATIONS    { lex' AlexRawToken_DECLARATIONS    }
@KW_SRC_TYPE        { lex' AlexRawToken_SRC_TYPE        }
@KW_GENERATOR       { lex' AlexRawToken_GENERATOR       }
@KW_STMT_IF         { lex' AlexRawToken_STMT_IF         }
@KW_EXPR_VAR        { lex' AlexRawToken_EXPR_VAR        }
@KW_EXPR_NEW        { lex' AlexRawToken_EXPR_NEW        }
@KW_STMT_EXPR       { lex' AlexRawToken_STMT_EXPR       }
@KW_SCALAR_INT      { lex' AlexRawToken_SCALAR_INT      }
@KW_IDENTIFIER      { lex' AlexRawToken_IDENTIFIER      }
@KW_RETURN_TYPE     { lex' AlexRawToken_RETURN_TYPE     }
@KW_TEMPLATE_LI     { lex' AlexRawToken_TEMPLATE_LI     }
@KW_TEMPLATE_EL     { lex' AlexRawToken_TEMPLATE_EL     }
@KW_FUNCTION_DEC    { lex' AlexRawToken_FUNCTION_DEC    }
@KW_EXPR_CONST      { lex' AlexRawToken_EXPR_CONST      }
@KW_EXPR_UNOP       { lex' AlexRawToken_EXPR_UNOP       }
@KW_EXPR_BINOP_PLUS { lex' AlexRawToken_EXPR_BINOP_PLUS }
@KW_VAR_DECLARATION { lex' AlexRawToken_VAR_DECLARATION }
@KW_VAR_DECLARATOR  { lex' AlexRawToken_VAR_DECLARATOR  }

-- **************
-- *            *
-- * statements *
-- *            *
-- **************

@KW_STMT_IF       { lex' AlexRawToken_STMT_IF       }
@KW_STMT_FOR      { lex' AlexRawToken_STMT_FOR      }
@KW_STMT_BLOCK    { lex' AlexRawToken_STMT_BLOCK    }
@KW_STMT_RETURN   { lex' AlexRawToken_STMT_RETURN   }
@KW_STMT_EXP      { lex' AlexRawToken_STMT_EXP      }
@KW_STMT_FUNCTION { lex' AlexRawToken_STMT_FUNCTION }

-- *************
-- *           *
-- * operators *
-- *           *
-- *************

@KW_OP_LT       { lex' AlexRawToken_OP_LT       }
@KW_OP_EQ       { lex' AlexRawToken_OP_EQ       }
@KW_OP_ASSIGN   { lex' AlexRawToken_OP_ASSIGN   }
@KW_OP_TIMES    { lex' AlexRawToken_OP_TIMES    }

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
     | AlexRawToken_ID String       -- ^ including constant strings

     -- ***************
     -- *             *
     -- * parentheses *
     -- *             *
     -- ***************

     | AlexRawToken_LPAREN          -- ^ Parentheses __(__
     | AlexRawToken_RPAREN          -- ^ Parentheses __)__
     | AlexRawToken_LBRACK          -- ^ Parentheses __[__
     | AlexRawToken_RBRACK          -- ^ Parentheses __]__
     | AlexRawToken_LBRACE          -- ^ Parentheses __{__
     | AlexRawToken_RBRACE          -- ^ Parentheses __}__
 
     | AlexRawToken_KWID            -- ^ Reserved Keyword
     | AlexRawToken_CTX             -- ^ Reserved Keyword
     | AlexRawToken_OR              -- ^ Reserved Keyword
     | AlexRawToken_EQ              -- ^ Reserved Keyword
     | AlexRawToken_NOT             -- ^ Reserved Keyword
     | AlexRawToken_ADD             -- ^ Reserved Keyword
     | AlexRawToken_END             -- ^ Reserved Keyword
     | AlexRawToken_RAW             -- ^ Reserved Keyword
     | AlexRawToken_LOC             -- ^ Reserved Keyword
     | AlexRawToken_ARG             -- ^ Reserved Keyword
     | AlexRawToken_VAR             -- ^ Reserved Keyword
     | AlexRawToken_TEST            -- ^ Reserved Keyword
     | AlexRawToken_NULL            -- ^ Reserved Keyword
     | AlexRawToken_TAIL            -- ^ Reserved Keyword
     | AlexRawToken_KIND            -- ^ Reserved Keyword
     | AlexRawToken_COL             -- ^ Reserved Keyword
     | AlexRawToken_ECOL            -- ^ Reserved Keyword
     | AlexRawToken_LINE            -- ^ Reserved Keyword
     | AlexRawToken_LOAD            -- ^ Reserved Keyword
     | AlexRawToken_STORE           -- ^ Reserved Keyword
     | AlexRawToken_TARGET          -- ^ Reserved Keyword
     | AlexRawToken_TARGETS         -- ^ Reserved Keyword
     | AlexRawToken_DEFAULTS        -- ^ Reserved Keyword
     | AlexRawToken_KW_DEFAULTS     -- ^ Reserved Keyword
     | AlexRawToken_ELINE           -- ^ Reserved Keyword
     | AlexRawToken_TRUE            -- ^ Reserved Keyword
     | AlexRawToken_FUNC            -- ^ Reserved Keyword
     | AlexRawToken_ATTR            -- ^ Reserved Keyword
     | AlexRawToken_ATTR2           -- ^ Reserved Keyword
     | AlexRawToken_ARGS            -- ^ Reserved Keyword
     | AlexRawToken_ARGS2           -- ^ Reserved Keyword
     | AlexRawToken_ARGS3           -- ^ Reserved Keyword
     | AlexRawToken_ARGS4           -- ^ Reserved Keyword
     | AlexRawToken_EXPR            -- ^ Reserved Keyword
     | AlexRawToken_MAME            -- ^ Reserved Keyword
     | AlexRawToken_TYPE            -- ^ Reserved Keyword
     | AlexRawToken_LEFT            -- ^ Reserved Keyword
     | AlexRawToken_LOOP            -- ^ Reserved Keyword
     | AlexRawToken_INIT            -- ^ Reserved Keyword
     | AlexRawToken_COND            -- ^ Reserved Keyword
     | AlexRawToken_BODY            -- ^ Reserved Keyword
     | AlexRawToken_LEVEL           -- ^ Reserved Keyword
     | AlexRawToken_NAME            -- ^ Reserved Keyword
     | AlexRawToken_NAME2           -- ^ Reserved Keyword
     | AlexRawToken_ASNAME          -- ^ Reserved Keyword
     | AlexRawToken_NAMES           -- ^ Reserved Keyword
     | AlexRawToken_ALIAS           -- ^ Reserved Keyword
     | AlexRawToken_ORELSE          -- ^ Reserved Keyword
     | AlexRawToken_KEYWORD         -- ^ Reserved Keyword
     | AlexRawToken_KEYWORDS        -- ^ Reserved Keyword
     | AlexRawToken_IMPORT          -- ^ Reserved Keyword
     | AlexRawToken_FSTRING         -- ^ Reserved Keyword
     | AlexRawToken_IMPORTF         -- ^ Reserved Keyword
     | AlexRawToken_CONVERSION      -- ^ Reserved Keyword
     | AlexRawToken_FORMATTED_VAL   -- ^ Reserved Keyword
     | AlexRawToken_ASSIGN          -- ^ Reserved Keyword
     | AlexRawToken_ASSIGN2         -- ^ Reserved Keyword
     | AlexRawToken_MODULE          -- ^ Reserved Keyword
     | AlexRawToken_MODULE2         -- ^ Reserved Keyword
     | AlexRawToken_START           -- ^ Reserved Keyword
     | AlexRawToken_COOKED          -- ^ Reserved Keyword
     | AlexRawToken_UPDATE          -- ^ Reserved Keyword
     | AlexRawToken_QUASIS          -- ^ Reserved Keyword
     | AlexRawToken_FALSE           -- ^ Reserved Keyword
     | AlexRawToken_ITEMS           -- ^ Reserved Keyword
     | AlexRawToken_WITH            -- ^ Reserved Keyword
     | AlexRawToken_WITH2           -- ^ Reserved Keyword
     | AlexRawToken_CTX_MANAGER     -- ^ Reserved Keyword
     | AlexRawToken_EXPRS           -- ^ Reserved Keyword
     | AlexRawToken_VALUE           -- ^ Reserved Keyword
     | AlexRawToken_VALUES          -- ^ Reserved Keyword
     | AlexRawToken_RIGHT           -- ^ Reserved Keyword
     | AlexRawToken_STMTS           -- ^ Reserved Keyword
     | AlexRawToken_ARRAY           -- ^ Reserved Keyword
     | AlexRawToken_PARAM           -- ^ Reserved Keyword
     | AlexRawToken_OBJECT          -- ^ Reserved Keyword
     | AlexRawToken_PREFIX          -- ^ Reserved Keyword
     | AlexRawToken_PARAMS          -- ^ Reserved Keyword
     | AlexRawToken_COLUMN          -- ^ Reserved Keyword
     | AlexRawToken_LITERAL         -- ^ Reserved Keyword
     | AlexRawToken_PROGRAM         -- ^ Reserved Keyword
     | AlexRawToken_PROPERTY        -- ^ Reserved Keyword
     | AlexRawToken_COMPUTED        -- ^ Reserved Keyword
     | AlexRawToken_DECORATORS      -- ^ Reserved Keyword
     | AlexRawToken_TYPE_PARAMS     -- ^ Reserved Keyword
     | AlexRawToken_COMPARE         -- ^ Reserved Keyword
     | AlexRawToken_COMPARE3        -- ^ Reserved Keyword
     | AlexRawToken_COMPARE2        -- ^ Reserved Keyword
     | AlexRawToken_OPERATOR        -- ^ Reserved Keyword
     | AlexRawToken_OPERATOR2       -- ^ Reserved Keyword
     | AlexRawToken_OPERAND         -- ^ Reserved Keyword
     | AlexRawToken_ALTERNATE       -- ^ Reserved Keyword
     | AlexRawToken_CONSEQUENT      -- ^ Reserved Keyword
     | AlexRawToken_ARGUMENT        -- ^ Reserved Keyword
     | AlexRawToken_ARGUMENTS       -- ^ Reserved Keyword
     | AlexRawToken_CALL            -- ^ Reserved Keyword
     | AlexRawToken_CALLEE          -- ^ Reserved Keyword
     | AlexRawToken_ASYNC           -- ^ Reserved Keyword
     | AlexRawToken_EXPRESSION      -- ^ Reserved Keyword
     | AlexRawToken_EXPRESSIONS     -- ^ Reserved Keyword
     | AlexRawToken_DECLARATIONS    -- ^ Reserved Keyword
     | AlexRawToken_SRC_TYPE        -- ^ Reserved Keyword
     | AlexRawToken_GENERATOR       -- ^ Reserved Keyword
     | AlexRawToken_STMT_ECHO       -- ^ Reserved Keyword
     | AlexRawToken_EXPR_VAR        -- ^ Reserved Keyword
     | AlexRawToken_STMT_EXPR       -- ^ Reserved Keyword
     | AlexRawToken_SCALAR_INT      -- ^ Reserved Keyword
     | AlexRawToken_IDENTIFIER      -- ^ Reserved Keyword
     | AlexRawToken_RETURN_TYPE     -- ^ Reserved Keyword
     | AlexRawToken_TEMPLATE_LI     -- ^ Reserved Keyword
     | AlexRawToken_TEMPLATE_EL     -- ^ Reserved Keyword
     | AlexRawToken_STMT_FUNCTION   -- ^ Reserved Keyword
     | AlexRawToken_FUNCTION_DEC    -- ^ Reserved Keyword
     | AlexRawToken_EXPR_CONST      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_UNOP       -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP_PLUS -- ^ Reserved Keyword
     | AlexRawToken_VAR_DECLARATION -- ^ Reserved Keyword
     | AlexRawToken_VAR_DECLARATOR  -- ^ Reserved Keyword

     -- *********
     -- *       *
     -- * other *
     -- *       *
     -- *********

     | AlexRawToken_QUOTED_INT      -- ^ Reserved Keyword
     | AlexRawToken_QUOTED_STR      -- ^ Reserved Keyword
     | AlexRawToken_QUOTED_BOOL     -- ^ Reserved Keyword

     -- *************
     -- *           *
     -- * operators *
     -- *           *
     -- *************

     | AlexRawToken_OP_LT           -- ^ Reserved Keyword
     | AlexRawToken_OP_EQ           -- ^ Reserved Keyword
     | AlexRawToken_OP_ASSIGN       -- ^ Reserved Keyword
     | AlexRawToken_OP_TIMES        -- ^ Reserved Keyword
     | AlexRawToken_OP_PLUSPLUS     -- ^ Reserved Keyword

     -- ***************
     -- *             *
     -- * expressions *
     -- *             *
     -- ***************

     | AlexRawToken_EXPR_NEW        -- ^ Reserved Keyword
     | AlexRawToken_EXPR_CALL       -- ^ Reserved Keyword
     | AlexRawToken_EXPR_BINOP      -- ^ Reserved Keyword
     | AlexRawToken_EXPR_MEMBER     -- ^ Reserved Keyword
     | AlexRawToken_EXPR_UPDATE     -- ^ Reserved Keyword
     | AlexRawToken_EXPR_ASSIGN     -- ^ Reserved Keyword
     | AlexRawToken_EXPR_LAMBDA     -- ^ Reserved Keyword

     -- **************
     -- *            *
     -- * statements *
     -- *            *
     -- **************

     | AlexRawToken_STMT_IF         -- ^ Reserved Keyword
     | AlexRawToken_STMT_FOR        -- ^ Reserved Keyword
     | AlexRawToken_STMT_BLOCK      -- ^ Reserved Keyword
     | AlexRawToken_STMT_RETURN     -- ^ Reserved Keyword
     | AlexRawToken_STMT_EXP        -- ^ Reserved Keyword

     -- ***************
     -- *             *
     -- * punctuation *
     -- *             *
     -- ***************

     | AlexRawToken_COLON           -- ^ Punctuation __:__
     | AlexRawToken_COMMA           -- ^ Punctuation __,__
     | AlexRawToken_HYPHEN          -- ^ Punctuation __-__

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
                lineStart = l,
                lineEnd = l,
                colStart = c,
                colEnd = c,
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
                lineStart = l,
                lineEnd = l,
                colStart = c,
                colEnd = c+i,
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

-- ************
-- *          *
-- * runAlex' *
-- *          *
-- ************
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)
}

