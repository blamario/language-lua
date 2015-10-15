{
{-# LANGUAGE RankNTypes #-}

module Language.Lua.Annotated.Parser
  ( parseText
  , parseNamedText
  , parseFile
  , Parser
  , chunk
  , exp
  , stat
  ) where

import           Control.Monad (liftM,ap)
import           Prelude hiding (LT,GT,EQ,exp)
import           Data.Text (Text)
import qualified Data.Text.IO as Text

import           Language.Lua.Token           (LToken(..))
import           Language.Lua.Annotated.Lexer (SourcePos(..), Lexeme(..), llexNamed)
import           Language.Lua.Annotated.Syntax

}

%tokentype    { Lexeme SourcePos }
%token
'+'           { Lexeme { ltokToken = LTokPlus      } }
'-'           { Lexeme { ltokToken = LTokMinus     } }
'*'           { Lexeme { ltokToken = LTokStar      } }
'/'           { Lexeme { ltokToken = LTokSlash     } }
'//'          { Lexeme { ltokToken = LTokDSlash    } }
'%'           { Lexeme { ltokToken = LTokPercent   } }
'^'           { Lexeme { ltokToken = LTokExp       } }
'#'           { Lexeme { ltokToken = LTokSh        } }
'=='          { Lexeme { ltokToken = LTokEqual     } }
'~='          { Lexeme { ltokToken = LTokNotequal  } }
'<='          { Lexeme { ltokToken = LTokLEq       } }
'>='          { Lexeme { ltokToken = LTokGEq       } }
'<'           { Lexeme { ltokToken = LTokLT        } }
'>'           { Lexeme { ltokToken = LTokGT        } }
'&'           { Lexeme { ltokToken = LTokAmpersand } }
'~'           { Lexeme { ltokToken = LTokTilde     } }
'|'           { Lexeme { ltokToken = LTokPipe      } }
'>>'          { Lexeme { ltokToken = LTokDGT       } }
'<<'          { Lexeme { ltokToken = LTokDLT       } }
'='           { Lexeme { ltokToken = LTokAssign    } }
'('           { Lexeme { ltokToken = LTokLParen    } }
')'           { Lexeme { ltokToken = LTokRParen    } }
'{'           { Lexeme { ltokToken = LTokLBrace    } }
'}'           { Lexeme { ltokToken = LTokRBrace    } }
'['           { Lexeme { ltokToken = LTokLBracket  } }
']'           { Lexeme { ltokToken = LTokRBracket  } }
'::'          { Lexeme { ltokToken = LTokDColon    } }
';'           { Lexeme { ltokToken = LTokSemic     } }
':'           { Lexeme { ltokToken = LTokColon     } }
','           { Lexeme { ltokToken = LTokComma     } }
'.'           { Lexeme { ltokToken = LTokDot       } }
'..'          { Lexeme { ltokToken = LTokDDot      } }
'...'         { Lexeme { ltokToken = LTokEllipsis  } }
'and'         { Lexeme { ltokToken = LTokAnd       } }
'break'       { Lexeme { ltokToken = LTokBreak     } }
'do'          { Lexeme { ltokToken = LTokDo        } }
'else'        { Lexeme { ltokToken = LTokElse      } }
'elseif'      { Lexeme { ltokToken = LTokElseIf    } }
'end'         { Lexeme { ltokToken = LTokEnd       } }
'false'       { Lexeme { ltokToken = LTokFalse     } }
'for'         { Lexeme { ltokToken = LTokFor       } }
'function'    { Lexeme { ltokToken = LTokFunction  } }
'goto'        { Lexeme { ltokToken = LTokGoto      } }
'if'          { Lexeme { ltokToken = LTokIf        } }
'in'          { Lexeme { ltokToken = LTokIn        } }
'local'       { Lexeme { ltokToken = LTokLocal     } }
'nil'         { Lexeme { ltokToken = LTokNil       } }
'not'         { Lexeme { ltokToken = LTokNot       } }
'or'          { Lexeme { ltokToken = LTokOr        } }
'repeat'      { Lexeme { ltokToken = LTokRepeat    } }
'return'      { Lexeme { ltokToken = LTokReturn    } }
'then'        { Lexeme { ltokToken = LTokThen      } }
'true'        { Lexeme { ltokToken = LTokTrue      } }
'until'       { Lexeme { ltokToken = LTokUntil     } }
'while'       { Lexeme { ltokToken = LTokWhile     } }
numeral       { Lexeme { ltokToken = LTokNum       } }
literalString { Lexeme { ltokToken = LTokSLit      } }
ident         { Lexeme { ltokToken = LTokIdent     } }

%monad { Parser }
%lexer { (>>=) lexerP } { Lexeme { ltokToken = LTokEof } }
%error { errorP }

-- local a=b(nil)() is one statement
-- local a=b;(nil)() is two statements
-- therefore EXP '('
-- f()(nil)() is one statement
-- f();(nil)() is two statements
-- therefore STAT PREFIX
%nonassoc STAT    EXP
%nonassoc PREFIX  '('

%left 'or'
%left 'and'
%left '<' '<=' '>' '>=' '==' '~='
%left '|'
%left '~'
%left '&'
%left '<<' '>>'
%right '..'
%left '+' '-'
%left '*' '/' '//' '%'
%nonassoc 'not' '#' NEG COMPLEMENT
%right '^'

%name chunk block
%name exp exp
%name stat stat

%%

opt(p)
  : p { Just $1 }
  |   { Nothing }

many   (p) : revMany(p)   { reverse $1 }
revMany(p) : revMany(p) p { $2 : $1    }
           |              { []         }

sepBy    (p,sep) : sepBy1(p,sep)          { $1         }
                 |                        { []         }
sepBy1   (p,sep) : revSepBy1(p,sep)       { reverse $1 }
revSepBy1(p,sep) : p                      { [$1]       }
                 | revSepBy1(p,sep) sep p { $3 : $1    }

block ::                    { Block SourcePos              }
  : many(stat) opt(retstat) { Block (blockAnn $1 $2) $1 $2 }

retstat ::                           { [Exp SourcePos] }
  : 'return' sepBy(exp,',') opt(';') { $2              }

stat ::                                                   { Stat SourcePos                }
  : ';'                                                   { sp $1 EmptyStat               }
  | varlist '=' explist                                   { sp (head $1) Assign $1 $3     }
  | functioncall %prec STAT                               { sp $1 FunCall $1              }
  | '::' name '::'                                        { sp $1 Label $2                }
  | 'break'                                               { sp $1 Break                   }
  | 'goto' name                                           { sp $1 Goto $2                 }
  | 'local' namelist opt(assign)                          { sp $1 LocalAssign $2 $3       }

  ------- block structures -------------------------------
  | 'function' funcname funcbody 'end'                    { sp $1 FunAssign $2 $3         }
  | 'local' 'function' name funcbody 'end'                { sp $1 LocalFunAssign $3 $4    }
  | 'repeat' block 'until' exp                            { sp $1 Repeat $2 $4            }
  | 'do' block 'end'                                      { sp $1 Do $2                   }
  | 'while' exp 'do' block 'end'                          { sp $1 While $2 $4             }
  | 'if' exp 'then' block many(elseif) opt(else) 'end'    { sp $1 If (($2,$4):$5) $6      }
  | 'for' name '=' exp ',' exp opt(step) 'do' block 'end' { sp $1 ForRange $2 $4 $6 $7 $9 }
  | 'for' namelist 'in' explist 'do' block 'end'          { sp $1 ForIn $2 $4 $6          }

  ------- error cases for block structures ---------------
  | 'function' funcname funcbody error                    {% noEndP $1 }
  | 'local' 'function' name funcbody error                {% noEndP $1 }
  | 'repeat' block error                                  {% noEndP $1 }
  | 'do' block error                                      {% noEndP $1 }
  | 'while' exp 'do' block error                          {% noEndP $1 }
  | 'if' exp 'then' block many(elseif) opt(else) error    {% noEndP $1 }
  | 'for' name '=' exp ',' exp opt(step) 'do' block error {% noEndP $1 }
  | 'for' namelist 'in' explist 'do' block error          {% noEndP $1 }

elseif : 'elseif' exp 'then' block { ($2,$4) }
else   : 'else' block { $2 }
step   : ',' exp { $2 }
assign : '=' explist { $2 }

varlist  : sepBy1(var,  ',') { $1 }
explist  : sepBy1(exp,  ',') { $1 }
namelist : sepBy1(name, ',') { $1 }

prefixexp ::                  { PrefixExp SourcePos }
  : var                       { sp $1 PEVar $1      }
  | functioncall %prec PREFIX { sp $1 PEFunCall $1  }
  | '(' exp ')'               { sp $1 Paren $2      }

functioncall ::               { FunCall SourcePos         }
  : prefixexp            args { sp $1 NormalFunCall $1 $2 }
  | prefixexp methodname args { sp $1 MethodCall $1 $2 $3 }

funcname ::                               { FunName SourcePos      }
  : name many(dottedname) opt(methodname) { sp $1 FunName $1 $2 $3 }

dottedname : '.' name  { $2 }
methodname : ':' name  { $2 }

var ::                    { Var SourcePos          }
  : name                  { sp $1 VarName $1       }
  | prefixexp '[' exp ']' { sp $1 Select $1 $3     }
  | prefixexp '.' name    { sp $1 SelectName $1 $3 }

exp ::                     { Exp SourcePos               }
  : 'nil'                  { sp $1 Nil                   }
  | 'false'                { sp $1 Bool False            }
  | 'true'                 { sp $1 Bool True             }
  | numeral                { sp $1 Number (ltokText $1) }
  | literalString          { sp $1 String (ltokText $1) }
  | '...'                  { sp $1 Vararg                }
  | functiondef            { sp $1 EFunDef $1            }
  | prefixexp %prec EXP    { sp $1 PrefixExp $1          }
  | tableconstructor       { sp $1 TableConst $1         }

  | exp '+' exp   { sp $1 Binop (sp $2 Add   ) $1 $3 }
  | exp '-' exp   { sp $1 Binop (sp $2 Sub   ) $1 $3 }
  | exp '*' exp   { sp $1 Binop (sp $2 Mul   ) $1 $3 }
  | exp '/' exp   { sp $1 Binop (sp $2 Div   ) $1 $3 }
  | exp '//' exp  { sp $1 Binop (sp $2 IDiv  ) $1 $3 }
  | exp '^' exp   { sp $1 Binop (sp $2 Exp   ) $1 $3 }
  | exp '%' exp   { sp $1 Binop (sp $2 Mod   ) $1 $3 }
  | exp '..' exp  { sp $1 Binop (sp $2 Concat) $1 $3 }
  | exp '<'  exp  { sp $1 Binop (sp $2 LT    ) $1 $3 }
  | exp '<=' exp  { sp $1 Binop (sp $2 LTE   ) $1 $3 }
  | exp '>'  exp  { sp $1 Binop (sp $2 GT    ) $1 $3 }
  | exp '>=' exp  { sp $1 Binop (sp $2 GTE   ) $1 $3 }
  | exp '==' exp  { sp $1 Binop (sp $2 EQ    ) $1 $3 }
  | exp '~=' exp  { sp $1 Binop (sp $2 NEQ   ) $1 $3 }
  | exp 'and' exp { sp $1 Binop (sp $2 And   ) $1 $3 }
  | exp 'or'  exp { sp $1 Binop (sp $2 Or    ) $1 $3 }
  | exp '&' exp   { sp $1 Binop (sp $2 BAnd  ) $1 $3 }
  | exp '|' exp   { sp $1 Binop (sp $2 BOr   ) $1 $3 }
  | exp '~' exp   { sp $1 Binop (sp $2 BXor  ) $1 $3 }
  | exp '<<' exp  { sp $1 Binop (sp $2 ShiftL) $1 $3 }
  | exp '>>' exp  { sp $1 Binop (sp $2 ShiftR) $1 $3 }

  | '-' exp %prec NEG        { sp $1 Unop (sp $1 Neg)        $2 }
  | '~' exp %prec COMPLEMENT { sp $1 Unop (sp $1 Complement) $2 }
  | 'not' exp                { sp $1 Unop (sp $1 Not)        $2 }
  | '#'  exp                 { sp $1 Unop (sp $1 Len)        $2 }

args ::                    { FunArg SourcePos               }
  : '(' sepBy(exp,',') ')' { sp $1 Args $2                  }
  | tableconstructor       { sp $1 TableArg $1              }
  | literalString          { sp $1 StringArg (ltokText $1) }

functiondef ::          { FunDef SourcePos }
  : 'function' funcbody 'end' { sp $1 FunDef $2  }
  | 'function' funcbody error {% noEndP $1 }

funcbody ::                     { FunBody SourcePos                  }
  : '(' parlist ')' block { sp $1 FunBody (fst $2) (snd $2) $4 }

parlist ::              { ([Name SourcePos],Maybe SourcePos) }
  : parnames1 ',' '...' { (reverse $1,Just (ltokPos $3) )     }
  | parnames1           { (reverse $1,Nothing)               }
  | '...'               { ([],Just (ltokPos $1))              }
  |                     { ([],Nothing)                       }

parnames1 ::           { [Name SourcePos] }
  : name               { [$1]             }
  | parnames1 ',' name { $3 : $1          }

tableconstructor ::                 { Table SourcePos          }
  : '{'                         '}' { sp $1 Table []           }
  | '{' fieldlist opt(fieldsep) '}' { sp $1 Table (reverse $2) }

fieldlist ::                  { [TableField SourcePos] }
  : fieldlist fieldsep field  { $3 : $1                }
  | field                     { [$1]                   }

fieldsep :: { () }
  : ','     { () }
  | ';'     { () }

field ::                { TableField SourcePos   }
  : '[' exp ']' '=' exp { sp $1 ExpField $2 $5   }
  | name        '=' exp { sp $1 NamedField $1 $3 }
  |                 exp { sp $1 Field $1         }

name ::   { Name SourcePos            }
  : ident { sp $1 Name (ltokText $1) }

{

data Parser a = Parser
  { runP :: forall r. (SourcePos -> String -> r) ->
                      (a -> [Lexeme SourcePos] -> r) ->
                      [Lexeme SourcePos] ->
                      r }

runParser :: Parser a -> [Lexeme SourcePos] -> Either (String,SourcePos) a
runParser p = runP p (\pos e -> Left (e,pos)) (\x _ -> Right x)

instance Functor     Parser where fmap    = liftM
instance Applicative Parser where pure x  = Parser $ \_ k -> k x
                                  (<*>)   = ap
instance Monad       Parser where return  = pure
                                  m >>= f = Parser $ \e k ->
                                            runP m e $ \a ->
                                            runP (f a) e k

errorP :: Lexeme SourcePos -> Parser a
errorP Lexeme { ltokPos = pos, ltokToken = t } =
  Parser $ \e _ _ -> e pos ("unexpected " ++ show t)

noEndP :: Lexeme SourcePos -> Parser a
noEndP Lexeme { ltokPos = pos, ltokToken = t } =
  Parser $ \e _ _ -> e pos ("unterminated " ++ show t)

lexerP :: Parser (Lexeme SourcePos)
lexerP = Parser $ \ _ k (l:ls) -> k l ls

sp :: Annotated p => p SourcePos -> (SourcePos -> a) -> a
sp x f = f (ann x)

blockAnn :: [Stat SourcePos] -> Maybe [Exp SourcePos] -> SourcePos
blockAnn xs mbys =
  case map ann xs ++ maybe [] (map ann) mbys of
    a : _ -> a
    _     -> SourcePos "" 0 1 1

showPos :: SourcePos -> String
showPos p = "line: " ++ show (sourcePosLine p) ++
            " column: " ++ show (sourcePosColumn p)

-- | Runs Lua lexer before parsing. Use @parseNamedText stat "name"@ to parse
-- statements, and @parseText exp "name"@ to parse expressions.
parseNamedText ::
  Parser a ->
  String {- ^ name -} ->
  Text {- ^ chunk -} ->
  Either (String, SourcePos) a
parseNamedText p n xs =
  case runParser p (llexNamed n xs) of
    Left (msg,pos) ->
      Left ("parser error: " ++ show msg, pos)
    Right chunk -> Right chunk

-- | Runs Lua lexer before parsing. Use @parseText stat@ to parse
-- statements, and @parseText exp@ to parse expressions.
parseText ::
  Parser a ->
  Text {- ^ chunk -} ->
  Either (String, SourcePos) a
parseText p = parseNamedText p "=<string>"

-- | Parse a Lua file. You can use @parseText chunk@ to parse a file from a string.
parseFile :: FilePath -> IO (Either (String, SourcePos) (Block SourcePos))
parseFile fp = fmap (parseNamedText chunk fp) (Text.readFile fp)

}
