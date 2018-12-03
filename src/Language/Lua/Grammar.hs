{-# Language OverloadedStrings, RecordWildCards, ScopedTypeVariables, TemplateHaskell #-}
module Language.Lua.Grammar where

import Control.Applicative
import Control.Monad (guard, void)
import Data.Char (isAlphaNum, isDigit, isHexDigit, isLetter, isSpace)
import Data.Functor.Classes (Eq1, Show1, eq1, showsPrec1)
import Data.Monoid ((<>))
import Data.Monoid.Null (MonoidNull)
import Data.Monoid.Textual (TextualMonoid)
import qualified Data.Monoid.Textual as Textual
import Data.Set (Set, fromList, notMember)
import Data.String (fromString)
import Data.Text (Text)

import qualified Rank2.TH
import Text.Grampa hiding (identifier, keyword)
import Text.Grampa.ContextFree.LeftRecursive (Parser, longest, peg, terminalPEG)

import Text.Parser.Char (char, hexDigit, oneOf)
import Text.Parser.Combinators (choice, count, skipMany, try)
import Text.Parser.Expression (Assoc(..), Operator(..), buildExpressionParser)

import Language.Lua.Syntax

import Prelude hiding (EQ, GT, LT, exp, exponent)

data LuaGrammar f = LuaGrammar{
   chunk :: f Block,
   block :: f Block,
   stat :: f Stat,
   retstat :: f [Exp],
   label :: f Name,
   funcname :: f FunName,
   varlist :: f [Var],
   var :: f Var,
   namelist :: f [Name],
   explist :: f [Exp],
   exp :: f Exp,
   primaryexp :: f Exp,
   secondaryexp :: f Exp,
   prefixexp :: f PrefixExp,
   functioncall :: f FunCall,
   args :: f FunArg,
   functiondef :: f FunBody,
   funcbody :: f FunBody,
   parlist :: f ([Name], Bool),
   tableconstructor :: f [TableField],
   fieldlist :: f [TableField],
   field :: f TableField,
   fieldsep :: f ()}

$(Rank2.TH.deriveAll ''LuaGrammar)

instance Show1 f => Show (LuaGrammar f) where
   showsPrec prec g rest = "LuaGrammar{" ++
      "  chunk = " ++ showsPrec1 prec (chunk g) ",\n" ++
      "  block = " ++ showsPrec1 prec (block g) ",\n" ++
      "  stat = " ++ showsPrec1 prec (stat g) ",\n" ++
      "  retstat = " ++ showsPrec1 prec (retstat g) ",\n" ++
      "  label = " ++ showsPrec1 prec (label g) ",\n" ++
      "  funcname = " ++ showsPrec1 prec (funcname g) ",\n" ++
      "  varlist = " ++ showsPrec1 prec (varlist g) ",\n" ++
      "  var = " ++ showsPrec1 prec (var g) ",\n" ++
      "  namelist = " ++ showsPrec1 prec (namelist g) ",\n" ++
      "  explist = " ++ showsPrec1 prec (explist g) ",\n" ++
      "  exp = " ++ showsPrec1 prec (exp g) ",\n" ++
      "  primaryexp = " ++ showsPrec1 prec (primaryexp g) ",\n" ++
      "  secondaryexp = " ++ showsPrec1 prec (secondaryexp g) ",\n" ++
      "  prefixexp = " ++ showsPrec1 prec (prefixexp g) ",\n" ++
      "  functioncall = " ++ showsPrec1 prec (functioncall g) ",\n" ++
      "  args = " ++ showsPrec1 prec (args g) ",\n" ++
      "  functiondef = " ++ showsPrec1 prec (functiondef g) ",\n" ++
      "  funcbody = " ++ showsPrec1 prec (funcbody g) ",\n" ++
      "  parlist = " ++ showsPrec1 prec (parlist g) ",\n" ++
      "  tableconstructor = " ++ showsPrec1 prec (tableconstructor g) ",\n" ++
      "  fieldlist = " ++ showsPrec1 prec (fieldlist g) ",\n" ++
      "  field = " ++ showsPrec1 prec (field g) ",\n" ++
      "  fieldsep = " ++ showsPrec1 prec (fieldsep g) ("}\n" ++ rest)

instance Eq1 f => Eq (LuaGrammar f) where
   g1 == g2 = eq1 (chunk g1) (chunk g2)
              && eq1 (block g1) (block g2)
              && eq1 (stat g1) (stat g2)
              && eq1 (retstat g1) (retstat g2)
              && eq1 (label g1) (label g2)
              && eq1 (funcname g1) (funcname g2)
              && eq1 (varlist g1) (varlist g2)
              && eq1 (var g1) (var g2)
              && eq1 (namelist g1) (namelist g2)
              && eq1 (explist g1) (explist g2)
              && eq1 (exp g1) (exp g2)
              && eq1 (primaryexp g1) (primaryexp g2)
              && eq1 (secondaryexp g1) (secondaryexp g2)
              && eq1 (prefixexp g1) (prefixexp g2)
              && eq1 (functioncall g1) (functioncall g2)
              && eq1 (args g1) (args g2)
              && eq1 (functiondef g1) (functiondef g2)
              && eq1 (funcbody g1) (funcbody g2)
              && eq1 (parlist g1) (parlist g2)
              && eq1 (tableconstructor g1) (tableconstructor g2)
              && eq1 (fieldlist g1) (fieldlist g2)
              && eq1 (field g1) (field g2)
              && eq1 (fieldsep g1) (fieldsep g2)


moptional :: (Monoid x, Alternative p) => p x -> p x
moptional p = p <|> pure mempty

sepBy1 :: Alternative p => p x -> p sep -> p [x]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)
{-# SPECIALIZE sepBy1 :: Parser LuaGrammar Text x -> Parser LuaGrammar Text sep -> Parser LuaGrammar Text [x] #-}

upto :: (TextualMonoid s, MonoidParsing p) => Int -> (Char -> Bool) -> p s s
upto n0 predicate = scanChars n0 (\n c-> if n > 0 && predicate c then Just (pred n) else Nothing)

-- Section 3.1
reservedKeywords  :: Set Text
reservedKeywords = fromList ["and", "break", "do", "else", "elseif", "end",
                             "false", "for", "function", "goto", "if", "in",
                             "local", "nil", "not", "or", "repeat", "return",
                             "then", "true", "until", "while"]

luaGrammar :: -- (Eq t, Show t, TextualMonoid t) => 
              Grammar LuaGrammar Parser Text
luaGrammar = fixGrammar grammar

grammar :: -- (Eq t, Show t, TextualMonoid t) =>
           GrammarBuilder LuaGrammar LuaGrammar Parser Text
grammar LuaGrammar{..} = LuaGrammar{
   chunk = optional (string "#" *> takeCharsWhile (/= '\n') *> (void (string "\n") <|> endOfInput))
           *> ignorable *> block <* endOfInput,
   block = Block <$> peg (many $ longest stat) <*> optional retstat,
   stat = peg $ choice $ longest <$>
          [EmptyStat <$ symbol ";",
           Assign <$> varlist <* symbol "=" <*> explist,
           FunCall <$> functioncall,
           Label <$> label,
           Break <$ keyword "break",
           Goto <$ keyword "goto" <*> name,
           Do <$ keyword "do" <*> block <* keyword "end",
           While <$ keyword "while" <*> exp <* keyword "do" <*> block <* keyword "end",
           Repeat <$ keyword "repeat" <*> block <* keyword "until" <*> exp,
           If <$ keyword "if" <*> ((:) <$> ((,) <$> exp <* keyword "then" <*> block) 
                                       <*> many ((,) <$ keyword "elseif" <*> exp <* keyword "then" <*> block))
                   <*> optional (keyword "else" *> block) <* keyword "end",
           ForRange <$ keyword "for" <*> name <* symbol "="
                    <*> exp <* symbol "," <*> exp <*> optional (symbol "," *> exp)
                    <* keyword "do" <*> block <* keyword "end",
           ForIn <$ keyword "for" <*> namelist <* keyword "in" <*> explist 
                      <* keyword "do" <*> block <* keyword "end",
           FunAssign <$ keyword "function" <*> funcname <*> funcbody,
           LocalFunAssign <$ keyword "local" <* keyword "function" <*> name <*> funcbody,
           LocalAssign <$ keyword "local" <*> namelist <*> optional (symbol "=" *> explist)],

   retstat = keyword "return" *> moptional explist <* optional (symbol ";"),
   label = symbol "::" *> name <* symbol "::",
   funcname = FunName <$> name <*> many (symbol "." *> name) <*> optional (symbol ":" *> name),
   varlist = peg (sepBy1 (longest var) (longest $ symbol ",")),
   var = VarName <$> name <|>
         Select <$> prefixexp <* symbol "[" <*> exp <* symbol "]" <|> 
         SelectName <$> prefixexp <* symbol "." <*> name,
   
   namelist = peg (sepBy1 (longest name) (longest $ symbol ",")),
   explist = peg (sepBy1 (longest exp) (longest $ symbol ",")),

   -- Operator precedence from 3.4.8
   exp = let binary op = Infix (Binop <$> longest op)
             operators = [
--                [binary (Exp <$ symbol "^") AssocRight],
--                [Prefix (Unop <$> unop)],
                [binary (Mul <$ symbol "*") AssocLeft,
                 binary (IDiv <$ symbol "//") AssocLeft,
                 binary (Div <$ symbol "/") AssocLeft,
                 binary (Mod <$ symbol "%") AssocLeft],
                [binary (Add <$ symbol "+") AssocLeft,
                 binary (Sub <$ string "-" <* notSatisfyChar (== '-') <* ignorable) AssocLeft], -- avoid ambiguity with comment
                [binary (Concat <$ symbol "..") AssocRight],
                [binary (ShiftL <$ symbol "<<") AssocLeft,
                 binary (ShiftR <$ symbol ">>") AssocLeft],
                [binary (BAnd <$ symbol "&") AssocLeft],
                [binary (BXor <$ symbol "~") AssocLeft],
                [binary (BOr <$ symbol "|") AssocLeft],
                [binary (LTE <$ symbol "<=") AssocLeft,
                 binary (GTE <$ symbol ">=") AssocLeft,
                 binary (LT <$ symbol "<") AssocLeft,
                 binary (GT <$ symbol ">") AssocLeft,
                 binary (NEQ <$ symbol "~=") AssocLeft,
                 binary (EQ <$ symbol "==") AssocLeft],
                [binary (And <$ keyword "and") AssocLeft],
                [binary (Or <$ keyword "or") AssocLeft]]
         in peg (buildExpressionParser operators $ longest secondaryexp),
   secondaryexp =
      let unop =
             terminalPEG $ choice
             [Neg        <$ string "-" <* notSatisfyChar (== '-'),   -- eliminate ambiguity
              Not        <$ string "not" <* notSatisfyChar isAlphaNum,
              Len        <$ string "#",
              Complement <$ string "~"] <* ignorablePEG
      in Unop <$> unop <*> secondaryexp <|>
--                 primaryexp <**> (foldr (.) id <$> many (flip <$> (Binop Exp <$ symbol "^") <*> secondaryexp)),
         flip Binop <$> primaryexp <*> (Exp <$ symbol "^") <*> secondaryexp <|>
         primaryexp,
   primaryexp = 
      peg $ choice $ longest <$>
      [Nil <$ keyword "nil",
       Bool <$> pure False <* keyword "false",
       Bool <$> pure True <* keyword "true",
       numeral,
       String <$> literalString,
       Vararg <$ symbol "...",
       EFunDef <$> functiondef,
       PrefixExp <$> prefixexp <* notFollowedBy (symbol "("), -- fix the ambiguity from 3.3.1
       TableConst <$> tableconstructor],
   prefixexp =
      PEVar <$> var <|>
      PEFunCall <$> functioncall <|>
      Paren <$ symbol "(" <*> exp <* symbol ")",

   functioncall =
      (NormalFunCall <$> prefixexp <*> args) <|>
      (MethodCall <$> prefixexp <* symbol ":" <*> name <*> args),

   args =
      peg $ choice $ longest <$>
      [Args <$ symbol "(" <*> moptional explist <* symbol ")",
       TableArg <$> tableconstructor,
       StringArg <$> literalString],

   functiondef = keyword "function" *> funcbody,

   funcbody = uncurry FunBody <$ symbol "(" <*> parlist <* symbol ")" <*> block <* keyword "end",

   parlist =
      (,) <$> namelist <*> (True <$ symbol "," <* symbol "..." <|> pure False) <|> 
      (,) <$> pure [] <*> (True <$ symbol "..." <|> pure False),

   tableconstructor = symbol "{" *> fieldlist <* symbol "}",

   fieldlist = peg (sepBy1 (longest field) (longest fieldsep) <* optional (longest fieldsep) <|> pure []),
   field = peg $
      longest (ExpField <$ symbol "[" <*> exp <* symbol "]" <* symbol "=" <*> exp) <|>
      longest (NamedField <$> name <* symbol "=" <*> exp) <|>
      Field <$> longest exp,

   fieldsep = void (symbol "," <|> symbol ";")}

   where digits = takeCharsWhile1 isDigit
         hexDigits = takeCharsWhile1 isHexDigit
         initialHexDigits = (string "0x" <|> string "0X") <> hexDigits
         exponent = (string "e" <|> string "E") <> moptional (string "+" <|> string "-") <> digits
         hexExponent = (string "p" <|> string "P") <> moptional (string "+" <|> string "-") <> digits
         numeral = terminalPEG $
            choice
            [Number IntNum <$> digits <* notSatisfyChar (`elem` ['.', 'e', 'E', 'x', 'X']),
             Number FloatNum <$> (digits <> string "." <> moptional digits <> moptional exponent),
             Number FloatNum <$> (string "." <> digits <> moptional exponent),
             Number FloatNum <$> (digits <> exponent),
             Number IntNum <$> initialHexDigits <* notSatisfyChar (`elem` ['.', 'p', 'P']),
             Number FloatNum <$> (initialHexDigits <> string "." <> moptional hexDigits <> moptional hexExponent),
             Number FloatNum <$> ((string "0x." <|> string "0X.") <> hexDigits <> moptional hexExponent),
             Number FloatNum <$> (initialHexDigits <> hexExponent)]
            <* notSatisfyChar isAlphaNum <* ignorablePEG
         literalString = terminalPEG $
                         let escapeSequence =
                                string "\\" 
                                <> choice [Textual.singleton <$> oneOf "\\abfnrtv\"\'\n",
                                           satisfyCharInput isDigit <> upto 2 isDigit,
                                           string "x" <> (fromString <$> count 2 hexDigit),
                                           string "u{" <> (fromString <$> some hexDigit) <> string "}",
                                           string "z" <* takeCharsWhile isSpace]
                             literalWith quote = string quote
                                                 <> concatMany (escapeSequence <|>
                                                                takeWhile1 (\c-> c /= "\\" && c /= quote))
                                                 <> string quote
                         in (literalWith "\"" <|>
                             literalWith "\'" <|>
                             longBracket)
                            <* ignorablePEG
         name = terminalPEG $
                do let isStartChar c = isLetter c || c == '_'
                       isNameChar c = isStartChar c || isDigit c
                   identifier <- (satisfyCharInput isStartChar <> takeCharsWhile isNameChar)
                   guard (notMember identifier reservedKeywords)
                   pure (Name identifier)
                <* ignorablePEG
                <?> "name"
         longBracket = do void (string "[")
                          equalSigns <- takeCharsWhile (== '=')
                          void (string "[")
                          newline <- string "\n" <|> mempty <$ notSatisfyChar (== '\n')
                          let terminator = string "]" <> string equalSigns <> string "]"
                          (("[" <> equalSigns <> "[" <> newline) <>)
                             <$> concatMany (notFollowedBy terminator *> anyToken <> takeCharsWhile (/= ']')) 
                                 <> terminator
         comment = string "--" *> (longBracket <|>
                                   takeCharsWhile (/= '\n') <* (void (char '\n') <|> endOfInput))
         ignorable = terminalPEG ignorablePEG
         ignorablePEG = takeCharsWhile isSpace *> skipMany (comment *> takeCharsWhile isSpace)
         keyword k = terminalPEG (string k <* notSatisfyChar isAlphaNum <* ignorablePEG)
         symbol s = terminalPEG (string s <* ignorablePEG)
