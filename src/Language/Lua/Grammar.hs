{-# Language OverloadedStrings, RecordWildCards, ScopedTypeVariables, TemplateHaskell #-}
module Language.Lua.Grammar where

import Control.Applicative
import Control.Monad (guard, void)
import Data.Char (chr, isAlphaNum, isDigit, isHexDigit, isLetter)
import Data.Functor.Classes (Eq1, Show1, eq1, showsPrec1)
import Data.Monoid ((<>))
import Data.Monoid.Textual (TextualMonoid)
import qualified Data.Monoid.Textual as Textual
import Data.Text (Text)
import Data.Set (Set, fromList, notMember)
import Numeric (readHex)

import qualified Rank2.TH
import Text.Grampa
import Text.Grampa.ContextFree.LeftRecursive (Parser)

import Text.Parser.Char (alphaNum, char, digit, hexDigit)
import Text.Parser.Combinators (choice, count, sepBy, skipMany, try)
import Text.Parser.Expression (Assoc(..), Operator(..))

import Language.Lua.Syntax

import Prelude hiding (EQ, GT, LT, exp, exponent)

data LuaGrammar f = LuaGrammar{
   chunk :: f Block,
   block :: f Block,
   stats :: f [Stat],
   stat :: f Stat,
   retstat :: f [Exp],
   label :: f Name,
   funcname :: f FunName,
   varlist :: f [Var],
   var :: f Var,
   namelist :: f [Name],
   explist :: f [Exp],
   explist1 :: f [Exp],
   exp :: f Exp,
   primaryexp :: f Exp,
   prefixexp :: f PrefixExp,
   functioncall :: f FunCall,
   args :: f FunArg,
   functiondef :: f FunBody,
   funcbody :: f FunBody,
   parlist :: f ([Name], Bool),
   tableconstructor :: f [TableField],
   fieldlist :: f [TableField],
   field :: f TableField,
   fieldsep :: f (),
   binop :: f Binop,
   unop :: f Unop,
   literalString :: f Text,
   longBracket :: f Text,
   comment :: f Text,
   numeral :: f Exp,
   name :: f Name,
   digits :: f Text,
   hexDigits :: f Text,
   initialHexDigits :: f Text,
   exponent :: f Text,
   hexExponent :: f Text}

$(Rank2.TH.deriveAll ''LuaGrammar)

instance Show1 f => Show (LuaGrammar f) where
   showsPrec prec g rest = "LuaGrammar{" ++
      "  chunk = " ++ showsPrec1 prec (chunk g) "\n" ++
      "  block = " ++ showsPrec1 prec (block g) "\n" ++
      "  stats = " ++ showsPrec1 prec (stats g) "\n" ++
      "  stat = " ++ showsPrec1 prec (stat g) "\n" ++
      "  retstat = " ++ showsPrec1 prec (retstat g) "\n" ++
      "  label = " ++ showsPrec1 prec (label g) "\n" ++
      "  funcname = " ++ showsPrec1 prec (funcname g) "\n" ++
      "  varlist = " ++ showsPrec1 prec (varlist g) "\n" ++
      "  var = " ++ showsPrec1 prec (var g) "\n" ++
      "  namelist = " ++ showsPrec1 prec (namelist g) "\n" ++
      "  explist = " ++ showsPrec1 prec (explist g) "\n" ++
      "  explist1 = " ++ showsPrec1 prec (explist1 g) "\n" ++
      "  exp = " ++ showsPrec1 prec (exp g) "\n" ++
      "  primaryexp = " ++ showsPrec1 prec (primaryexp g) "\n" ++
      "  prefixexp = " ++ showsPrec1 prec (prefixexp g) "\n" ++
      "  functioncall = " ++ showsPrec1 prec (functioncall g) "\n" ++
      "  args = " ++ showsPrec1 prec (args g) "\n" ++
      "  functiondef = " ++ showsPrec1 prec (functiondef g) "\n" ++
      "  funcbody = " ++ showsPrec1 prec (funcbody g) "\n" ++
      "  parlist = " ++ showsPrec1 prec (parlist g) "\n" ++
      "  tableconstructor = " ++ showsPrec1 prec (tableconstructor g) "\n" ++
      "  fieldlist = " ++ showsPrec1 prec (fieldlist g) "\n" ++
      "  field = " ++ showsPrec1 prec (field g) "\n" ++
      "  fieldsep = " ++ showsPrec1 prec (fieldsep g) "\n" ++
      "  binop = " ++ showsPrec1 prec (binop g) "\n" ++
      "  unop = " ++ showsPrec1 prec (unop g) "\n" ++
      "  literalString = " ++ showsPrec1 prec (literalString g) "\n" ++
      "  longBracket = " ++ showsPrec1 prec (longBracket g) "\n" ++
      "  comment = " ++ showsPrec1 prec (comment g) "\n" ++
      "  numeral = " ++ showsPrec1 prec (numeral g) "\n" ++
      "  name = " ++ showsPrec1 prec (name g) "\n" ++
      "  digits = " ++ showsPrec1 prec (digits g) "\n" ++
      "  hexDigits = " ++ showsPrec1 prec (hexDigits g) "\n" ++
      "  initialHexDigits = " ++ showsPrec1 prec (initialHexDigits g) "\n" ++
      "  exponent = " ++ showsPrec1 prec (exponent g) "\n" ++
      "  hexExponent = " ++ showsPrec1 prec (hexExponent g) ("}" ++ rest)

instance Eq1 f => Eq (LuaGrammar f) where
   g1 == g2 = eq1 (chunk g1) (chunk g2)
              && eq1 (block g1) (block g2)
              && eq1 (stats g1) (stats g2)
              && eq1 (stat g1) (stat g2)
              && eq1 (retstat g1) (retstat g2)
              && eq1 (label g1) (label g2)
              && eq1 (funcname g1) (funcname g2)
              && eq1 (varlist g1) (varlist g2)
              && eq1 (var g1) (var g2)
              && eq1 (namelist g1) (namelist g2)
              && eq1 (explist g1) (explist g2)
              && eq1 (explist1 g1) (explist1 g2)
              && eq1 (exp g1) (exp g2)
              && eq1 (primaryexp g1) (primaryexp g2)
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
              && eq1 (binop g1) (binop g2)
              && eq1 (unop g1) (unop g2)
              && eq1 (literalString g1) (literalString g2)
              && eq1 (longBracket g1) (longBracket g2)
              && eq1 (comment g1) (comment g2)
              && eq1 (numeral g1) (numeral g2)
              && eq1 (name g1) (name g2)
              && eq1 (digits g1) (digits g2)
              && eq1 (hexDigits g1) (hexDigits g2)
              && eq1 (initialHexDigits g1) (initialHexDigits g2)
              && eq1 (exponent g1) (exponent g2)
              && eq1 (hexExponent g1) (hexExponent g2)

moptional :: (Monoid x, Alternative p) => p x -> p x
moptional p = p <|> pure mempty

ignorable :: -- (TextualMonoid t, Parsing (p LuaGrammar t), GrammarParsing p, MonoidParsing (p LuaGrammar)) => 
             Parser LuaGrammar Text ()
ignorable = whiteSpace *> skipMany (nonTerminal comment *> whiteSpace)

sepBy1 :: Alternative p => p x -> p sep -> p [x]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)
{-# SPECIALIZE sepBy1 :: Parser LuaGrammar Text x -> Parser LuaGrammar Text sep -> Parser LuaGrammar Text [x] #-}

upto :: (TextualMonoid s, MonoidParsing p) => Int -> (Char -> Bool) -> p s s
upto n0 predicate = scanChars n0 (\n c-> if n > 0 && predicate c then Just (pred n) else Nothing)

-- | Tweaked version of 'Text.Parser.Expression.buildExpressionParser' that allows chaining prefix operators of arbitrary
-- precedence
buildExpressionParser :: forall m g s a. (GrammarParsing m, Parsing (m g s)) =>
                         [[Operator (m g s) a]] -> m g s a -> m g s a
buildExpressionParser operators simpleExpr = foldl makeParser prefixExpr operators
   where
      prefixExpr = foldl makePrefixParser simpleExpr operators
      makePrefixParser term ops =
         let (_, _, _, prefix, postfix) = foldr splitOp ([],[],[],[],[]) ops
             prefixOp   = choice prefix  <?> ""
             postfixOp  = choice postfix <?> ""
             termP         = (prefixFactor <|> term) <**> postfixFactor
             prefixFactor  = foldr (.) id <$> some prefixOp <*> makeParser term ops
             postfixFactor = foldr (flip (.)) id <$> many postfixOp
         in termP <?> "operator"
      makeParser term ops
        = let (rassoc,lassoc,nassoc,_prefix,_postfix) = foldr splitOp ([],[],[],[],[]) ops
              rassocOp   = choice rassoc
              lassocOp   = choice lassoc
              nassocOp   = choice nassoc

              ambiguous assoc op = try (op *> empty <?> ("ambiguous use of a " ++ assoc ++ "-associative operator"))

              ambiguousLeft, ambiguousNon, ambiguousRight :: m g s (x -> x)
              ambiguousRight    = ambiguous "right" rassocOp
              ambiguousLeft     = ambiguous "left" lassocOp
              ambiguousNon      = ambiguous "non" nassocOp

              termP = term

              rassocP  = (flip <$> rassocOp <*> (termP <**> recursive rassocP1)
                          <|> ambiguousLeft
                          <|> ambiguousNon)

              rassocP1 = rassocP <|> pure id

              lassocP  = ((flip <$> lassocOp <*> termP) <**> ((.) <$> recursive lassocP1)
                          <|> ambiguousRight
                          <|> ambiguousNon)

              lassocP1 = lassocP <|> pure id

              nassocP = (flip <$> nassocOp <*> termP)
                        <**> (ambiguousRight
                              <|> ambiguousLeft
                              <|> ambiguousNon
                              <|> pure id)
           in term <**> (rassocP <|> lassocP <|> nassocP <|> pure id) <?> "operator"


      splitOp (Infix op assoc) (rassoc,lassoc,nassoc,prefix,postfix)
        = case assoc of
            AssocNone  -> (rassoc,lassoc,op:nassoc,prefix,postfix)
            AssocLeft  -> (rassoc,op:lassoc,nassoc,prefix,postfix)
            AssocRight -> (op:rassoc,lassoc,nassoc,prefix,postfix)

      splitOp (Prefix op) (rassoc,lassoc,nassoc,prefix,postfix)
        = (rassoc,lassoc,nassoc,op:prefix,postfix)

      splitOp (Postfix op) (rassoc,lassoc,nassoc,prefix,postfix)
        = (rassoc,lassoc,nassoc,prefix,op:postfix)

keyword :: -- (Show t, TextualMonoid t, CharParsing (p LuaGrammar t), GrammarParsing p, MonoidParsing (p LuaGrammar)) =>
           Text -> Parser LuaGrammar Text Text
keyword k = string k <* notSatisfyChar isAlphaNum <* ignorable

symbol :: -- (Show t, TextualMonoid t, Parsing (p LuaGrammar t), GrammarParsing p, MonoidParsing (p LuaGrammar)) => 
          Text -> Parser LuaGrammar Text Text
symbol s = string s <* ignorable

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
   chunk = optional (token "#" *> takeCharsWhile (/= '\n') *> (void (token "\n") <|> endOfInput))
           *> ignorable *> block <* endOfInput,
   block = Block <$> stats <*> optional retstat,
   stats = (:) <$> stat <*> stats <|> pure [],
   stat = EmptyStat <$ symbol ";" <|>
          Assign <$> varlist <* symbol "=" <*> explist1 <|>
          FunCall <$> functioncall <|>
          Label <$> label <|>
          Break <$ keyword "break" <|>
          Goto <$ keyword "goto" <*> name <|>
          Do <$ keyword "do" <*> block <* keyword "end" <|>
          While <$ keyword "while" <*> exp <* keyword "do" <*> block <* keyword "end" <|>
          Repeat <$ keyword "repeat" <*> block <* keyword "until" <*> exp <|>
          If <$ keyword "if" <*> ((:) <$> ((,) <$> exp <* keyword "then" <*> block) 
                                      <*> many ((,) <$ keyword "elseif" <*> exp <* keyword "then" <*> block))
                  <*> optional (keyword "else" *> block) <* keyword "end" <|>
          ForRange <$ keyword "for" <*> name <* symbol "="
                   <*> exp <* symbol "," <*> exp <*> optional (symbol "," *> exp)
                   <* keyword "do" <*> block <* keyword "end" <|>
          ForIn <$ keyword "for" <*> namelist <* keyword "in" <*> explist1 
                     <* keyword "do" <*> block <* keyword "end" <|>
          FunAssign <$ keyword "function" <*> funcname <*> funcbody <|>
          LocalFunAssign <$ keyword "local" <* keyword "function" <*> name <*> funcbody <|>
          LocalAssign <$ keyword "local" <*> namelist <*> optional (symbol "=" *> explist1),

   retstat = keyword "return" *> explist <* optional (symbol ";"),
   label = symbol "::" *> name <* symbol "::",
   funcname = FunName <$> name <*> many (symbol "." *> name) <*> optional (symbol ":" *> name),
   varlist = sepBy1 var (symbol ","),
   var = VarName <$> name <|>
         Select <$> prefixexp <* symbol "[" <*> exp <* symbol "]" <|> 
         SelectName <$> prefixexp <* symbol "." <*> name,
   
   namelist = sepBy1 name (symbol ","),
   explist = sepBy exp (symbol ","),
   explist1 = sepBy1 exp (symbol ","),

   -- Operator precedence from 3.4.8
   exp = let binary op = Infix (Binop <$> op)
             operators = [
                [binary (Exp <$ symbol "^") AssocRight],
                [Prefix (Unop <$> unop)],
                [binary (Mul <$ symbol "*") AssocLeft,
                 binary (Div <$ symbol "/") AssocLeft,
                 binary (IDiv <$ symbol "//") AssocLeft,
                 binary (Mod <$ symbol "%") AssocLeft],
                [binary (Add <$ symbol "+") AssocLeft,
                 binary (Sub <$ string "-" <* notSatisfyChar (== '-') <* ignorable) AssocLeft], -- avoid ambiguity with comment
                [binary (Concat <$ symbol "..") AssocRight],
                [binary (ShiftL <$ symbol "<<") AssocLeft,
                 binary (ShiftR <$ symbol ">>") AssocLeft],
                [binary (BAnd <$ symbol "&") AssocLeft],
                [binary (BXor <$ symbol "~") AssocLeft],
                [binary (BOr <$ symbol "|") AssocLeft],
                [binary (LT <$ symbol "<") AssocLeft,
                 binary (GT <$ symbol ">") AssocLeft,
                 binary (LTE <$ symbol "<=") AssocLeft,
                 binary (GTE <$ symbol ">=") AssocLeft,
                 binary (NEQ <$ symbol "~=") AssocLeft,
                 binary (EQ <$ symbol "==") AssocLeft],
                [binary (And <$ keyword "and") AssocLeft],
                [binary (Or <$ keyword "or") AssocLeft]]
         in buildExpressionParser operators primaryexp,
   primaryexp =
      Nil <$ keyword "nil" <|>
      Bool <$> pure False <* keyword "false" <|>
      Bool <$> pure True <* keyword "true" <|>
      numeral <|>
      String <$> literalString <|>
      Vararg <$ symbol "..." <|>
      EFunDef <$> functiondef <|>
      PrefixExp <$> prefixexp <* notFollowedBy (symbol "(") <|> -- fix the ambiguity from 3.3.1
      TableConst <$> tableconstructor,

   prefixexp =
      PEVar <$> var <|>
      PEFunCall <$> functioncall <|>
      Paren <$ symbol "(" <*> exp <* symbol ")",

   functioncall =
      NormalFunCall <$> prefixexp <*> args <|>
      MethodCall <$> prefixexp <* symbol ":" <*> name <*> args,

   args =
      Args <$ symbol "(" <*> explist <* symbol ")" <|>
      TableArg <$> tableconstructor <|>
      StringArg <$> literalString ,

   functiondef = keyword "function" *> funcbody,

   funcbody = uncurry FunBody <$ symbol "(" <*> parlist <* symbol ")" <*> block <* keyword "end",

   parlist =
      (,) <$> namelist <*> (True <$ symbol "," <* symbol "..." <|> pure False) <|> 
      (,) <$> pure [] <*> (True <$ symbol "..." <|> pure False),

   tableconstructor = symbol "{" *> fieldlist <* symbol "}",

   fieldlist = sepBy1 field fieldsep <* optional fieldsep <|> pure [],
   field =
      ExpField <$ symbol "[" <*> exp <* symbol "]" <* symbol "=" <*> exp <|>
      NamedField <$> name <* symbol "=" <*> exp <|>
      Field <$> exp,

   fieldsep = void (symbol "," <|> symbol ";"),

   binop =
      Add        <$ symbol "+"    <|>
      Sub        <$ string "-" <* notSatisfyChar (== '-') <* ignorable <|> 
      Mul        <$ symbol "*"    <|>
      Div        <$ symbol "/"    <|>
      IDiv       <$ symbol "//"   <|>
      Exp        <$ symbol "^"    <|>
      Mod        <$ symbol "%"    <|>
      BAnd       <$ symbol "&"    <|>
      BXor       <$ symbol "~"    <|>
      BOr        <$ symbol "|"    <|>
      ShiftR     <$ symbol ">>"   <|>
      ShiftL     <$ symbol "<<"   <|>
      Concat     <$ symbol ".."   <|>
      LT         <$ symbol "<"    <|>
      LTE        <$ symbol "<="   <|>
      GT         <$ symbol ">"    <|>
      GTE        <$ symbol ">="   <|>
      EQ         <$ symbol "=="   <|>
      NEQ        <$ symbol "~="   <|>
      And        <$ keyword "and" <|>
      Or         <$ keyword "or",

   unop =
      Neg        <$ string "-" <* notSatisfyChar (== '-') <* ignorable <|>   -- eliminate ambiguity
      Not        <$ keyword "not" <|>
      Len        <$ symbol "#"    <|>
      Complement <$ symbol "~",

   numeral = (Number IntNum <$> digits <|>
              Number FloatNum <$> (digits <> string "." <> moptional digits <> moptional exponent) <|>
              Number FloatNum <$> (string "." <> digits <> moptional exponent) <|>
              Number FloatNum <$> (digits <> exponent) <|>
              Number IntNum <$> initialHexDigits <|>
              Number FloatNum <$> (initialHexDigits <> string "." <> moptional hexDigits <> moptional hexExponent) <|>
              Number FloatNum <$> ((string "0x." <|> string "0X.") <> hexDigits <> moptional hexExponent) <|>
              Number FloatNum <$> (initialHexDigits <> hexExponent))
             <* notSatisfyChar isAlphaNum <* ignorable,
   digits = takeCharsWhile1 isDigit,
   hexDigits = takeCharsWhile1 isHexDigit,
   initialHexDigits = (string "0x" <|> string "0X") <> hexDigits,
   exponent = (string "e" <|> string "E") <> moptional (string "+" <|> string "-") <> digits,
   hexExponent = (string "p" <|> string "P") <> moptional (string "+" <|> string "-") <> digits,
   name = do let isStartChar c = isLetter c || c == '_'
                 isNameChar c = isStartChar c || isDigit c
             identifier <- (satisfyCharInput isStartChar <> takeCharsWhile isNameChar)
             guard (notMember identifier reservedKeywords)
             ignorable
             pure (Name identifier)
          <?> "name",
   literalString = let escapeSequence =
                          token "\\" 
                          *> ("\\" <$ token "\\" <|>
                              "\a" <$ token "a" <|>
                              "\b" <$ token "b" <|>
                              "\f" <$ token "f" <|>
                              "\n" <$ token "n" <|>
                              "\r" <$ token "r" <|>
                              "\t" <$ token "t" <|>
                              "\v" <$ token "v" <|>
                              "\"" <$ token "\"" <|>
                              "\'" <$ token "\'" <|>
                              "\n" <$ token "\n" <|>
                              (Textual.singleton . chr) 
                              <$> (read <$> ((:) <$> digit <*> (Textual.toString (const "") <$> upto 2 isDigit)) <|>
                                   token "x" *> ((fst . head . readHex) <$> count 2 hexDigit) <|>
                                   string "u{" *> ((fst . head . readHex) <$> some hexDigit) <* token "}")
                                <|>
                              "" <$ token "z" <* whiteSpace)
                       literalWith quote = char quote
                                           *> concatMany (escapeSequence <|>
                                                          takeCharsWhile1 (\c-> c /= '\\' && c /= quote))
                                           <* char quote
                   in (literalWith '"' <|> 
                       literalWith '\'' <|> 
                       longBracket)
                      <* ignorable,
   longBracket = do void (token "[")
                    equalSigns <- takeCharsWhile (== '=')
                    void (token "[")
                    void (token "\n") <|> notSatisfyChar (== '\n')
                    let terminator = token "]" *> string equalSigns *> token "]"
                    concatMany (notFollowedBy terminator *> anyToken <> takeCharsWhile (/= ']')) <* terminator,
   comment = string "--" *> (takeCharsWhile (/= '\n') <* (void (char '\n') <|> endOfInput) <|>
                             longBracket)
   }
