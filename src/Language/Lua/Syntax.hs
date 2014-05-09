{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Lua 5.2 syntax tree, as specified in <http://www.lua.org/manual/5.2/manual.html#9>.
module Language.Lua.Syntax where

import Prelude hiding (LT, EQ, GT)
import Data.Data

type Name = String

data Stat
    = Assign [Var] [Exp] -- ^var1, var2 .. = exp1, exp2 ..
    | FunCall FunCall -- ^function call
    | Label Name -- ^label for goto
    | Break -- ^break
    | Goto Name -- ^goto label
    | Do Block -- ^do .. end
    | While Exp Block -- ^while .. do .. end
    | Repeat Block Exp -- ^repeat .. until ..
    | If [(Exp, Block)] (Maybe Block) -- ^if .. then .. [elseif ..] [else ..] end
    | ForRange Name Exp Exp (Maybe Exp) Block -- ^for x=start, end [, step] do .. end
    | ForIn [Name] [Exp] Block -- ^for x in .. do .. end
    | FunAssign FunName FunBody -- ^function \<var\> (..) .. end
    | LocalFunAssign Name FunBody -- ^local function \<var\> (..) .. end
    | LocalAssign [Name] (Maybe [Exp]) -- ^local var1, var2 .. = exp1, exp2 ..
    | EmptyStat -- ^/;/
    deriving (Show, Eq, Data, Typeable)

data Exp
    = Nil
    | Bool Bool
    | Number String
    | String String
    | Vararg -- ^/.../
    | EFunDef FunBody -- ^/function (..) .. end/
    | PrefixExp PrefixExp
    | TableConst [TableField] -- ^table constructor
    | Binop Binop Exp Exp -- ^binary operators, /+ - * ^ % .. < <= > >= == ~= and or/
    | Unop Unop Exp -- ^unary operators, /- not #/
    deriving (Show, Eq, Data, Typeable)

data Var
    = VarName Name -- ^variable
    | Select PrefixExp Exp -- ^/table[exp]/
    | SelectName PrefixExp Name -- ^/table.variable/
    deriving (Show, Eq, Data, Typeable)

data Binop = Add | Sub | Mul | Div | Exp | Mod | Concat
    | LT | LTE | GT | GTE | EQ | NEQ | And | Or
    deriving (Show, Eq, Data, Typeable)

data Unop = Neg | Not | Len
    deriving (Show, Eq, Data, Typeable)

data PrefixExp
    = PEVar Var
    | PEFunCall FunCall
    | Paren Exp
    deriving (Show, Eq, Data, Typeable)

data TableField
    = ExpField Exp Exp -- ^/[exp] = exp/
    | NamedField Name Exp -- ^/name = exp/
    | Field Exp
    deriving (Show, Eq, Data, Typeable)

-- | A block is list of statements with optional return statement.
data Block = Block [Stat] (Maybe [Exp])
    deriving (Show, Eq, Data, Typeable)

data FunName = FunName Name [Name] (Maybe Name)
    deriving (Show, Eq, Data, Typeable)

data FunBody = FunBody [Name] Bool Block -- ^(args, vararg, block)
    deriving (Show, Eq, Data, Typeable)

data FunCall
    = NormalFunCall PrefixExp FunArg -- ^/prefixexp ( funarg )/
    | MethodCall PrefixExp Name FunArg -- ^/prefixexp : name ( funarg )/
    deriving (Show, Eq, Data, Typeable)

data FunArg
    = Args [Exp] -- ^list of args
    | TableArg [TableField] -- ^table constructor
    | StringArg String -- ^string
    deriving (Show, Eq, Data, Typeable)