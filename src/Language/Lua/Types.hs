{-# OPTIONS_GHC -Wall
                -fno-warn-hi-shadowing
                -fno-warn-name-shadowing
                -fno-warn-unused-do-bind #-}
module Language.Lua.Types where

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
    | FunAssign Name FunBody -- ^function \<var\> (..) .. end
    | LocalFunAssign Name FunBody -- ^local function \<var\> (..) .. end
    | LocalAssign [Name] (Maybe [Exp]) -- ^local var1, var2 .. = exp1, exp2 ..
    | EmptyStat -- ^`,`
    deriving (Show, Eq)

data Exp
    = Nil
    | Bool String -- ^true, false
    | Number String
    | String String
    | Vararg -- ^...
    | EFunDef FunDef -- ^function (..) .. end
    | PrefixExp PrefixExp -- ^variable, function call, or parenthesized expression
    | TableConst Table -- ^table constructor
    | Binop Binop Exp Exp -- ^binary operations, + - * / ^ % .. \< \<= \> \>= == ~= and or
    | Unop Unop Exp -- ^unary operations, - not #
    deriving (Show, Eq)

data Var
    = Name Name -- ^variable
    | Select PrefixExp Exp -- ^table[exp]
    | SelectName PrefixExp Name -- ^table.variable
    deriving (Show, Eq)

data Binop = Add | Sub | Mul | Div | Exp | Mod | Concat
    | LT | LTE | GT | GTE | EQ | NEQ | And | Or
    deriving (Show, Eq)

data Unop = Neg | Not | Len
    deriving (Show, Eq)

data PrefixExp
    = PEVar Var
    | PEFunCall FunCall
    | Paren Exp
    deriving (Show, Eq)

data Table = Table [TableField] -- ^list of table fields
    deriving (Show, Eq)

data TableField
    = ExpField Exp Exp -- ^\[exp\] = exp
    | NamedField Name Exp -- ^name = exp
    | Field Exp -- ^exp
    deriving (Show, Eq)

-- | A block is list of statements with optional return statement.
data Block = Block [Stat] (Maybe [Exp]) -- ^list of statements with optional return statement
    deriving (Show, Eq)

data FunDef = FunDef FunBody
    deriving (Show, Eq)

data FunBody = FunBody [Name] Bool Block -- ^args, vararg predicate, block
    deriving (Show, Eq)

data FunCall
    = NormalFunCall PrefixExp FunArg -- ^prefixexp ( funarg )
    | MethodCall PrefixExp Name FunArg -- ^prefixexp : name ( funarg )
    deriving (Show, Eq)

data FunArg
    = Args [Exp] -- ^list of args
    | TableArg Table -- ^table constructor
    | StringArg String -- ^string
    deriving (Show, Eq)