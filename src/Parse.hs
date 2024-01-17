module Parse where

{- Data Structures -}

data ExprNode = ExprVar String
              | ExprNumber Double
              | ExprChar Char
              | ExprString String
              | ExprBool Bool
              | ExprQuotation ExprNode
              | ExprProcedureCall ExprNode [ExprNode]
              | ExprLambda [String] ExprNode
              | ExprVariadicLambda [String] String ExprNode
              -- (special forms)
              | ExprIf ExprNode ExprNode ExprNode
              | ExprAssignment String ExprNode
              | ExprCond [CondClause]
              | ExprAnd ExprNode ExprNode
              | ExprOr ExprNode ExprNode
              | ExprLet [BindingSpec] ExprNode
              | ExprLetStar [BindingSpec] ExprNode
              | ExprLetrec [BindingSpec] ExprNode
              | ExprBegin [ExprNode]

data CondClause = CondIf ExprNode [ExprNode]
                | CondElse [ExprNode]

data BindingSpec = BindingSpec String ExprNode

-- Datum: similar to ExprNode, but without special forms
-- (used for quotation)
data Datum = DatumSymbol String
           | DatumBool Bool
           | DatumNumber Double
           | DatumChar Char
           | DatumString String
           | DatumList [Datum]
           | DatumVector [Datum]

{-

expression -> variable
            | quotation
            | boolean
            | number
            | character
            | string
            | procedure_call
            | lambda
            | special_form

variable -> <non-keyword-id>

quotation -> ' datum
           | ( quote datum )

procedure_call -> ( expression expression* )

lambda -> ( lambda formals body )

formals -> ( variable* )
         | variable
         | (variable+ . variable)

body -> definition* sequence

sequence -> expression* expression

special_form -> sf_if
              | sf_set_bang
              | sf_cond
              | sf_and
              | sf_of
              | sf_let
              | sf_let_star
              | sf_letrec
              | sf_begin

sf_if -> ( if expression expression expression )

sf_set_bang -> ( set! variable expression )

sf_cond -> ( cond cond_clause+ )
         | ( cond cond_clause* ( else sequence ) )

cond_clause -> ( expression sequence )

sf_and -> ( and expression* )

sf_or -> ( or expression* )

sf_let -> ( let ( binding_spec* ) body )

sf_let_star -> ( let* ( binding_spec* ) body )

sf_letrec -> ( letrec ( binding_spec* ) body )

binding_spec -> ( variable expression)

sf_begin -> ( begin seuqence )

-}

{-

datum -> boolean
       | number
       | character
       | string
       | symbol
       | list
       | vector

symbol -> <id>

list -> ( datum* )

vector -> #( datum* )

-}

{-

program -> expression | definition

definition -> ( define variable expression )
            | ( define ( variable def_formals ) body )

def_formals -> variable*
             | variable* . variable

-}