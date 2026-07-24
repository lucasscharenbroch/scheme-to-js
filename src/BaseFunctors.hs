module BaseFunctors where
-- a separate module because inlining the TH seems to
-- mess up other declarations

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Parse (Datum, Expression)

-- data Datum = DatumSymbol String
--            | DatumQuotation Datum
--            | DatumBool Bool
--            | DatumNumber Double
--            | DatumChar Char
--            | DatumString String
--            | DatumPair Datum Datum
--            | DatumNull
--            | DatumVector [Datum]

-- datums :: [DatumF ()]
-- datums =
    -- [ DatumSymbolF "sym"
    -- , DatumQuotationF ()
    -- , DatumBoolF True
    -- , DatumNumberF 2.3
    -- , DatumCharF 'c'
    -- , DatumStringF "d"
    -- , DatumPairF () ()
    -- , DatumNullF
    -- , DatumVectorF [(), (), ()]
    -- ]

makeBaseFunctor ''Datum

makeBaseFunctor ''Expression
