module Data.TimePhase(M,T,Value,runRead,readValue,evalWithDict,readExpression,Item(..),readItems,showItems) where
{
    import Text.ParserCombinators.ReadPrec;
    import Data.TimePhase.Time;
    import Data.TimePhase.Read;
    import Data.TimePhase.Value;
    import Data.TimePhase.SExpression.Read;
    import Data.TimePhase.Eval;
    import Data.TimePhase.Item;
    
    readValue :: ReadPrec (M Value);
    readValue = fmap evalWithDict readExpression;
}
