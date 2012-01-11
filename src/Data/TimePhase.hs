module Data.TimePhase(M,T,Value,readValue,evalWithDict,readExpression,Item(..),readItems,showItems) where
{
    import Text.ParserCombinators.ReadPrec;
    import Data.TimePhase.Read;
    import Data.TimePhase.Value;
    import Data.TimePhase.Eval;
    import Data.TimePhase.Item;
    
    readValue :: ReadPrec (M Value);
    readValue = fmap evalWithDict readExpression;
}
