module Data.TimePhase(M,T,Value,readValue) where
{
    import Text.ParserCombinators.ReadPrec;
    import Data.TimePhase.Read;
    import Data.TimePhase.Value;
    import Data.TimePhase.Eval;
    
    readValue :: ReadPrec (M Value);
    readValue = fmap evalWithDict readExpression;
}
