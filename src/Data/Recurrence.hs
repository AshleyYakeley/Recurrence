module Data.Recurrence(M,T,Value,runRead,readValue,evalWithDict,readExpression) where
{
    import Text.ParserCombinators.ReadPrec;
    import Data.Recurrence.Time;
    import Data.Recurrence.Read;
    import Data.Recurrence.Value;
    import Data.SExpression.Read;
    import Data.Recurrence.Eval;

    readValue :: (?now :: T) => ReadPrec (M Value);
    readValue = fmap evalWithDict readExpression;
}
