module Data.Recurrence(M,T,Value,runRead,readValue,evalWithDict) where
{
    import Text.Read;
    import Data.Recurrence.Time;
    import Data.SExpression.Read;
    import Data.Recurrence.SExpression;

    readValue :: (?now :: T) => ReadPrec (M Value);
    readValue = fmap evalWithDict readPrec;
}
