module Data.SetSearch.DeltaSmaller where
{
    import Data.Time;

    class (Ord a) => DeltaSmaller a where
    {
        -- | Return some slightly smaller value, unless given the smallest possible value.
        -- What \"slightly\" means doesn't really matter.
        ;
        deltaSmaller :: a -> Maybe a;
    };
    
    instance DeltaSmaller Integer where
    {
        deltaSmaller a = Just (a - 1);
    };
    
    instance DeltaSmaller Double where
    {
        deltaSmaller a = Just (a - 1);
    };

    instance DeltaSmaller UTCTime where
    {
        deltaSmaller (UTCTime d t) = Just (UTCTime (addDays (-1) d) t);
    };

    instance DeltaSmaller LocalTime where
    {
        deltaSmaller (LocalTime d t) = Just (LocalTime (addDays (-1) d) t);
    };
}
