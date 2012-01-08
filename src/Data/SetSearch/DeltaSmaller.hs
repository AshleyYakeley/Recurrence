module Data.SetSearch.DeltaSmaller where
{
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
}
