module Data.SetSearch.Intervals where
{
    import Control.Applicative hiding (empty);
    import Data.SetSearch.Set;
    import Data.SetSearch.PointSet;
    import Data.SetSearch.DeltaSmaller;
    import Data.SetSearch.StepFunction;
    
    type Intervals a = StepFunction a Bool;
    
    instance (Ord a) => Set (Intervals a) where
    {
        empty = pure False;
        member = sfValue;
        union = liftA2 (||);
        intersect = liftA2 (&&);
        diff = liftA2 (\a b -> a && (not b));
        symdiff = liftA2 (\a b -> a /= b);
    };
    
    instance (Ord a) => SetFull (Intervals a) where
    {
        full = pure True;
        invert = fmap not;
    };
    
    intervalsIntersect :: (Ord a) => PointSet a -> Intervals a -> PointSet a;
    intervalsIntersect p i = mixedIntersect i p;
    
    intervalsDiff :: (Ord a) => PointSet a -> Intervals a -> PointSet a;
    intervalsDiff p i = filterIntersect (not . (member i)) p;
    
    intervalsFromTo :: (Ord a,?first :: a) => PointSet a -> PointSet a -> Intervals a;
    intervalsFromTo ps1 ps2 = MkStepFunction
    {
        sfValue = pointsOnAndOff ps1 ps2,
        sfPossibleChanges = union ps1 ps2
    };
    
    intervalsStartOf :: (DeltaSmaller a) => Intervals a -> PointSet a;
    intervalsStartOf i = sfMatchChanges i id;
    
    intervalsEndOf :: (DeltaSmaller a) => Intervals a -> PointSet a;
    intervalsEndOf i = sfMatchChanges i not;
    
    intervalsOnAfter :: (Ord a,?first :: a) => PointSet a -> Intervals a;
    intervalsOnAfter ps = MkStepFunction
    {
        sfValue = pointsOnAfter ps,
        sfPossibleChanges = ps
    };

    intervalsOf :: (Ord a,?first :: a,?last :: a) => PointSet a -> PointSet a -> Intervals a;
    intervalsOf subject delimiter = MkStepFunction
    {
        sfValue = \a -> case pointsPrevious delimiter subject a of
        {
            Just (Right _) -> True;
            _ -> case pointsNext delimiter subject a of
            {
                Just (Right _) -> True;
                _ -> False;
            };           
        },
        sfPossibleChanges = delimiter
    };
}
