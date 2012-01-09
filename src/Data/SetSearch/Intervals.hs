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
        type Base (Intervals a) = a;
        empty = pure False;
        member = sfValue;
        union = liftA2 (||);
        intersect = liftA2 (&&);
        diff = liftA2 (\a b -> a && (not b));
    };
    
    instance (Ord a) => SetFull (Intervals a) where
    {
        full = pure True;
        invert = fmap not;
    };
    
    intervalsIntersect :: (Ord a) => PointSet a -> Intervals a -> PointSet a;
    intervalsIntersect p i = filterIntersect (member i) p;
    
    intervalsDiff :: (Ord a) => PointSet a -> Intervals a -> PointSet a;
    intervalsDiff p i = filterIntersect (not . (member i)) p;
    
    intervalsFromTo :: (Ord a,?first :: a) => PointSet a -> PointSet a -> Intervals a;
    intervalsFromTo ps1 ps2 = MkStepFunction
    {
        sfValue = onAndOff ps1 ps2,
        sfPossibleChanges = union ps1 ps2
    };
    
    intervalsStartOf :: (DeltaSmaller a) => Intervals a -> PointSet a;
    intervalsStartOf i = sfMatchChanges i id;
    
    intervalsEndOf :: (DeltaSmaller a) => Intervals a -> PointSet a;
    intervalsEndOf i = sfMatchChanges i not;
}
