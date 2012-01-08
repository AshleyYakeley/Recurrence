module Intervals where
{
    import Control.Applicative hiding (empty);
    import Set;
    import PointSet;
    import DeltaSmaller;
    import StepFunction;
    
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
