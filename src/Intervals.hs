module Intervals where
{
    import Control.Applicative hiding (empty);
    import Set;
    import PointSet;
    import DeltaSmaller;
    import StepFunction;
    
    type Intervals a = StepFunction a Bool;
    
    iMember = sfValue;
    
    instance (Ord a) => Set1 (Intervals a) where
    {
        type Base (Intervals a) = a;
        empty = pure False;
        member = sfValue;
        union = liftA2 (||);
    };
    
    intervalsFromTo :: (Ord a,?first :: a) => PointSet a -> PointSet a -> Intervals a;
    intervalsFromTo ps1 ps2 = MkStepFunction
    {
        sfValue = onAndOff ps1 ps2,
        sfPossibleChanges = union ps1 ps2
    };
    
    intervalsIntersect :: (Ord a) => Intervals a -> Intervals a -> Intervals a;
    intervalsIntersect = liftA2 (&&);
    
    intervalsStartOf :: (DeltaSmaller a) => Intervals a -> PointSet a;
    intervalsStartOf i = sfMatchChanges i id;
    
    intervalsEndOf :: (DeltaSmaller a) => Intervals a -> PointSet a;
    intervalsEndOf i = sfMatchChanges i not;
}
