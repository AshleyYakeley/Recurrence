module StepFunction where
{
    import Control.Applicative hiding (empty);
    import Set;
    import PointSet;
    import DeltaSmaller;
    
    data StepFunction a b = MkStepFunction
    {
        sfValue :: a -> b,
        sfPossibleChanges :: PointSet a
    };
    
    instance Functor (StepFunction a) where
    {
        fmap bc sf = MkStepFunction
        {
            sfValue = bc . (sfValue sf),
            sfPossibleChanges = sfPossibleChanges sf
        };
    };
    
    instance (Ord a) => Applicative (StepFunction a) where
    {
        pure b = MkStepFunction
        {
            sfValue = pure b,
            sfPossibleChanges = empty
        };
        
        sfbc <*> sfb = MkStepFunction
        {
            sfValue = (sfValue sfbc) <*> (sfValue sfb),
            sfPossibleChanges = union (sfPossibleChanges sfbc) (sfPossibleChanges sfb)
        };
    };
    
    sfChanges :: (DeltaSmaller a,Eq b) => StepFunction a b -> PointSet a;
    sfChanges sf = fIntersect (\a -> case deltaSmaller a of
    {
        Just a' -> case ssLastBeforeUntil (sfPossibleChanges sf) a a' of
        {
            Just a'' -> sfValue sf a'' /= sfValue sf a;
            Nothing -> sfValue sf a' /= sfValue sf a;
        };
        Nothing -> False;
    }) (sfPossibleChanges sf);
    
    sfMatchPossibleChanges :: (Ord a) => StepFunction a b -> (b -> Bool) -> PointSet a;
    sfMatchPossibleChanges sf match = fIntersect (match . (sfValue sf)) (sfPossibleChanges sf);
    
    sfMatchChanges :: (DeltaSmaller a,Eq b) => StepFunction a b -> (b -> Bool) -> PointSet a;
    sfMatchChanges sf match = fIntersect (match . (sfValue sf)) (sfChanges sf);
}
