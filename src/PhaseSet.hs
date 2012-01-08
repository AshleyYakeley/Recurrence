module PhaseSet where
{
    import Set;
    import StationSet;
    import Control.Applicative hiding (empty);
    
    class (Ord a) => DeltaSmaller a where
    {
        -- | Return some slightly smaller value, unless given the smallest possible value.
        -- What "slightly" means doesn't really matter.
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
    
    type Intervals a = StepFunction a Bool;
    
    iMember = sfValue;
    iStarts sf = sfMatchPossibleChanges sf id;
    iEnds sf = sfMatchPossibleChanges sf not;
    
    intervalsEmpty :: (Ord a) => Intervals a;
    intervalsEmpty = pure False;

    intervalsFromTo :: (Ord a,?first :: a) => PointSet a -> PointSet a -> Intervals a;
    intervalsFromTo ps1 ps2 = MkStepFunction
    {
        sfValue = onAndOff ps1 ps2,
        sfPossibleChanges = union ps1 ps2
    };
    
    intervalsUnion :: (Ord a) => Intervals a -> Intervals a -> Intervals a;
    intervalsUnion = liftA2 (||);
    
    intervalsIntersect :: (Ord a) => Intervals a -> Intervals a -> Intervals a;
    intervalsIntersect = liftA2 (&&);
    
    intervalsStartOf :: (DeltaSmaller a) => Intervals a -> PointSet a;
    intervalsStartOf i = sfMatchChanges i id;
    
    intervalsEndOf :: (DeltaSmaller a) => Intervals a -> PointSet a;
    intervalsEndOf i = sfMatchChanges i not;
    

{-    
    intervalsIntersect :: (Ord a) => Intervals a -> Intervals a -> Intervals a;
    intervalsIntersect (MkIntervals start1 ends1) (MkIntervals start2 ends2) = 
        MkIntervals () (union ends1 ends2);
 -}   
    data Phase a = MkPhase Bool (Intervals a);
    
    phaseEmpty :: (Ord a) => Phase a;
    phaseEmpty = MkPhase False intervalsEmpty;
    
    phaseMember :: (Ord a) => Phase a -> a -> Bool;
    phaseMember (MkPhase prior intervals) t = (iMember intervals t) /= prior;
}
