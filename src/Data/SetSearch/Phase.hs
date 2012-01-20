module Data.SetSearch.Phase where
{
    import Data.SetSearch.Set;
    import Data.SetSearch.DeltaSmaller;
    import Data.SetSearch.PointSet;
    import Data.SetSearch.PhaseSet;
    import Data.SetSearch.Intervals;
    import Data.SetSearch.StepFunction;
    
    data Phase a = MkPhase
    {
        phaseSet :: PhaseSet a,
        phaseStartOf :: PointSet a
    };
    
    instance BasedOn (Phase a) where
    {
        type Base (Phase a) = a;
    };
    
    instance RemapBase (Phase a) (Phase b) where
    {
        remapBase ab ba phasea = MkPhase
        {
            phaseSet = remapBase ab ba (phaseSet phasea),
            phaseStartOf = remapBase ab ba (phaseStartOf phasea)
        };
    };
    
    toPhase :: (DeltaSmaller a) => PhaseSet a -> Phase a;
    toPhase ps = MkPhase
    {
        phaseSet = ps,
        phaseStartOf = psStartOf ps
    };
    
    phaseDivideBy :: (Ord a) => PointSet a -> Phase a;
    phaseDivideBy ps = MkPhase
    {
        phaseSet = full,
        phaseStartOf = ps
    };
    
    phaseEmpty :: (DeltaSmaller a) => Phase a;
    phaseEmpty = toPhase empty;

    phaseFull :: (DeltaSmaller a) => Phase a;
    phaseFull = toPhase full;

    phaseMember :: (Ord a) => Phase a -> a -> Bool;
    phaseMember phase = member (phaseSet phase);
    
    phaseEndOf :: (DeltaSmaller a) => Phase a -> PointSet a;
    phaseEndOf phase = pointsLastUpTo (psEndOf (phaseSet phase)) (phaseStartOf phase);
    
    phaseInvert :: (Ord a) => Phase a -> Phase a;
    phaseInvert phase = MkPhase
    {
        phaseSet = invert (phaseSet phase),
        phaseStartOf = empty -- WRONG
    };
    
    phaseUnion :: (Ord a) => Phase a -> PhaseSet a -> Phase a;
    phaseUnion phase ps = MkPhase
    {
        phaseSet = union (phaseSet phase) ps,
        phaseStartOf = phaseStartOf phase
    };
    
    phaseIntersect :: (Ord a) => Phase a -> PhaseSet a -> Phase a;
    phaseIntersect phase ps = MkPhase
    {
        phaseSet = intersect (phaseSet phase) ps,
        phaseStartOf = mixedIntersect ps (phaseStartOf phase)
    };

    phaseOf :: (Ord a,?first :: a,?last :: a) => Phase a -> PointSet a -> Phase a;
    phaseOf phase ps = let
    {
        betweenIntervals = intervalsOf ps (phaseStartOf phase);
    } in MkPhase
    {
        phaseSet = intersect (phaseSet phase) (toPhaseSet betweenIntervals),
        phaseStartOf = mixedIntersect betweenIntervals (phaseStartOf phase)
    };

    phaseNthFrom :: (Ord a,?first :: a) => Int -> Phase a -> PointSet a -> Phase a;
    phaseNthFrom n psubject pdelimiter = phaseIntersect psubject
        (toPhaseSet (fmap ((==) (Just n)) (sfCountSince pdelimiter (phaseStartOf psubject))));

    phaseNthIn :: (Ord a,?first :: a) => Int -> Phase a -> Phase a -> Phase a;
    phaseNthIn n psubject pdelimiter = phaseIntersect psubject
        (toPhaseSet (fmap ((==) (Just n)) (sfCountSince (phaseStartOf pdelimiter) (phaseStartOf psubject))));
}
