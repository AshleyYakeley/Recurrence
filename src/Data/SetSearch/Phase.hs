module Data.SetSearch.Phase where
{
    import Data.SetSearch.Set;
    import Data.SetSearch.DeltaSmaller;
    import Data.SetSearch.PointSet;
    import Data.SetSearch.PhaseSet;
    
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
        phaseStartOf = phaseStartOf phase
    };
}
