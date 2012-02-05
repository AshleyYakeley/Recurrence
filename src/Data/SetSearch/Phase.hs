module Data.SetSearch.Phase where
{
    import Control.Monad;
    import Data.SetSearch.Set;
    import Data.SetSearch.DeltaSmaller;
    import Data.SetSearch.PointSet;
    import Data.SetSearch.Intervals;
    import Data.SetSearch.StepFunction;
    import Data.SetSearch.Cut;
    
    data Phase a = MkPhase
    {
        phaseSet :: Intervals a,
        phaseStartOf :: PointSet (Cut a)
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
            phaseStartOf = remapBase (remapBase ab ba) (remapBase ba ab) (phaseStartOf phasea)
        };
    };
    
    phaseDivideBy :: (Ord a) => PointSet (Cut a) -> Phase a;
    phaseDivideBy ps = MkPhase
    {
        phaseSet = full,
        phaseStartOf = ps
    };
    
    phaseEmpty :: forall a. (DeltaSmaller a) => Phase a;
    phaseEmpty = toPhase (empty :: Intervals a);

    phaseFull :: forall a. (DeltaSmaller a) => Phase a;
    phaseFull = toPhase (full :: Intervals a);

    phaseMember :: (Ord a) => Phase a -> a -> Bool;
    phaseMember phase = member (phaseSet phase);

    phaseEndOf :: forall a. (DeltaSmaller a,?last :: Cut a) => Phase a -> PointSet (Cut a);
    phaseEndOf phase = let
    {
        intervals :: Intervals a;
        intervals = phaseSet phase;
        
        intervalsChanges :: PointSet (Cut a);
        intervalsChanges = sfChanges intervals;
        
        intEnds :: PointSet (Cut a);
        intEnds = intervalsEndOf intervals;
        
        psStarts :: PointSet (Cut a);
        psStarts = phaseStartOf phase;
        
        lastIntEnds :: PointSet (Cut a);
        lastIntEnds = pointsLastOnOrBeforePoints intEnds psStarts;
        
        markedEnds :: PointSet (Cut a);
        markedEnds = filterIntersect (\x@(MkCut v _) -> not (member intervalsChanges x) && (member intervals v)) psStarts;
    } in union lastIntEnds markedEnds;
    
--     pointsLastAndIncluding (intervalsEndOf (phaseSet phase)) (phaseStartOf phase);
    
    phaseInvert :: (Ord a) => Phase a -> Phase a;
    phaseInvert phase = MkPhase
    {
        phaseSet = invert (phaseSet phase),
        phaseStartOf = empty -- WRONG
    };
    
    phaseUnion :: (Ord a) => Phase a -> Intervals a -> Phase a;
    phaseUnion phase ps = MkPhase
    {
        phaseSet = union (phaseSet phase) ps,
        phaseStartOf = phaseStartOf phase
    };
    
    phaseIntersect :: (Ord a) => Phase a -> Intervals a -> Phase a;
    phaseIntersect phase ints = MkPhase
    {
        phaseSet = intersect (phaseSet phase) ints,
        phaseStartOf = intervalsIntersectCut ints (phaseStartOf phase)
    };

    phaseOf :: (Ord a,?first :: Cut a,?last :: Cut a) => Phase a -> PointSet a -> Phase a;
    phaseOf phase ps = phaseIntersect phase (intervalsOf ps (phaseStartOf phase));
{-
    phaseNthFrom :: (Ord a,?first :: a) => Int -> Phase a -> PointSet (Cut a) -> Phase a;
    phaseNthFrom n psubject pdelimiter = phaseIntersect psubject
        (fmap ((==) (Just n)) (sfCountSince pdelimiter (phaseStartOf psubject)));

    phaseNthIn :: (Ord a,?first :: a) => Int -> Phase a -> Phase a -> Phase a;
    phaseNthIn n psubject pdelimiter = phaseIntersect psubject
        (fmap ((==) (Just n)) (sfCountSince (phaseStartOf pdelimiter) (phaseStartOf psubject)));
-}

    phaseChanges :: (DeltaSmaller a) => Phase a -> PointSet (Cut a);
    phaseChanges phase = union (sfChanges (phaseSet phase)) (phaseStartOf phase);

    cutNextInterval :: forall a. (DeltaSmaller a) => Phase a -> Cut a -> Cut a -> Maybe (Interval a);
    cutNextInterval phase cut@(MkCut a _) limit = if sfUpwardValue (phaseSet phase) cut
     then Just (MkInterval
        (if member (phaseChanges phase) cut then Starts cut else Ongoing)
        (getEnd cut)
     )
     else do
    {
        startcut <- cutFirstAfterUntil phase cut limit;
        return (MkInterval (Starts startcut) (getEnd startcut));
    } where
    {
        cutFirstAfterUntil phase cut limit = firstAfterUntil (phaseChanges phase) cut limit;

        getEnd c = case cutFirstAfterUntil phase c limit of
        {
            Just end -> Ends end;
            Nothing -> Whenever;
        };
    };
    
    class (BasedOn s) => ToPhase s where
    {
        toPhase :: s -> Phase (Base s);
    };
    
    instance ToPhase (Phase a) where
    {
        toPhase = id;
    };
    
    instance (DeltaSmaller a) => ToPhase (Intervals a) where
    {   
        toPhase ints = MkPhase
        {
            phaseSet = ints,
            phaseStartOf = intervalsStartOf ints
        };
    };

    instance (DeltaSmaller a) => ToPhase (PointSet a) where
    {
        toPhase = toPhase . pointsToIntervals;
    };
}
