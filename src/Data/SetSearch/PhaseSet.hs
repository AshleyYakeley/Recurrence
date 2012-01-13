module Data.SetSearch.PhaseSet where
{
    import Data.SetSearch.Set;
    import Data.SetSearch.PointSet;
    import Data.SetSearch.Intervals;
    import Data.SetSearch.StepFunction;
    import Data.SetSearch.DeltaSmaller;

    data PhaseSet a = MkPhaseSet
    {
        psIntervals :: Intervals a,
        psAdditions :: PointSet a,
        psDeletions :: PointSet a
    };
    
    class (Set s) => ToPhaseSet s where
    {
        toPhaseSet :: s -> PhaseSet (Base s);
    };
    
    instance (Ord a) => ToPhaseSet (PhaseSet a) where
    {
        toPhaseSet ps = ps;
    };
    
    instance (Ord a) => ToPhaseSet (PointSet a) where
    {
        toPhaseSet ps = MkPhaseSet
        {
            psIntervals = empty,
            psAdditions = ps,
            psDeletions = empty
        };
    };
    
    instance (Ord a) => ToPhaseSet (Intervals a) where
    {
        toPhaseSet ints = MkPhaseSet
        {
            psIntervals = ints,
            psAdditions = empty,
            psDeletions = empty
        };
    };
    
    -- union, intersect and diff checked by test/TestPhaseSet
    instance (Ord a) => Set (PhaseSet a) where
    {
        type Base (PhaseSet a) = a;
        empty = MkPhaseSet
        {
            psIntervals = empty,
            psAdditions = empty,
            psDeletions = empty
        };
        member ps a = if member (psIntervals ps) a then not (member (psDeletions ps) a) else member (psAdditions ps) a;
        union (MkPhaseSet i1 a1 d1) (MkPhaseSet i2 a2 d2) = let
        {
            ir = union i1 i2;
        } in MkPhaseSet
        {
            psIntervals = ir,
            psAdditions = intervalsDiff (union a1 a2) ir,
            psDeletions = union
                (intervalsIntersect (intersect d1 d2) (intersect i1 i2))
                (union
                    (intervalsIntersect (diff d1 a2) (diff i1 i2))
                    (intervalsIntersect (diff d2 a1) (diff i2 i1)))
        };
        intersect (MkPhaseSet i1 a1 d1) (MkPhaseSet i2 a2 d2) = let
        {
            ir = intersect i1 i2;
        } in MkPhaseSet
        {
            psIntervals = ir,
            psAdditions = union
                (intervalsDiff (intersect a1 a2) (union i1 i2))
                (union
                    (intervalsIntersect (diff a1 d2) (diff i2 i1))
                    (intervalsIntersect (diff a2 d1) (diff i1 i2))),
            psDeletions = intervalsIntersect (union d1 d2) ir
        };
        diff (MkPhaseSet i1 a1 d1) (MkPhaseSet i2 a2 d2) =  let
        {
            ir = diff i1 i2;
        } in MkPhaseSet
        {
            psIntervals = ir,
            psAdditions = union
                (intervalsIntersect (intersect a1 d2) (diff i2 i1))
                (union
                    (intervalsDiff (diff a1 a2) (union i2 i1))
                    (intervalsIntersect (diff d2 d1) (intersect i1 i2))),
            psDeletions = intervalsIntersect (union d1 a2) ir
        };
    };
    
    instance (Ord a) => SetSingle (PhaseSet a) where
    {
        single a = MkPhaseSet
        {
            psIntervals = empty,
            psAdditions = single a,
            psDeletions = empty
        };
    };
    
    instance (Ord a) => SetFull (PhaseSet a) where
    {
        full = MkPhaseSet
        {
            psIntervals = full,
            psAdditions = empty,
            psDeletions = empty
        };
        invert ps = MkPhaseSet
        {
            psIntervals = invert (psIntervals ps),
            psAdditions = psDeletions ps,
            psDeletions = psAdditions ps
        };
    };

    data EventType = ETChange | ETLateChange | ETPoint deriving Eq;
    
    eventFirstAfterUntil :: (DeltaSmaller a) => PhaseSet a -> a -> a -> Maybe (a,EventType,Bool);
    eventFirstAfterUntil ps t limit = let
    {
        current = member ps t;
        mrI = ssFirstAfterUntil (sfChanges (psIntervals ps)) t limit;
        mrA = ssFirstAfterUntil (psAdditions ps) t limit;
        mrD = ssFirstAfterUntil (psDeletions ps) t limit;
        mrX = if current then mrD else mrA;
    } in case (mrI,mrX) of
    {
        (Just rI,Just rX) -> Just (case compare rI rX of
        {
            LT -> (rI,ETChange,not current);
            EQ -> (rI,ETLateChange,not current);
            GT -> (rX,ETPoint,not current);
        });
        (Just rI,Nothing) -> Just (rI,ETChange,not current);
        (Nothing,Just rX) -> Just (rX,ETPoint,not current);
        (Nothing,Nothing) -> Nothing;
    };

    eventStateFirstAfterUntil :: (DeltaSmaller a) => PhaseSet a -> Bool -> a -> a -> Maybe (a,EventType);
    eventStateFirstAfterUntil ps state t limit = let
    {
        current = member ps t;
        mrI = ssFirstAfterUntil (sfMatchChanges (psIntervals ps) ((==) state)) t limit;
        mrA = ssFirstAfterUntil (psAdditions ps) t limit;
        mrD = ssFirstAfterUntil (psDeletions ps) t limit;
        mrX = if current then mrD else mrA;
    } in case (mrI,mrX) of
    {
        (Just rI,Just rX) -> Just (case compare rI rX of
        {
            LT -> (rI,ETChange);
            EQ -> (rI,ETLateChange);
            GT -> (rX,if current then ETLateChange else ETPoint);
        });
        (Just rI,Nothing) -> Just (rI,ETChange);
        (Nothing,Just rX) -> Just (rX,if current then ETLateChange else ETPoint);
        (Nothing,Nothing) -> Nothing;
    };
    
}
