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
        psExceptions :: PointSet a
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
            psExceptions = ps
        };
    };
    
    instance (Ord a) => ToPhaseSet (Intervals a) where
    {
        toPhaseSet ints = MkPhaseSet
        {
            psIntervals = ints,
            psExceptions = empty
        };
    };
    
    -- union, intersect and diff checked by test/TestPhaseSet
    instance (Ord a) => Set (PhaseSet a) where
    {
        type Base (PhaseSet a) = a;
        empty = MkPhaseSet
        {
            psIntervals = empty,
            psExceptions = empty
        };
        member ps a = (member (psIntervals ps) a) /= (member (psExceptions ps) a);
        union (MkPhaseSet i1 x1) (MkPhaseSet i2 x2) = let
        {
            ir = union i1 i2;
        } in MkPhaseSet
        {
            psIntervals = ir,
            psExceptions = unionAll
            [
                intervalsDiff (union x1 x2) ir,
                intervalsIntersect (intersect x1 x2) (intersect i1 i2),
                intervalsIntersect (diff x1 x2) (diff i1 i2),
                intervalsIntersect (diff x2 x1) (diff i2 i1)
            ]
        };
        intersect (MkPhaseSet i1 x1) (MkPhaseSet i2 x2) = let
        {
            ir = intersect i1 i2;
        } in MkPhaseSet
        {
            psIntervals = ir,
            psExceptions = unionAll
            [
                intervalsDiff (intersect x1 x2) (union i1 i2),
                intervalsIntersect (diff x1 x2) (diff i2 i1),
                intervalsIntersect (diff x2 x1) (diff i1 i2),
                intervalsIntersect (union x1 x2) ir
            ]
        };
        diff (MkPhaseSet i1 x1) (MkPhaseSet i2 x2) =  let
        {
            ir = diff i1 i2;
        } in MkPhaseSet
        {
            psIntervals = ir,
            psExceptions = unionAll
            [
                intervalsIntersect (intersect x1 x2) (diff i2 i1),
                intervalsDiff (diff x1 x2) (union i2 i1),
                intervalsIntersect (diff x2 x1) (intersect i1 i2),
                intervalsIntersect (union x1 x2) ir
            ]
        };
        symdiff ps1 ps2 = MkPhaseSet
        {
            psIntervals = symdiff (psIntervals ps1) (psIntervals ps2),
            psExceptions = symdiff (psExceptions ps1) (psExceptions ps2)
        };
    };
    
    instance (Ord a) => SetSingle (PhaseSet a) where
    {
        single a = MkPhaseSet
        {
            psIntervals = empty,
            psExceptions = single a
        };
    };
    
    instance (Ord a) => SetFull (PhaseSet a) where
    {
        full = MkPhaseSet
        {
            psIntervals = full,
            psExceptions = empty
        };
        invert ps = MkPhaseSet
        {
            psIntervals = invert (psIntervals ps),
            psExceptions = psExceptions ps
        };
    };

    data EventType = ETChange | ETLateChange | ETPoint deriving Eq;
    
    eventFirstAfterUntil :: (DeltaSmaller a) => PhaseSet a -> a -> a -> Maybe (a,EventType,Bool);
    eventFirstAfterUntil ps t limit = let
    {
        current = member ps t;
        mrI = ssFirstAfterUntil (sfChanges (psIntervals ps)) t limit;
        mrX = ssFirstAfterUntil (psExceptions ps) t limit;
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
        mrX = ssFirstAfterUntil (psExceptions ps) t limit;
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
