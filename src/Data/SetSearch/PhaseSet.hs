module Data.SetSearch.PhaseSet where
{
    import Control.Monad;
    import Data.SetSearch.Set;
    import Data.SetSearch.PointSet;
    import Data.SetSearch.Intervals;
    import Data.SetSearch.StepFunction;
    import Data.SetSearch.DeltaSmaller;
    import Data.SetSearch.Cut;

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
    
    instance BasedOn (PhaseSet a) where
    {
        type Base (PhaseSet a) = a;
    };
    
    instance RemapBase (PhaseSet a) (PhaseSet b) where
    {
        remapBase ab ba psa = MkPhaseSet
        {
            psIntervals = remapBase ab ba (psIntervals psa),
            psExceptions = remapBase ab ba (psExceptions psa)
        };
    };
    
    -- union, intersect and diff checked by test/TestPhaseSet
    instance (Ord a) => Set (PhaseSet a) where
    {
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

    cutCurrent :: (DeltaSmaller a) => PhaseSet a -> Cut a -> Bool;
    cutCurrent phase (MkCut a False) = (member (sfChanges (psIntervals phase)) a) /= (member (psExceptions phase) a);
    cutCurrent phase (MkCut a True) = member (psExceptions phase) a;

    cutAfterMember :: (DeltaSmaller a) => PhaseSet a -> Cut a -> Bool;
    cutAfterMember phase (MkCut a False) = member phase a;
    cutAfterMember phase (MkCut a True) = (member phase a) /= (member (psExceptions phase) a);

    cutFirstAfterUntil :: (DeltaSmaller a) => PhaseSet a -> Cut a -> a -> Maybe (Cut a);
    cutFirstAfterUntil phase (MkCut a ff) limit = if ff then nextCut else (mplus thisCut nextCut) where
    {
        thisCut = if member (psExceptions phase) a then Just (MkCut a True) else Nothing;
    
        nextCut = let
        {
            mrI = pointsFirstAfterUntil (sfChanges (psIntervals phase)) a limit;
            mrX = pointsFirstAfterUntil (psExceptions phase) a limit;
        } in case (mrI,mrX) of
        {
            (Just rI,Just rX) -> Just (case compare rI rX of
            {
                LT -> MkCut rI False;
                EQ -> MkCut rI True;
                GT -> MkCut rX False;
            });
            (Just rI,Nothing) -> Just (MkCut rI False);
            (Nothing,Just rX) -> Just (MkCut rX False);
            (Nothing,Nothing) -> Nothing;
        }
    };  
    
    cutNextInterval :: forall a. (DeltaSmaller a) => PhaseSet a -> Cut a -> a -> Maybe (Interval a);
    cutNextInterval phase cut limit = if cutAfterMember phase cut
     then Just (MkInterval
        (if cutCurrent phase cut then Starts cut else Ongoing)
        (getEnd cut)
     )
     else do
    {
        startcut <- cutFirstAfterUntil phase cut limit;
        return (MkInterval (Starts startcut) (getEnd startcut));
    } where
    {
        getEnd c = case cutFirstAfterUntil phase c limit of
        {
            Just end -> Ends end;
            Nothing -> Whenever;
        };
    };

    psStartOf :: (DeltaSmaller a) => PhaseSet a -> PointSet a;
    psStartOf ps = union (psExceptions ps) (intervalsStartOf (psIntervals ps));

    psEndOf :: (DeltaSmaller a) => PhaseSet a -> PointSet a;
    psEndOf ps = union (psExceptions ps) (intervalsEndOf (psIntervals ps));
}
