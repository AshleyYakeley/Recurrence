module Data.SetSearch.PhaseSet where
{
{-
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

    psStartOf :: (DeltaSmaller a) => PhaseSet a -> PointSet a;
    psStartOf ps = union (psExceptions ps) (intervalsStartOf (psIntervals ps));

    psEndOf :: (DeltaSmaller a) => PhaseSet a -> PointSet a;
    psEndOf ps = union (psExceptions ps) (intervalsEndOf (psIntervals ps));
-}
}
