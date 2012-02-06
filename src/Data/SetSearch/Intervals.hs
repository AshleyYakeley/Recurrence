module Data.SetSearch.Intervals where
{
    import Control.Applicative hiding (empty);
    import Data.SetSearch.Set;
    import Data.SetSearch.ValueSet;
    import Data.SetSearch.PointSet;
    import Data.SetSearch.DeltaSmaller;
    import Data.SetSearch.StepFunction;
    import Data.SetSearch.Cut;
    
    type Intervals a = StepFunction a Bool;
    
    instance (Ord a) => Set (Intervals a) where
    {
        empty = pure False;
        member = sfValue;
        union = liftA2 (||);
        intersect = liftA2 (&&);
        diff = liftA2 (\a b -> a && (not b));
        symdiff = liftA2 (\a b -> a /= b);
    };
    
    instance (Ord a) => SetFull (Intervals a) where
    {
        full = pure True;
        invert = fmap not;
    };
    
    intervalsIntersect :: (Ord a) => PointSet a -> Intervals a -> PointSet a;
    intervalsIntersect ps ints = MkPointSet (\p q -> let
    {
        pc = justBefore p;
        qc = justAfter q;    

        forwards xc | xc >= qc = [];
        forwards xc = case psFirstCut ps xc qc of
        {
            Just t1 | member ints t1 -> (retMarked t1):(forwards (justAfter t1));
            Just t1 | Just n <- psFirstCut (sfPossibleChanges ints) (doubleCut t1) (doubleCut q) -> forwards n;
            _ -> [];
        };
        
        backwards xc | xc <= qc = [];
        backwards xc = case psLastCut ps pc xc of
        {
            Just t1 | member ints t1 -> (retMarked t1):(backwards (justBefore t1));
            Just t1 | Just n <- psLastCut (sfPossibleChanges ints) (doubleCut p) (doubleCut t1) -> backwards n;
            _ -> [];
        };
    } in MkValueSet (forwards pc) (backwards qc));
  
    intervalsDiff :: (Ord a) => PointSet a -> Intervals a -> PointSet a;
    intervalsDiff p i = intervalsIntersect p (invert i);
    
    intervalsFromTo :: (Ord a,?first :: Cut a) => PointSet (Cut a) -> PointSet (Cut a) -> Intervals a;
    intervalsFromTo ps1 ps2 = MkStepFunction
    {
        sfUpwardValue = let {?first = justBefore ?first} in psOnAndOff ps1 ps2,
        sfPossibleChanges = union ps1 ps2
    };
    
    intervalsFromToInclusive :: (Ord a,?first :: Cut a) => PointSet (Cut a) -> PointSet (Cut a) -> Intervals a;
    intervalsFromToInclusive ps1 ps2 = invert (intervalsFromTo ps2 ps1);
    
    intervalsStartOf :: (DeltaSmaller a) => Intervals a -> PointSet (Cut a);
    intervalsStartOf i = sfMatchUpwardChanges i id;
    
    intervalsEndOf :: (DeltaSmaller a) => Intervals a -> PointSet (Cut a);
    intervalsEndOf i = sfMatchUpwardChanges i not;
    
    intervalsAfter :: (Ord a,?first :: Cut a) => PointSet (Cut a) -> Intervals a;
    intervalsAfter ps = MkStepFunction
    {
        sfUpwardValue = \x -> psNonEmpty ps (justBefore ?first) (justAfter x), 
        sfPossibleChanges = ps
    };

    intervalsOf :: (Ord a,?first :: Cut a,?last :: Cut a) => PointSet a -> PointSet (Cut a) -> Intervals a;
    intervalsOf subject delimiter =
     intervalsFromToInclusive (pointsCutLastBeforePoints delimiter subject) (pointsCutFirstAfterPoints delimiter subject);

    intervalsOneAfter :: (Ord a) => Cut a -> Intervals a;
    intervalsOneAfter start = MkStepFunction
    {
        sfUpwardValue = \t -> (t >= start),
        sfPossibleChanges = single start
    };

    intervalsOneBefore :: (Ord a) => Cut a -> Intervals a;
    intervalsOneBefore = invert . intervalsOneAfter;

    intervalsOne :: (Ord a) => Cut a -> Cut a -> Intervals a;
    intervalsOne start end = intersect (intervalsOneAfter start) (intervalsOneBefore end);

    intervalsOneAfterMaybe :: (Ord a) => Maybe (Cut a) -> Intervals a;
    intervalsOneAfterMaybe Nothing = full;
    intervalsOneAfterMaybe (Just a) = intervalsOneAfter a;

    intervalsOneBeforeMaybe :: (Ord a) => Maybe (Cut a) -> Intervals a;
    intervalsOneBeforeMaybe Nothing = full;
    intervalsOneBeforeMaybe (Just a) = intervalsOneBefore a;

    intervalsOneMaybe :: (Ord a) => Maybe (Cut a) -> Maybe (Cut a) -> Intervals a;
    intervalsOneMaybe mstart mend = intersect (intervalsOneAfterMaybe mstart) (intervalsOneBeforeMaybe mend);
    
    pointsToIntervals :: (Ord a) => PointSet a -> Intervals a;
    pointsToIntervals ps = MkStepFunction
    {
        sfUpwardValue = \cut -> case cut of
        {
            MkCut a Before -> member ps a;
            _ -> False;
        },
        sfPossibleChanges = pointsCutBoth ps
    };
    
    instance (Ord a) => SetSingle (Intervals a) where
    {
        single a = intervalsOne (justBefore a) (justAfter a);
    };
}
