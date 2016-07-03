module Data.SetSearch.Intervals where
{
    import Control.Applicative hiding (empty);
    import Data.SetSearch.Set;
    import Data.SetSearch.PointFunction;
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
        diff = liftA2 (\a b -> a && (not b));
        intersect = liftA2 (&&);
        symdiff = liftA2 (/=);
    };
    
    instance (Ord a) => SetFull (Intervals a) where
    {
        full = pure True;
        invert = fmap not;
    };
    
    intervalsIntersect :: (Ord a) => PointFunction a p -> Intervals a -> PointFunction a p;
    intervalsIntersect pf ints = MkPointFunction
    {
        pfValue = \a -> do
        {
            p <- pfValue pf a;
            if member ints a then Just p else Nothing;
        },
        pfNextUntil = \near far -> let
        {
            fc = justAfter far;
            find nc = case pfFirstCut pf nc fc of
            {
                Just a | member ints a -> Just a;
                Just a | Just nn <- pfFirstCut (sfPossibleChanges ints) (doubleCut a) (doubleCut far) -> find nn;
                _ -> Nothing;
            }
        } in find (justAfter near),
        pfPrevUntil = \near far -> let
        {
            fc = justBefore far;
            find nc = case pfLastCut pf nc fc of
            {
                Just a | member ints a -> Just a;
                Just a | Just nn <- pfLastCut (sfPossibleChanges ints) (doubleCut a) (doubleCut far) -> find nn;
                _ -> Nothing;
            }
        } in find (justBefore near)
    };
    
{-   
    MkPointSet (\p q -> let
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
-}  
    intervalsDiff :: (Ord a) => PointSet a -> Intervals a -> PointSet a;
    intervalsDiff p i = intervalsIntersect p (invert i);
    
    intervalsFromTo :: (Ord a,?first :: Cut a) => Bool -> PointSet (Cut a) -> PointSet (Cut a) -> Intervals a;
    intervalsFromTo ambient ps1 ps2 = MkStepFunction
    {
        sfUpwardValue = let {?first = justBefore ?first} in \a -> psOnAndOff ambient ps1 ps2 (justAfter a),
        sfPossibleChanges = union ps1 ps2
    };
    
    intervalsFromToInclusive :: (Ord a,?first :: Cut a) => Bool -> PointSet (Cut a) -> PointSet (Cut a) -> Intervals a;
    intervalsFromToInclusive ambient ps1 ps2 = invert (intervalsFromTo (not ambient) ps2 ps1);
    
    intervalsStartOf :: (DeltaSmaller a) => Intervals a -> PointSet (Cut a);
    intervalsStartOf i = sfMatchUpwardChanges i id;
    
    intervalsEndOf :: (DeltaSmaller a) => Intervals a -> PointSet (Cut a);
    intervalsEndOf i = sfMatchUpwardChanges i not;
    
    intervalsAfter :: (Ord a,?first :: Cut a) => PointSet (Cut a) -> Intervals a;
    intervalsAfter ps = MkStepFunction
    {
        sfUpwardValue = \x -> pfNonEmpty ps (justBefore ?first) (justAfter x), 
        sfPossibleChanges = ps
    };

    intervalsOf :: (Ord a,?first :: Cut a,?last :: Cut a) => PointSet a -> PointSet (Cut a) -> Intervals a;
    intervalsOf subject delimiter =
     intervalsFromToInclusive False (pointsCutLastBeforePoints delimiter subject) (pointsCutFirstAfterPoints delimiter subject);

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
