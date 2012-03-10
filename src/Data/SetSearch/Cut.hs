module Data.SetSearch.Cut where
{
    import Prelude hiding (id,(.));
    import Control.Category;
    import Data.SetSearch.Set;
    import Data.SetSearch.DeltaSmaller;

    class (BasedOn s) => ShowBasedOn s where
    {
        showBasedOn :: (Base s -> String) -> s -> String;
    };

    data Side = Before | After deriving Eq;

    instance Show Side where
    {
        show Before = "before"; 
        show After = "after"; 
    };
    
    -- False means "just before", True means "just after".
    data Cut a = MkCut a Side deriving Eq;
    
    justBefore :: a -> Cut a;
    justBefore a = MkCut a Before;
    
    justAfter :: a -> Cut a;
    justAfter a = MkCut a After;
    
    doubleCut :: a -> Cut (Cut a);
    doubleCut a = MkCut (MkCut a After) Before;
    
    instance (Ord a) => Ord (Cut a) where
    {
        compare (MkCut a1 x1) (MkCut a2 x2) = let
        {
            ac = compare a1 a2;
        } in if ac == EQ then (case (x1, x2) of
        {
            (Before,After) -> LT;
            (After,Before) -> GT;
            _ -> EQ;
        }) else ac;
    };

    instance (DeltaSmaller a) => DeltaSmaller (Cut a) where
    {
        deltaSmaller (MkCut a After) = Just (MkCut a Before);
        deltaSmaller (MkCut a Before) = fmap (\a' -> MkCut a' After) (deltaSmaller a);
    };

    instance BasedOn (Cut a) where
    {
        type Base (Cut a) = a;
    };
    
    instance RemapBase (Cut a) (Cut b) where
    {
        remapBase ab _ (MkCut a x) = MkCut (ab a) x;
    };

    instance (Show a) => Show (Cut a) where
    {
        show (MkCut a x) = (show x) ++ " " ++ (show a); 
    };

    class (Ord a,Ord b) => Interleaved a b where
    {
        greaterThan :: a -> b -> Bool;
    };

    instance (Ord a) => Interleaved a (Cut a) where
    {
        greaterThan a (MkCut a' Before) = a >= a';
        greaterThan a (MkCut a' After) = a > a';
    };

    instance (Ord a) => Interleaved (Cut a) a where
    {
        greaterThan (MkCut a Before) a' = a > a';
        greaterThan (MkCut a After) a' = a >= a';
    };


    data Start a = Ongoing | Starts (Cut a);

    instance (Eq a) => Eq (Start a) where
    {
        Ongoing == Ongoing = True;
        (Starts a1) == (Starts a2) = a1 == a2;
        _ == _ = False;
    };

    instance (Ord a) => Ord (Start a) where
    {
        compare Ongoing Ongoing = EQ;
        compare Ongoing (Starts _) = LT;
        compare (Starts _) Ongoing = GT;
        compare (Starts a1) (Starts a2) = compare a1 a2;
    };

    instance BasedOn (Start a) where
    {
        type Base (Start a) = a;
    };

    instance ShowBasedOn (Start a) where
    {
        showBasedOn _ Ongoing = "ongoing";
        showBasedOn show (Starts (MkCut a Before)) = show a;
        showBasedOn show (Starts (MkCut a After)) = "just after " ++ (show a); 
    };

    instance (Show a) => Show (Start a) where
    {
        show = showBasedOn show;
    };
    
    data End a = Whenever | Ends (Cut a);

    instance (Eq a) => Eq (End a) where
    {
        Whenever == Whenever = True;
        (Ends a1) == (Ends a2) = a1 == a2;
        _ == _ = False;
    };

    instance (Ord a) => Ord (End a) where
    {
        compare Whenever Whenever = EQ;
        compare Whenever (Ends _) = GT;
        compare (Ends _) Whenever = LT;
        compare (Ends a1) (Ends a2) = compare a1 a2;
    };

    instance BasedOn (End a) where
    {
        type Base (End a) = a;
    };
    
    instance ShowBasedOn (End a) where
    {
        showBasedOn _ Whenever = "whenever";
        showBasedOn show (Ends (MkCut a Before)) = show a;
        showBasedOn show (Ends (MkCut a After)) = "including " ++ (show a);
    };

    instance (Show a) => Show (End a) where
    {
        show = showBasedOn show;
    };

    data Interval a = MkInterval (Start a) (End a);
    
    instance (Eq a) => Eq (Interval a) where
    {
        (MkInterval s1 e1) == (MkInterval s2 e2) = (s1 == s2) && (e1 == e2);
    };
    
    instance (Ord a) => Ord (Interval a) where
    {
        compare (MkInterval s1 e1) (MkInterval s2 e2) = let
        {
            sc = compare s1 s2;
        } in if sc == EQ then compare e1 e2 else sc;
    };

    instance (Show a) => Show (Interval a) where
    {
        show (MkInterval start end) = (show start) ++ " until " ++ (show end);
    };

    data MonotonicInjection a b = MkMonotonicInjection
    {
        projectForwards :: a -> b,
        projectBack :: b -> a
    };
    
    projectBackwards :: (Ord b) => MonotonicInjection a b -> b -> Either (Cut a) a;
    projectBackwards mi b = let
    {
        a = projectBack mi b; -- loses information
        ba = projectForwards mi a;
    } in case compare b ba of
    {
        LT -> Left (justBefore a);
        EQ -> Right a;
        GT -> Left (justAfter a);
    };

    instance Category MonotonicInjection where
    {
        id = MkMonotonicInjection
        {
            projectForwards = id,
            projectBack = id
        };
        (MkMonotonicInjection bc cb) . (MkMonotonicInjection ab ba) = MkMonotonicInjection
         (bc . ab) (ba . cb);
    };

    projectBefore :: MonotonicInjection a (Cut a);
    projectBefore = MkMonotonicInjection justBefore (\(MkCut a _) -> a);

    projectAfter :: MonotonicInjection a (Cut a);
    projectAfter = MkMonotonicInjection justAfter (\(MkCut a _) -> a);
}
