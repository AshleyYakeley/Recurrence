{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, ImplicitParams #-}
module Main where
{
    import Data.SetSearch;
    import Data.Time;
    import Data.TimePhase;
    import Data.TimePhase.Time;
    
    midnights :: PointSet T;
    midnights = timeOfDay midnight;

    check :: (Show a,Eq a) => String -> a -> a -> IO ();
    check _ expected found | expected == found = return ();
    check text expected found = putStrLn (text ++ ": expected " ++ (show expected) ++ ", found " ++ (show found));

    t0 :: T;
    t0 = dayMidnight (ModifiedJulianDay 0);

    t1 :: T;
    t1 = dayMidnight (ModifiedJulianDay 1);

    t2 :: T;
    t2 = dayMidnight (ModifiedJulianDay 2);

    main :: IO ();
    main = let {?first = firstTime} in do
    {
        check "is" True (member midnights t0);
        check "forward" (Just t1) (firstAfterUntil midnights t0 t1);
        check "backward" (Just t1) (lastBeforeUntil midnights t2 t1);
        
        check "onAndOff1" False (psOnAndOff False (single t0) (single t0) (justAfter t0));
        check "onAndOff2" True (psOnAndOff False (single t0) (single t2) (justAfter t0));
        check "onAndOff3" True (psOnAndOff False (single t0) (single t2) (justAfter t1));
        check "onAndOff4" False (psOnAndOff False (single t0) (single t2) (justAfter t2));
    };
}
