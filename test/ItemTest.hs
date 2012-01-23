{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
module Main where
{
    import Data.SetSearch;
    import Data.Time;
    import Data.TimePhase;
    import Data.TimePhase.Time;
    import Data.TimePhase.Item;
    
    midnights :: Item T;
    midnights = MkItem "midnight" (toPhase (timeOfDay midnight));



    check :: (Show a,Eq a) => String -> a -> a -> IO ();
    check _ expected found | expected == found = return ();
    check text expected found = putStrLn (text ++ ": expected " ++ (show expected) ++ ", found " ++ (show found));

    t0 :: T;
    t0 = dayMidnight (ModifiedJulianDay 0);

    t05 :: T;
    t05 = LocalTime (ModifiedJulianDay 0) midday;

    t1 :: T;
    t1 = dayMidnight (ModifiedJulianDay 1);

    t2 :: T;
    t2 = dayMidnight (ModifiedJulianDay 2);

    pointInterval p = MkInterval (Starts (MkCut p False)) (Ends (MkCut p True));

    expected :: T -> Event T;
    expected t = MkEvent "midnight" (pointInterval t);

    main :: IO ();
    main = do
    {
        check "events" [expected t0,expected t1,expected t2] (allEvents midnights (MkCut t0 False) (MkCut t2 True));
    };
}
