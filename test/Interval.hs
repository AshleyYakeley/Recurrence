module Interval(tests) where
{
    import Data.Time;
    import Data.Recurrence;
    import Data.Recurrence.Time;
    import Data.Recurrence.Interval;
    import Test.Tasty;
    import Test.Tasty.HUnit;

    midnights :: Recurrence;
    midnights = InstantRecurrence (isTimeOfDay midnight);

    dayMidnight :: Day -> T;
    dayMidnight localDay = let
    {
        localTimeOfDay = midnight;
    } in LocalTime{..};

    check :: (Show a,Eq a) => String -> a -> a -> TestTree;
    check name expected found = testCase name $ assertEqual "" expected found;

    t0 :: T;
    t0 = dayMidnight (ModifiedJulianDay 0);
{-
    t05 :: T;
    t05 = LocalTime (ModifiedJulianDay 0) midday;
-}
    t1 :: T;
    t1 = dayMidnight (ModifiedJulianDay 1);

    t2 :: T;
    t2 = dayMidnight (ModifiedJulianDay 2);

    pointInterval :: T -> Interval T;
    pointInterval p = MkInterval (Just p) (Just p);

    tests :: TestTree;
    tests = testGroup "item"
    [
        check "events" [pointInterval t0,pointInterval t1,pointInterval t2] (allIntervals midnights t0 t2)
    ];
}
