module Item(tests) where
{
    import Data.Time;
    import Data.TimePhase;
    import Data.TimePhase.Time;
    import Data.TimePhase.Day;
    import Data.TimePhase.Calendar;
    import Test.Tasty;
    import Test.Tasty.HUnit;

    midnights :: Item;
    midnights = MkItem "midnight" (InstantTimeSet (isTimeOfDay midnight));

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

    expectedEvent :: T -> Event T;
    expectedEvent t = MkEvent "midnight" (pointInterval t);

    tests :: TestTree;
    tests = testGroup "item"
    [
        check "events" [expectedEvent t0,expectedEvent t1,expectedEvent t2] (allEvents midnights t0 t2)
    ];
}
