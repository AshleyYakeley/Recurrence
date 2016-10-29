module PointSet(tests) where
{
    import Data.Time;
    import Test.Tasty;
    import Test.Tasty.HUnit;
    import Data.SetSearch;
    import Data.Recurrence;
    import Data.Recurrence.Day;
    import Data.Recurrence.Time;

    midnights :: PointSet T;
    midnights = isTimeOfDay midnight;

    check :: (Show a,Eq a) => String -> a -> a -> TestTree;
    check name expected found = testCase name $ assertEqual "" expected found;

    dayMidnight :: Day -> T;
    dayMidnight localDay = let
    {
        localTimeOfDay = midnight;
    } in LocalTime{..};

    t0 :: T;
    t0 = dayMidnight (ModifiedJulianDay 0);

    t1 :: T;
    t1 = dayMidnight (ModifiedJulianDay 1);

    t2 :: T;
    t2 = dayMidnight (ModifiedJulianDay 2);

    pieceOnAndOff :: (?first :: t, Ord t) => Bool -> PointSet t -> PointSet t -> PieceFunction t Bool;
    pieceOnAndOff ambient ons offs = let
    {
        f (EBLeft ()) = True;
        f (EBRight ()) = False;
        f (EBBoth () ()) = ambient;
    } in pieceLatestPoint ambient $ fmap f $ pointEitherBoth ons offs;

    psOnAndOff :: (?first :: t, Ord t) => Bool -> PointSet t -> PointSet t -> t -> Bool;
    psOnAndOff ambient ons offs = pieceEval $ pieceOnAndOff ambient ons offs;

    tests :: TestTree;
    tests = let {?first = firstTime} in testGroup "PointSet"
    [
        check "is" True (member midnights t0),
        --check "forward" (Just t1) (firstAfterUntil midnights t0 t1),
        --check "backward" (Just t1) (lastBeforeUntil midnights t2 t1),

        check "onAndOff1" False (psOnAndOff False (single t0) (single t0) t0),
        check "onAndOff2" True (psOnAndOff False (single t0) (single t2) t0),
        check "onAndOff3" True (psOnAndOff False (single t0) (single t2) t1),
        check "onAndOff4" False (psOnAndOff False (single t0) (single t2) t2)
    ];
}
