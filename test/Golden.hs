module Golden(tests) where
{
    import System.IO;
    import Test.Tasty;
    import Test.Tasty.Golden;
    import Data.ByteString.Lazy;
    import Data.ByteString.Lazy.UTF8;
    import Data.Time;
    import Data.Recurrence.Calendar;

    runCalendar :: FilePath -> FilePath -> Integer -> IO ();
    runCalendar inputPath outputPath days = do
    {
        let
        {
            startDay :: Day;
            startDay = fromGregorian 2012 04 17; -- Tuesday; 21st is Saturday

            startTime :: LocalTime;
            startTime = LocalTime startDay midnight;

            testNow :: LocalTime;
            testNow = startTime;

            endTime :: LocalTime;
            endTime = LocalTime (addDays days $ localDay startTime) midnight;
        };
        calendar <- let {?now = testNow} in calendarFromFile inputPath;
        withFile outputPath WriteMode $ \h -> let
        {
            ?output = \str -> hPut h $ fromString str;
        } in outputCalendar startTime endTime calendar;
    };

    testfiles :: [(String,Integer)];
    testfiles =
    [
        ("between.instant",10),
        ("between.period",60),
        ("day",10),
        ("dateformats",3650),
        ("duration",60),
        ("easter",60),
        ("from-until",60),
        ("months",60),
        --("now",60),
        ("nth",60),
        ("of",60),
        ("startend",60),
        ("test",60),
        ("timeformats",7),
        ("to",60)
    ];

    getTest :: (String,Integer) -> TestTree;
    getTest (name,days) = let
    {
        path = "test/golden/"++name++".phases"
    } in goldenVsFile name (path++".ref") (path++".out") $ runCalendar path (path++".out") days;

    tests :: TestTree;
    tests = localOption (mkTimeout 1000000) $ testGroup "golden" $ fmap getTest testfiles;
}
