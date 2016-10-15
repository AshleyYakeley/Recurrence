module Main(main) where
{
    import Data.IORef;
    import Test.Tasty;
    import Test.Tasty.Golden;
    import Data.ByteString.Lazy.UTF8;
    import Data.Time;
    import Data.TimePhase.Calendar;

    startTime :: LocalTime;
    startTime = LocalTime (fromGregorian 2012 04 17) midnight;

    endTime :: LocalTime;
    endTime = LocalTime (fromGregorian 2012 06 17) midnight;

    testNow :: LocalTime;
    testNow = startTime;

    runCalendar :: FilePath -> IO ByteString;
    runCalendar path = do
    {
        calendar <- let {?now = testNow} in calendarFromFile path;
        outputref <- newIORef mempty;
        let
        {
            ?output = \str -> do
            {
                r <- readIORef outputref;
                writeIORef outputref $ mappend r $ fromString str;
            };
        } in outputCalendar startTime endTime calendar;
        readIORef outputref;
    };

    testfiles :: [String];
    testfiles =
    [
        --"dateformats",
        "duration",
        "easter",
        "from-until",
        "months",
        --"now",
        "nth",
        "of",
        "startend",
        "test",
        --"timeformats",
        "to"
    ];

    getTest :: String -> TestTree;
    getTest name = goldenVsString name ("test/"++name++".phases.ref") $ runCalendar ("test/"++name++".phases");

    tests :: TestTree;
    tests = localOption (mkTimeout 5000000) $ testGroup "Interpolate" $ fmap getTest testfiles;

    main :: IO ();
    main = defaultMain tests;
}
