module Examples(makeTests) where
{
    import Data.Maybe;
    import System.FilePath;
    import System.Directory;
    import Test.Tasty;
    import Test.Tasty.HUnit;
    import Data.Recurrence.Time;
    import Data.Recurrence.Calendar;


    runFile :: FilePath -> IO ();
    runFile fpath = do
    {
        now <- getNow;
        _calendar <- let {?now = now} in calendarFromFile fpath;
        return ();
    };

    getTest :: String -> Maybe TestTree;
    getTest "." = Nothing;
    getTest ".." = Nothing;
    getTest s = Just $ testCase s $ runFile $ "calendars" </> s;

    makeTests :: IO TestTree;
    makeTests = do
    {
        files <- listDirectory "calendars";
        return $ localOption (mkTimeout 1000000) $ testGroup "examples" $ mapMaybe getTest files;
    };
}
