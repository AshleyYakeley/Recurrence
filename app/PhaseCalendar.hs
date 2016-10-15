module Main where
{
    import System.IO;
    import System.Environment;
    import Text.Read;
    import Data.TimePhase.Time;
    import Data.TimePhase;
    import Data.SetSearch;

    doHandle :: (?now :: T) => Handle -> IO [Item];
    doHandle h = do
    {
        text <- hGetContents h;
        case readPrec_to_S readItems 0 text of
        {
            [(mitems,"")] -> case mitems of
            {
                Left err -> fail err;
                Right items -> return items;
            };
            [(_,rest)] -> fail ("unreadable: " ++ (show rest));
            _ -> fail ("unreadable: " ++ (show text));
        };
    };

    matchArgs :: [String] -> IO ((Maybe T, Maybe Int), [String]);
    matchArgs [] = return ((Nothing,Nothing),[]);
    matchArgs ("--start":t:args) = do
    {
        ((_,days),files) <- matchArgs args;
        case runRead readPrec t of
        {
            Just value -> return ((Just value,days),files);
            _ -> fail ("bad argument: " ++ (show t));
        };
    };
    matchArgs ("--days":t:args) = do
    {
        ((start,_),files) <- matchArgs args;
        case runRead readPrec t of
        {
            Just value -> return ((start,Just value),files);
            _ -> fail ("bad argument: " ++ (show t));
        };
    };
    matchArgs (s@('-':_):_) = fail ("bad argument: " ++ (show s));
    matchArgs (f:args) = do
    {
        (opts,files) <- matchArgs args;
        return (opts,f:files);
    };

    main :: IO ();
    main = do
    {
        now <- getNow;
        args <- getArgs;
        ((mtime,mdays),filepaths) <- matchArgs args;
        start <- case mtime of
        {
            Just time -> return time;
            Nothing -> return now;
        };
        end <- return (case mdays of
        {
            Just days -> addAffine ((fromIntegral days) * nominalDayLength) start;
            Nothing -> addAffine (60 * nominalDayLength) start;
        });
        itemlists <- let {?now = now} in mapM (\filepath -> withFile filepath ReadMode (doHandle)) filepaths;
        printCalendar start end (concat itemlists);
    };
}
