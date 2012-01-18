module Main where
{
    import System.IO;
    import System.Environment;
    import Data.Time;
    import Text.Read;
    import Text.ParserCombinators.ReadPrec;
    import Data.TimePhase.Time;
    import Data.TimePhase.SExpression;
    import Data.TimePhase;
    import Data.TimePhase.Value;
    import Data.SetSearch;

    searchTime :: NominalDiffTime;
    searchTime = 60 * 86400;

    showPoints :: T -> PointSet T -> String;
    showPoints now set = case ssFirstAfterUntil set now (addAffine searchTime now) of
    {
        Just t -> show t;
        Nothing -> "not in the next year"
    };
    
    doHandle :: Handle -> IO [Item T];
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

    matchArgs [] = return (Nothing,[]);
    matchArgs ("--start":t:args) = do
    {
        (opts,files) <- matchArgs args;
        case runRead readPrec t of
        {
            Just time -> return (Just time,files);
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
        args <- getArgs;
        (mtime,filepaths) <- matchArgs args;
        time <- case mtime of
        {
            Just time -> return time;
            Nothing -> getNow;
        };
        itemlists <- mapM (\filepath -> withFile filepath ReadMode (doHandle)) filepaths;
        showItems time (addAffine searchTime time) (concat itemlists);
    };
}
