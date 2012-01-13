module Main where
{
    import System.IO;
    import System.Environment;
    import Data.Time;
    import Text.ParserCombinators.ReadPrec;
    import Language.SExpression;
    import Data.TimePhase;
    import Data.TimePhase.Value;
    import Data.SetSearch;

    process :: (Monad m) => ReadS a -> m (Maybe String) -> (a -> m ()) -> String -> m String;
    process interpret source sink = p where
    {
        p initial = case interpret initial of
        {
            (a,rest):_ -> do
            {
                sink a;
                p rest;
            };
            [] -> do
            {
                mmore <- source;
                case mmore of
                {
                    Just more -> p (initial ++ more);
                    Nothing -> return initial;
                };
            };
        };
    };

    searchTime :: NominalDiffTime;
    searchTime = 365.25 * 86400;

    showPoints :: T -> PointSet T -> String;
    showPoints now set = case ssFirstAfterUntil set now (addT searchTime now) of
    {
        Just t -> show t;
        Nothing -> "not in the next year"
    };
    
    showPhase :: T -> Phase T -> String;
    showPhase now ps = if member ps now then "happening" else "not happening";
{-    
    (IntervalsPhase ints) = 
     if member ints now 
        then "happening until " ++ (showPoints now (intervalsEndOf ints))
        else "not happening until " ++ (showPoints now (intervalsStartOf ints));
    showPhase now (PointSetPhase (PointPCPSet set)) = "next happening " ++ (showPoints now set);
    showPhase now (PointSetPhase (CoPointPCPSet set)) = "next not happening " ++ (showPoints now set);
-}    
    showValue :: T -> Value -> String;
    showValue now (PhaseValue phase) = showPhase now phase;
    showValue _ _ = "not a phase";

    makeSource :: Handle -> IO (Maybe String);
    makeSource h = do
    {
        eof <- hIsEOF h;
        if eof then return Nothing else do
        {
            text <- hGetLine h;
            return (Just text);
        };
    };

    doMValue :: T -> M Value -> IO ();
    doMValue _ (Left err) = fail err;
    doMValue now (Right v) = putStrLn (showValue now v);

    getLeftover :: String -> String;
    getLeftover s = case readPrec_to_S readAnyWhiteSpace 0 s of
    {
        (a,rest):_ -> rest;
        [] -> s;
    };

    doHandle :: T -> Handle -> IO ();
    doHandle now h = do
    {
        rest <- process (readPrec_to_S readValue 0) (makeSource h) (doMValue now) "";
        case rest of
        {
            [] -> return ();
            _ -> fail ("unreadable: " ++ (show rest));
        };
    };

    main :: IO ();
    main = do
    {
        args <- getArgs;
        now <- getNow;
        case args of
        {
            [] -> doHandle now stdin;
            _ -> mapM_ (\arg -> withFile arg ReadMode (doHandle now)) args;
        };
    };
}
