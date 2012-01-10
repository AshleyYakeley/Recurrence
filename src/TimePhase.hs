module Main where
{
    import System.IO;
    import System.Environment;
    import Data.Time;
    import Text.ParserCombinators.ReadPrec;
    import Language.SExpression;
    import Data.TimePhase;

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

    runValue :: UTCTime -> Value -> String;
    runValue now v = "TEST";

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

    doMValue :: UTCTime -> M Value -> IO ();
    doMValue _ (Left err) = fail err;
    doMValue now (Right v) = putStrLn (runValue now v);

    getLeftover :: String -> String;
    getLeftover s = case readPrec_to_S readAnyWhiteSpace 0 s of
    {
        (a,rest):_ -> rest;
        [] -> s;
    };

    doHandle :: UTCTime -> Handle -> IO ();
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
        now <- getCurrentTime;
        case args of
        {
            [] -> doHandle now stdin;
            _ -> mapM_ (\arg -> withFile arg ReadMode (doHandle now)) args;
        };
    };
}
