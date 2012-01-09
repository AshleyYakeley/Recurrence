module Main where
{
    import System.IO;
    import System.Environment;
    import Data.TimePhase;

    doHandle :: Handle -> IO ();
    doHandle h = do
    {
        return ();
    };

    main :: IO ();
    main = do
    {
        args <- getArgs;
        case args of
        {
            [] -> doHandle stdin;
            _ -> mapM_ (\arg -> withFile arg ReadMode doHandle) args;
        };
    };
}
