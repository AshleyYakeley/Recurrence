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

    searchTime :: NominalDiffTime;
    searchTime = 365.25 * 86400;

    showPoints :: T -> PointSet T -> String;
    showPoints now set = case ssFirstAfterUntil set now (addT searchTime now) of
    {
        Just t -> show t;
        Nothing -> "not in the next year"
    };
    
    doHandle :: T -> Handle -> IO [Item];
    doHandle now h = do
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

    main :: IO ();
    main = do
    {
        filepaths <- getArgs;
        now <- getNow;
        itemlists <- mapM (\filepath -> withFile filepath ReadMode (doHandle now)) filepaths;
        showItems now (addT searchTime now) (concat itemlists);
    };
}
