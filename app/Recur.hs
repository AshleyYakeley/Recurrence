module Main where
{
    import Options.Applicative;
    import Data.SetSearch;
    import Data.Recurrence;
    import Data.Recurrence.Time;
    import Data.Recurrence.Calendar;


    data Options = MkOptions
    {
        optStartTime :: Maybe T,
        optDays :: Int,
        optExpressions :: [String],
        optFilePaths :: [FilePath]
    };

    optParser :: Parser Options;
    optParser = let
    {
        startTimeParser :: Parser T;
        startTimeParser = option auto $
            long "start" <>
            metavar "TIME" <>
            help "start time (default now)";

        daysParser :: Parser Int;
        daysParser = option auto $
            long "days" <>
            metavar "DAYS" <>
            help "calendar length (default 60)" <>
            value 60;

        expressionParser :: Parser String;
        expressionParser = strOption $
            short 'e' <>
            long "expr" <>
            metavar "EXPR" <>
            help "recurrence expression";

        fileParser :: Parser FilePath;
        fileParser = argument str $
            metavar "FILE" <>
            help "recurrence file";
    } in MkOptions <$>
        optional startTimeParser <*>
        daysParser <*>
        many expressionParser <*>
        many fileParser;

    cmdParser :: ParserInfo Options;
    cmdParser = info (helper <*> optParser) $
        fullDesc <>
        header "recur - show calendar of recurring events" <>
        progDesc "Show calendar from TIME of length DAYS";

    main :: IO ();
    main = execParser cmdParser >>= \MkOptions{..} -> do
    {
        now <- getNow;
        let
        {
            start = case optStartTime of
            {
                Just time -> time;
                Nothing -> now;
            };
            end = addAffine ((fromIntegral optDays) * nominalDayLength) start;
        };
        calendar <- let {?now = now} in do
        {
            expressionCalendars <- mapM calendarFromString optExpressions;
            fileCalendars <- mapM calendarFromFile optFilePaths;
            return $ concat $ expressionCalendars ++ fileCalendars;
        };
        printCalendar start end calendar;
    };
}
