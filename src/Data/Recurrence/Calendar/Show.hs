module Data.Recurrence.Calendar.Show(outputCalendar,printCalendar) where
{
    import Data.List;
    import Data.Time;
    import Data.Recurrence.Time;
    import Data.Recurrence.Gregorian;
    import Data.Recurrence.Interval;
    import Data.Recurrence.Calendar.Item;
    import Data.Recurrence.Calendar.Event;

    mergeByPairPresorted :: (a -> a -> Ordering) -> [a] -> [a] -> [a];
    mergeByPairPresorted _cmp aa [] = aa;
    mergeByPairPresorted _cmp [] bb = bb;
    mergeByPairPresorted cmp aa@(a:as) bb@(b:bs) = case cmp a b of
    {
        GT -> b:(mergeByPairPresorted cmp aa bs);
        _ -> a:(mergeByPairPresorted cmp as bb);
    };

    mergeByListPresorted :: (a -> a -> Ordering) -> [[a]] -> [a];
    mergeByListPresorted _cmp [] = [];
    mergeByListPresorted cmp (list:lists) = mergeByPairPresorted cmp list (mergeByListPresorted cmp lists);

    compareEvents :: (Ord a) => Event a -> Event a -> Ordering;
    compareEvents (MkEvent _ i1) (MkEvent _ i2) = compare i1 i2;

    groupByFunc :: (Eq b) => (a -> b) -> [a] -> [(b,[a])];
    groupByFunc _f [] = [];
    groupByFunc f aa@(a:_) = let
    {
        b = f a;
        (matching,rest) = span (\a' -> f a' == b) aa;
    } in (b,matching):(groupByFunc f rest);

    getStartTime :: Event T -> Maybe T;
    getStartTime (MkEvent _ (MkInterval mt _)) = mt;

    pad2 :: String -> String;
    pad2 [] = "00";
    pad2 s@[_] = '0':s;
    pad2 s = s;

    show2 :: Int -> String;
    show2 = pad2 . show;

    showTimeOfDay :: TimeOfDay -> String;
    showTimeOfDay tod | todSec tod == 0 = (show2 (todHour tod)) ++ ":" ++ (show2 (todMin tod));
    showTimeOfDay tod = show tod;

    showMonthDay :: Day -> String;
    showMonthDay day = case toGregorian day of
    {
        (_,m,d) -> "-" ++ (show2 m) ++ "-" ++ (show2 d);
    };

    showEnd :: (?context :: T) => T -> String;
    showEnd e | localDay ?context == localDay e = show (localTimeOfDay e);
    showEnd e | yearOfDay (localDay ?context) == yearOfDay (localDay e) = (showMonthDay (localDay e)) ++ " " ++ (show (localTimeOfDay e));
    showEnd e = show e;

    newContext :: (?context :: a) => Maybe a -> a;
    newContext (Just s) = s;
    newContext Nothing = ?context;

    showMaybeEnd :: (?context :: T) => Maybe T -> String;
    showMaybeEnd Nothing = "whenever";
    showMaybeEnd (Just t) = showEnd t;

    showEnding :: (?context :: T) => Interval T -> String;
    showEnding (MkInterval (Just s) (Just e)) | s == e = "";
    showEnding (MkInterval start end) = let
    {
        ?context = newContext start;
    } in " (until " ++ (showMaybeEnd end) ++ ")";

    showEvent :: (?context :: T) => Event T -> String;
    showEvent (MkEvent name interval) = name ++ (showEnding interval);

    isWholeDayStart :: Maybe T -> Maybe (Maybe Day);
    isWholeDayStart (Just t) | localTimeOfDay t == midnight = Just (Just (localDay t));
    isWholeDayStart Nothing = Just Nothing;
    isWholeDayStart _ = Nothing;

    isWholeDayEnd :: Maybe T -> Maybe (Maybe Day);
    isWholeDayEnd (Just t) | localTimeOfDay t == midnight = Just (Just (localDay t));
    isWholeDayEnd Nothing = Just Nothing;
    isWholeDayEnd _ = Nothing;

    isWholeDayInterval :: Interval T -> Maybe (Maybe Day,Maybe Day);
    isWholeDayInterval (MkInterval (Just start) (Just end)) | start == end = Nothing;
    isWholeDayInterval (MkInterval start end) = do
    {
        smday <- isWholeDayStart start;
        emday <- isWholeDayEnd end;
        return (smday,emday);
    };

    isWholeDayEvent :: Event T -> Maybe (Maybe Day,Maybe Day);
    isWholeDayEvent (MkEvent _ interval) = isWholeDayInterval interval;

    filterMaybe :: (a -> Maybe b) -> [a] -> ([(b,a)],[a]);
    filterMaybe _ [] = ([],[]);
    filterMaybe f (a:as) = let
    {
        (rj,rn) = filterMaybe f as;
    } in case f a of
    {
        Just b -> ((b,a):rj,rn);
        Nothing -> (rj,a:rn);
    };

    showWDInterval :: Integer -> Maybe Day -> Maybe Day -> String;
    showWDInterval _year (Just start) (Just end) | end == addDays 1 start = "";
    showWDInterval year _ (Just end) | year == yearOfDay end = " (to "++(showMonthDay (addDays (-1) end))++")";
    showWDInterval _ _ (Just end) = " (to "++(show (addDays (-1) end))++")";
    showWDInterval _ _ Nothing = " (to whenever)";

    showWDEvent :: (?context :: T) => ((Maybe Day,Maybe Day),Event T) -> String;
    showWDEvent ((mstart,mend),MkEvent name _) = " " ++ name ++ (showWDInterval (yearOfDay (case mstart of
    {
        Just start -> start;
        _ -> localDay ?context;
    })) mstart mend);

    outputYearHeader :: (Monad m,?output :: String -> m (),?context :: T) => Maybe Integer -> m ();
    outputYearHeader Nothing = return ();
    outputYearHeader (Just year) | yearOfDay (localDay ?context) == year = return ();
    outputYearHeader (Just year) = ?output $ ((show year) ++ "-") ++ "\n";

    showDayHeader :: Maybe Day -> String;
    showDayHeader Nothing = "ongoing";
    showDayHeader (Just day) = showMonthDay day;

    outputEvents :: (Monad m,?output :: String -> m (),?context :: T) => [Event T] -> m ();
    outputEvents events = mapM_ (\(myear,yearevents) -> do
    {
        outputYearHeader myear;
        mapM_ (\(mday,dayevents) -> do
        {
            let
            {
                (wdevents,otherevents) = filterMaybe isWholeDayEvent dayevents;
            };
            ?output $ showDayHeader mday;
            ?output $ (intercalate "," (fmap showWDEvent wdevents)) ++ "\n";
            mapM_ (\(mtod,timeevents) -> do
            {
                case mtod of
                {
                    Just tod -> ?output ((showTimeOfDay tod) ++ " ");
                    Nothing -> return ();
                };
                ?output $ (intercalate ", " (fmap showEvent timeevents)) ++ "\n";
            }) (groupByFunc ((fmap localTimeOfDay) . getStartTime) otherevents);
        }) (groupByFunc ((fmap localDay) . getStartTime) yearevents);
    }) (groupByFunc ((fmap (yearOfDay . localDay)) . getStartTime) events);

    outputCalendar :: (Monad m,?output :: String -> m ()) => T -> T -> Calendar -> m ();
    outputCalendar t limit items = let {?context = t} in outputEvents events where
    {
        events = mergeByListPresorted compareEvents (fmap (\phase -> allEvents phase t limit) items);
    };

    printCalendar :: T -> T -> Calendar -> IO ();
    printCalendar = let {?output = putStr} in outputCalendar;
}
