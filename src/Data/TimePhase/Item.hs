module Data.TimePhase.Item where
{
    import Data.List;
    import Data.Maybe;
    import Data.Time;
    import Text.ParserCombinators.ReadPrec;
    import Data.SetSearch;
    import Data.TimePhase.Time;
    import Data.SExpression;
    import Data.SExpression.Read;
    import Data.TimePhase.Atom;
    import Data.TimePhase.Read;
    import Data.TimePhase.Value;
    import Data.TimePhase.Eval;

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


    data Item = MkItem String TimePhase;

    readPhasesFile :: ReadPrec [SExpression Atom];
    readPhasesFile = do
    {
        exps <- readZeroOrMore readExpression;
        readAnyWhiteSpace;
        return exps;
    };

    interpretItem :: (?now :: T) => SExpression Atom -> M Item;
    interpretItem (ListSExpression [AtomSExpression (IdentifierAtom name),defn]) = do
    {
        value <- evalWithDict defn;
        phase <- fromValue value;
        return (MkItem name phase);
    };
    interpretItem _ = reportError "S-expression not in correct format";

    readItems :: (?now :: T) => ReadPrec (M [Item]);
    readItems = fmap (mapM interpretItem) readPhasesFile;

    data Interval a = MkInterval (Maybe a) (Maybe a) deriving Eq;

    instance (Show a) => Show (Interval a) where
    {
        show (MkInterval start end) = let
        {
            showIStart Nothing = "ongoing";
            showIStart (Just t) = show t;
            showIEnd Nothing = "whenever";
            showIEnd (Just t) = show t;
        } in (showIStart start) ++ " until " ++ (showIEnd end);
    };

    instance (Ord a) => Ord (Interval a) where
    {
        compare (MkInterval s1 e1) (MkInterval s2 e2) = let
        {
            compareStart Nothing Nothing = EQ;
            compareStart Nothing (Just _) = LT;
            compareStart (Just _) Nothing = GT;
            compareStart (Just a) (Just b) = compare a b;

            compareEnd Nothing Nothing = EQ;
            compareEnd Nothing (Just _) = GT;
            compareEnd (Just _) Nothing = LT;
            compareEnd (Just a) (Just b) = compare a b;

            sc = compareStart s1 s2;
        } in if sc == EQ then compareEnd e1 e2 else sc;
    };

    data Event a = MkEvent String (Interval a) deriving Eq;

    instance (Show a) => Show (Event a) where
    {
        show (MkEvent name int) = name ++ ": " ++ (show int);
    };

    instance BasedOn (Event a) where
    {
        type Base (Event a) = a;
    };


    pfNextInterval :: forall t count. (DeltaSmaller t,Eq count) => PiecePartialFunction t count -> t -> t -> Maybe (Interval t);
    pfNextInterval phase t0 limit = if isJust $ pieceEval (piecePartialToSet phase) t0
     then Just (MkInterval
        (if member (pieceChangeSet phase) t0 then Just t0 else Nothing)
        (getEnd t0)
     )
     else do
    {
        start <- getEnd t0;
        return (MkInterval (Just start) (getEnd start));
    } where
    {
        getEnd t = fmap fst $ pointClosestExcluding (pieceChangeSet phase) t limit;
    };

    -- data TimePhase = forall count. Eq count => PeriodTimeSet (PiecePartialFunction T count) | InstantTimeSet (PointSet T) | EmptyTimeSet;


    nextInterval :: TimePhase -> T -> T -> Maybe (Interval T);
    nextInterval EmptyTimeSet _ _ = Nothing;
    nextInterval (InstantTimeSet ps) t0 t1 = do
    {
        (t,_) <- pointClosestExcluding ps t0 t1;
        return $ MkInterval (Just t) (Just t);
    };
    nextInterval (PeriodTimeSet pf) t0 t1 = pfNextInterval pf t0 t1;

    intervalEnd :: Interval a -> Maybe a;
    intervalEnd (MkInterval _ me) = me;

    allIntervals :: TimePhase -> T -> T -> [Interval T];
    allIntervals tp cut limit = case nextInterval tp cut limit of
    {
        Nothing -> [];
        Just interval -> interval : (case intervalEnd interval of
        {
            Just end -> allIntervals tp end limit;
            Nothing -> [];
        });
    };

    allEvents :: Item -> T -> T -> [Event T];
    allEvents (MkItem name phase) cut limit = fmap (MkEvent name) $ allIntervals phase cut limit;

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

    printYearHeader :: (?context :: T) => Maybe Integer -> IO ();
    printYearHeader Nothing = return ();
    printYearHeader (Just year) | yearOfDay (localDay ?context) == year = return ();
    printYearHeader (Just year) = putStrLn ((show year) ++ "-");

    showDayHeader :: Maybe Day -> String;
    showDayHeader Nothing = "ongoing";
    showDayHeader (Just day) = showMonthDay day;

    printEvents :: (?context :: T) => [Event T] -> IO ();
    printEvents events = mapM_ (\(myear,yearevents) -> do
    {
        printYearHeader myear;
        mapM_ (\(mday,dayevents) -> do
        {
            let
            {
                (wdevents,otherevents) = filterMaybe isWholeDayEvent dayevents;
            };
            putStr (showDayHeader mday);
            putStrLn (intercalate "," (fmap showWDEvent wdevents));
            mapM_ (\(mtod,timeevents) -> do
            {
                case mtod of
                {
                    Just tod -> putStr ((showTimeOfDay tod) ++ " ");
                    Nothing -> return ();
                };
                putStrLn (intercalate ", " (fmap showEvent timeevents));
            }) (groupByFunc ((fmap localTimeOfDay) . getStartTime) otherevents);
        }) (groupByFunc ((fmap localDay) . getStartTime) yearevents);
    }) (groupByFunc ((fmap (yearOfDay . localDay)) . getStartTime) events);

    printCalendar :: T -> T -> [Item] -> IO ();
    printCalendar t limit items = let {?context = t} in printEvents events where
    {
        events = mergeByListPresorted compareEvents (fmap (\phase -> allEvents phase t limit) items);
    };
}
