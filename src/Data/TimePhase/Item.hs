module Data.TimePhase.Item where
{
    import Data.List;
    import Data.Char;
    import Data.Maybe;
    import Control.Monad;
    import Data.Time;
    import Text.ParserCombinators.ReadPrec;
    import Data.SetSearch;
    import Data.TimePhase.Time;
    import Data.TimePhase.SExpression;
    import Data.TimePhase.SExpression.Read;
    import Data.TimePhase.Atom;
    import Data.TimePhase.Read;
    import Data.TimePhase.Value;
    import Data.TimePhase.Dict;
    import Data.TimePhase.Eval;

    mergeByPairPresorted :: (a -> a -> Ordering) -> [a] -> [a] -> [a];
    mergeByPairPresorted cmp aa [] = aa;
    mergeByPairPresorted cmp [] bb = bb;
    mergeByPairPresorted cmp aa@(a:as) bb@(b:bs) = case cmp a b of
    {
        GT -> b:(mergeByPairPresorted cmp aa bs);
        _ -> a:(mergeByPairPresorted cmp as bb);
    };
    
    mergeByListPresorted :: (a -> a -> Ordering) -> [[a]] -> [a];
    mergeByListPresorted cmp [] = [];
    mergeByListPresorted cmp (list:lists) = mergeByPairPresorted cmp list (mergeByListPresorted cmp lists);
    

    data Item a = MkItem String (PhaseSet a);
    
    readPhasesFile :: ReadPrec [SExpression Atom];
    readPhasesFile = do
    {
        exps <- readZeroOrMore readExpression;
        readAnyWhiteSpace;
        return exps;
    };
    
    interpretItem :: SExpression Atom -> M (Item T);
    interpretItem (ListSExpression [AtomSExpression (IdentifierAtom name),defn]) = do
    {
        value <- evalWithDict defn;
        phase <- fromValue value;
        return (MkItem name phase);
    };
    interpretItem _ = reportError "S-expression not in correct format";

    readItems :: ReadPrec (M [Item T]);
    readItems = fmap (mapM interpretItem) readPhasesFile;
    

    data Event a = MkEvent String (Interval a);

    instance BasedOn (Event a) where
    {
        type Base (Event a) = a;
    };
    
    nextEvent :: forall a. (DeltaSmaller a) => Item a -> Cut a -> a -> Maybe (Event a);
    nextEvent (MkItem name phase) cut limit = do
    {
        interval <- cutNextInterval phase cut limit;
        return (MkEvent name interval);
    };

    eventEndCut :: Event a -> Maybe (Cut a);
    eventEndCut (MkEvent _ (MkInterval _ (Ends cut))) = Just cut;
    eventEndCut _ = Nothing;

    allEvents :: forall a. (DeltaSmaller a) => Item a -> Cut a -> a -> [Event a];
    allEvents item cut limit = case nextEvent item cut limit of
    {
        Nothing -> [];
        Just event -> event : (case eventEndCut event of
        {
            Just endCut -> allEvents item endCut limit;
            _ -> [];
        });
    };
    
    compareEvents :: (Ord a) => Event a -> Event a -> Ordering;
    compareEvents (MkEvent _ i1) (MkEvent _ i2) = compare i1 i2;

    groupByFunc :: (Eq b) => (a -> b) -> [a] -> [(b,[a])];
    groupByFunc f [] = [];
    groupByFunc f aa@(a:_) = let
    {
        b = f a;
        (matching,rest) = span (\a' -> f a' == b) aa;
    } in (b,matching):(groupByFunc f rest); 

    getStartTime :: Event T -> Maybe T;
    getStartTime (MkEvent _ (MkInterval (Starts (MkCut t _)) _)) = Just t;
    getStartTime _ = Nothing;
    
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

    showEnd :: T -> T -> String;
    showEnd s e | localDay s == localDay e = show (localTimeOfDay e);
    showEnd s e | yearOfDay (localDay s) == yearOfDay (localDay e) = (showMonthDay (localDay e)) ++ " " ++ (show (localTimeOfDay e));
    showEnd _ e = show e;

    showEnding :: Interval T -> String;
    showEnding (MkInterval (Starts (MkCut s _)) (Ends (MkCut e _))) | s == e = "";
    showEnding (MkInterval start end) = " (until " ++ (showBasedOn (case start of
    {
        Starts (MkCut s _) -> showEnd s;
        _ -> show;
    }) end) ++ ")";

    showEvent :: Event T -> String;
    showEvent (MkEvent name interval) = name ++ (showEnding interval);

    isWholeDayStart :: Start T -> Maybe (Maybe Day);
    isWholeDayStart (Starts (MkCut t False)) | localTimeOfDay t == midnight = Just (Just (localDay t));
    isWholeDayStart Ongoing = Just Nothing;
    isWholeDayStart _ = Nothing;

    isWholeDayEnd :: End T -> Maybe (Maybe Day);
    isWholeDayEnd (Ends (MkCut t False)) | localTimeOfDay t == midnight = Just (Just (localDay t));
    isWholeDayEnd Whenever = Just Nothing;
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

    showWDInterval :: (Maybe Day,Maybe Day) -> String;
    showWDInterval (Just start,Just end) | end == addDays 1 start = "";
    showWDInterval (Just start,Just end) | yearOfDay start == yearOfDay end = " (to "++(showMonthDay (addDays (-1) end))++")";
    showWDInterval (_,Just end) = " (to "++(show (addDays (-1) end))++")";
    showWDInterval (_,Nothing) = " (to whenever)";

    showWDEvent :: ((Maybe Day,Maybe Day),Event T) -> String;
    showWDEvent (interval,MkEvent name _) = " " ++ name ++ (showWDInterval interval);
    
    printYearHeader :: Maybe Integer -> IO ();
    printYearHeader Nothing = return ();
    printYearHeader (Just year) = putStrLn ((show year) ++ "-");
    
    showDayHeader :: Maybe Day -> String;
    showDayHeader Nothing = "ongoing";
    showDayHeader (Just day) = showMonthDay day;

    printEvents :: [Event T] -> IO ();
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
   
    printItems :: T -> T -> [Item T] -> IO ();
    printItems t limit items = printEvents events where
    {
        events = mergeByListPresorted compareEvents (fmap (\phase -> allEvents phase (MkCut t False) limit) items);
    };
}
