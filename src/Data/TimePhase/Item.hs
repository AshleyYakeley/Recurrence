module Data.TimePhase.Item where
{
    import Data.Char;
    import Data.Maybe;
    import Control.Monad;
    import Language.SExpression;
    import Text.ParserCombinators.ReadPrec;
    import Data.Time;
    import Data.SetSearch;
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
    

    data Item a = MkItem String (Phase a);
    
    readPhasesFile :: ReadPrec [SExpression String];
    readPhasesFile = do
    {
        exps <- readZeroOrMore readExpression;
        readAnyWhiteSpace;
        return exps;
    };
    
    interpretItem :: SExpression String -> M (Item T);
    interpretItem (ListSExpression [AtomSExpression name,defn]) = do
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
    
    instance (Eq a) => ShowBasedOn (Event a) where
    {
        showBasedOn show (MkEvent name (MkInterval start end)) = (showBasedOn show start) ++ " " ++ name ++ (case (start,end) of
        {
            (Starts (MkCut s _),Ends (MkCut e _)) | s == e -> "";
            _ -> " (until " ++ (showBasedOn show end) ++ ")";
        });
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

    showEvents :: [Event T] -> IO ();
    showEvents [] = return ();
    showEvents ee = showNew Nothing ee where
    {
        getDay :: Event T -> Maybe Day;
        getDay (MkEvent _ (MkInterval (Starts (MkCut t _)) _)) = Just (localDay t);
        getDay _ = Nothing;
    
        showNew :: Maybe Day -> [Event T] -> IO ();
        showNew mday ee = do
        {
            putStrLn (case mday of
            {
                Nothing -> "Ongoing";
                Just day -> show day;
            });
            showOngoing mday ee;
        };
    
        pad2 :: String -> String;
        pad2 [] = "00";
        pad2 s@[_] = '0':s;
        pad2 s = s;
    
        showTimeOfDay :: TimeOfDay -> String;
        showTimeOfDay tod | todSec tod == 0 = (pad2 (show (todHour tod))) ++ ":" ++ (pad2 (show (todMin tod)));
        showTimeOfDay tod = show tod;
    
        showTime :: Maybe Day -> T -> String;
        showTime mday t = let
        {
            day = localDay t;
            tod = localTimeOfDay t;
        } in if mday == Just day then showTimeOfDay tod else (show day) ++ " " ++ (showTimeOfDay tod);
    
        showOngoing :: Maybe Day -> [Event T] -> IO ();
        showOngoing _ [] = return ();
        showOngoing mday ee@(e:_) | mday /= (getDay e) = showNew (getDay e) ee;
        showOngoing mday (e:es) = do
        {
            putStrLn (showBasedOn (showTime mday) e);
            showOngoing mday es;
        };
    };
    
    showItems :: T -> T -> [Item T] -> IO ();
    showItems t limit items = showEvents events where
    {
        -- events = mapMaybe (\phase -> nextEvent phase (MkCut t False) limit) items;
        -- events = items >>= (\phase -> allEvents phase (MkCut t False) limit);
        events = mergeByListPresorted compareEvents (fmap (\phase -> allEvents phase (MkCut t False) limit) items);
    };
}
