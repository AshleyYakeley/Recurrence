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

    groupEvents :: [Event T] -> [(Maybe Day,[(Maybe TimeOfDay,[Event T])])];
    groupEvents events = fmap (\(mday,dayevents) -> (mday,groupByFunc ((fmap localTimeOfDay) . getStartTime) dayevents))
     (groupByFunc ((fmap localDay) . getStartTime) events);
    
    showHeader :: Maybe Day -> IO ();
    showHeader mday = putStrLn (case mday of
    {
        Nothing -> "Ongoing";
        Just day -> show day;
    });
    
    pad2 :: String -> String;
    pad2 [] = "00";
    pad2 s@[_] = '0':s;
    pad2 s = s;
    
    showTimeOfDay :: TimeOfDay -> String;
    showTimeOfDay tod | todSec tod == 0 = (pad2 (show (todHour tod))) ++ ":" ++ (pad2 (show (todMin tod)));
    showTimeOfDay tod = show tod;

    showEvent :: Event T -> String;
    showEvent (MkEvent name (MkInterval start end)) = name ++ (case (start,end) of
    {
        (Starts (MkCut s _),Ends (MkCut e _)) | s == e -> "";
        _ -> " (until " ++ (showBasedOn show end) ++ ")";
    });

    showEvents :: [Event T] -> IO ();
    showEvents events = mapM_ (\(mday,dayevents) -> do
    {
        showHeader mday;
        mapM_ (\(mtod,timeevents) -> do
        {
            case mtod of
            {
                Just tod -> putStr ((showTimeOfDay tod) ++ " ");
                Nothing -> return ();
            };
            putStrLn (intercalate ", " (fmap showEvent timeevents));
        }) dayevents;
    }) (groupEvents events);
   
    showItems :: T -> T -> [Item T] -> IO ();
    showItems t limit items = showEvents events where
    {
        -- events = mapMaybe (\phase -> nextEvent phase (MkCut t False) limit) items;
        -- events = items >>= (\phase -> allEvents phase (MkCut t False) limit);
        events = mergeByListPresorted compareEvents (fmap (\phase -> allEvents phase (MkCut t False) limit) items);
    };
}
