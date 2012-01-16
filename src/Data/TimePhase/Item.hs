module Data.TimePhase.Item where
{
    import Data.Char;
    import Data.Maybe;
    import Control.Monad;
    import Language.SExpression;
    import Text.ParserCombinators.ReadPrec;
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
        showBasedOn show (MkEvent name (MkInterval start end)) = (showBasedOn show start) ++ ": " ++ name ++ (case (start,end) of
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
    showEvents events = mapM_ ff events where
    {
        ff event = putStrLn (showBasedOn show event);
    };
    
    showItems :: T -> T -> [Item T] -> IO ();
    showItems t limit items = showEvents events where
    {
        -- events = mapMaybe (\phase -> nextEvent phase (MkCut t False) limit) items;
        -- events = items >>= (\phase -> allEvents phase (MkCut t False) limit);
        events = mergeByListPresorted compareEvents (fmap (\phase -> allEvents phase (MkCut t False) limit) items);
    };
}
