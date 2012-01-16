module Data.TimePhase.Item where
{
    import Data.Char;
    import Data.Maybe;
    import Language.SExpression;
    import Text.ParserCombinators.ReadPrec;
    import Data.SetSearch;
    import Data.TimePhase.Read;
    import Data.TimePhase.Value;
    import Data.TimePhase.Dict;
    import Data.TimePhase.Eval;
    
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
    
    data Cut a = MkCut a Bool;
    
    data Start a = Ongoing | Starts (Cut a);

    instance (Show a) => Show (Start a) where
    {
        show Ongoing = "ongoing";
        show (Starts (MkCut a False)) = show a;
        show (Starts (MkCut a True)) = "just after " ++ (show a); 
    };
    
    data End a = Whenever | Ends (Cut a);
    
    instance (Show a) => Show (End a) where
    {
        show Whenever = "whenever";
        show (Ends (MkCut a False)) = show a;
        show (Ends (MkCut a True)) = "including " ++ (show a);
    };

    data Event a = MkEvent String (Start a) (End a);
    
    showEvent :: (Eq a,Show a) => Event a -> String;
    showEvent (MkEvent name start end) = (show start) ++ ": " ++ name ++ (case (start,end) of
    {
        (Starts (MkCut s _),Ends (MkCut e _)) | s == e -> "";
        _ -> " (until " ++ (show end) ++ ")";
    });
    
    currentEvent :: forall a. (DeltaSmaller a) => Item a -> a -> a -> Maybe (Event a);
    currentEvent (MkItem name phase) t limit = if member phase t then Just (case eventCurrent phase t of
    {
        Just ETPoint -> MkEvent name (Starts (MkCut t False)) (Ends (MkCut t True));
        Just ETChange -> MkEvent name (Starts (MkCut t False)) getEnd;
        Just ETLateChange -> MkEvent name Ongoing (Ends (MkCut t True));
        Nothing -> MkEvent name Ongoing getEnd;
    })
    else Nothing where
    {
        getEnd :: End a;
        getEnd = case eventStateFirstAfterUntil phase False t limit of
        {
            Just (end,etype') -> Ends (MkCut end (etype' /= ETChange));
            Nothing -> Whenever;
        };
    };
{-    
    nextEvent :: forall a. (DeltaSmaller a) => a -> a -> Item a -> Maybe (Event a);
    nextEvent t limit (MkItem name phase) = case (member phase t,eventCurrent phase t) of
    {
        (True,Just ETPoint) -> Just (getEvent t ETPoint);
        (True,Just ETChange) -> Just (getEvent t ETChange);
        (False,Just ETLateChange) -> Just (getEvent t ETLateChange);

        (False,Just ETPoint) -> Just (getEvent t ETLateChange);
        (False,Just ETChange) -> do
        {
            (start,etype) <- eventStateFirstAfterUntil phase True t limit;
            return (getEvent start etype);
        };
        (True,Just ETLateChange) -> Just (MkEvent name Ongoing (getEnd t));

        (True,Nothing) -> Just (MkEvent name Ongoing (getEnd t));
        (False,Nothing) -> do
        {
            (start,etype) <- eventStateFirstAfterUntil phase True t limit;
            return (getEvent start etype);
        };
    } where
    {
        getEnd :: a -> End a;
        getEnd start = case eventStateFirstAfterUntil phase False start limit of
        {
            Just (end,etype') -> Ends (MkCut end (etype' == ETLateChange));
            Nothing -> Whenever;
        );
        
        getEvent :: a -> EventType -> Event a;
        getEvent start etype = case etype of
        {
            ETPoint -> MkEvent name (Starts (MkCut start False)) (Ends (MkCut start True));
            ETChange -> MkEvent name (Starts (MkCut start False)) (getEnd start);
            ETLateChange -> MkEvent name (Starts (MkCut start True)) (getEnd start);
        };
    };
-}    
    showEvents :: [Event T] -> IO ();
    showEvents events = mapM_ ff events where
    {
        ff event = putStrLn (showEvent event);
    };
    
    showItems :: T -> T -> [Item T] -> IO ();
    showItems t limit items = showEvents events where
    {
        events = mapMaybe (\phase -> currentEvent phase t limit) items;
    };
}
