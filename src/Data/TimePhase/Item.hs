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
    
    instance (Show a) => Show (Cut a) where
    {
        show (MkCut a False) = show a;
        show (MkCut a True) = "just after " ++ (show a); 
    };

    data Event a = MkEvent String (Maybe (Cut a)) (Maybe (Maybe (Cut a)));
    
    showEvent (MkEvent name mstart mmend) = (case mstart of
    {
        Nothing -> "now";
        Just start -> show start;
    }) ++ ": " ++ name ++ (case mmend of
    {
        Nothing -> "";
        Just Nothing -> " (until whenever)";
        Just (Just end) -> " (until "++(show end)++")";
    });
    
    nextEvent :: forall a. (DeltaSmaller a) => a -> a -> Item a -> Maybe (Event a);
    nextEvent t limit (MkItem name phase) = case eventCurrent phase t of
    {
        Just etype -> Just (MkEvent name Nothing (getCut t etype));
        Nothing -> do
        {
            (start,etype) <- eventStateFirstAfterUntil phase True t limit;
            return (MkEvent name (Just (MkCut start False)) (getCut start etype));
        };
    } where
    {
        getCut :: a -> EventType -> Maybe (Maybe (Cut a));
        getCut start ETChange = Just (do
        {
            (end,etype') <- eventStateFirstAfterUntil phase False start limit;
            return (MkCut end (etype' == ETLateChange));
        });
        getCut start ETLateChange = Just (do
        {
            (end,etype') <- eventStateFirstAfterUntil phase False start limit;
            return (MkCut end (etype' == ETLateChange));
        });
        getCut start ETPoint = Nothing;
    };
    
    showEvents :: [Event T] -> IO ();
    showEvents events = mapM_ ff events where
    {
        ff event = putStrLn (showEvent event);
    };
    
    showItems :: T -> T -> [Item T] -> IO ();
    showItems t limit items = showEvents events where
    {
        events = mapMaybe (nextEvent t limit) items;
    };
}
