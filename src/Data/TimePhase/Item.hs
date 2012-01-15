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
    
    data Item = MkItem String (Phase T);
    
    readPhasesFile :: ReadPrec [SExpression String];
    readPhasesFile = do
    {
        exps <- readZeroOrMore readExpression;
        readAnyWhiteSpace;
        return exps;
    };
    
    interpretItem :: SExpression String -> M Item;
    interpretItem (ListSExpression [AtomSExpression name,defn]) = do
    {
        value <- evalWithDict defn;
        phase <- fromValue value;
        return (MkItem name phase);
    };
    interpretItem _ = reportError "S-expression not in correct format";

    readItems :: ReadPrec (M [Item]);
    readItems = fmap (mapM interpretItem) readPhasesFile;
    
    data OrLater = MkOrLater T Bool;
    data Event = MkEvent String OrLater (Maybe (Maybe OrLater));
    
    instance Show OrLater where
    {
        show (MkOrLater t False) = show t;
        show (MkOrLater t True) = "just after " ++ (show t); 
    };
    
    showEvent :: Event -> String;
    showEvent (MkEvent name start mmend) = (show start) ++ ": " ++ name ++ (case mmend of
    {
        Nothing -> "";
        Just Nothing -> " (until whenever)";
        Just (Just end) -> " (until "++(show end)++")";
    });
    
    showEvents :: [Event] -> IO ();
    showEvents events = mapM_ ff events where
    {
        ff event = putStrLn (showEvent event);
    };
    
    toEvent :: T -> T -> Item -> Maybe Event;
    toEvent t limit (MkItem name phase) = do
    {
        (start,mmend) <- getNext phase;
        return (MkEvent name start mmend);
    } where
    {
        getNext :: Phase T -> Maybe (OrLater,Maybe (Maybe OrLater));
        getNext ps = do
        {
            (start,etype) <- eventStateFirstAfterUntil ps True t limit;
            case etype of
            {
                ETChange -> return (MkOrLater start False,Just (do
                {
                    (end,etype') <- eventStateFirstAfterUntil ps False start limit;
                    return (MkOrLater end (etype' == ETLateChange));
                }));
                ETLateChange -> return (MkOrLater start True,Just (do
                {
                    (end,etype') <- eventStateFirstAfterUntil ps False start limit;
                    return (MkOrLater end (etype' == ETLateChange));
                }));
                ETPoint -> return (MkOrLater start False,Nothing);
            };
        };
    };
    
    showItems :: T -> T -> [Item] -> IO ();
    showItems t limit items = showEvents events where
    {
        events = mapMaybe (toEvent t limit) items;
    };
}
