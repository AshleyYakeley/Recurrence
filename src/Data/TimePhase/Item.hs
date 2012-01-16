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
    
    showEvent :: (Eq a,Show a) => Event a -> String;
    showEvent (MkEvent name (MkInterval start end)) = (show start) ++ ": " ++ name ++ (case (start,end) of
    {
        (Starts (MkCut s _),Ends (MkCut e _)) | s == e -> "";
        _ -> " (until " ++ (show end) ++ ")";
    });
    
    nextEvent :: forall a. (DeltaSmaller a) => Item a -> Cut a -> a -> Maybe (Event a);
    nextEvent (MkItem name phase) cut limit = do
    {
        interval <- cutNextInterval phase cut limit;
        return (MkEvent name interval);
    };

    showEvents :: [Event T] -> IO ();
    showEvents events = mapM_ ff events where
    {
        ff event = putStrLn (showEvent event);
    };
    
    showItems :: T -> T -> [Item T] -> IO ();
    showItems t limit items = showEvents events where
    {
        events = mapMaybe (\phase -> nextEvent phase (MkCut t False) limit) items;
    };
}
