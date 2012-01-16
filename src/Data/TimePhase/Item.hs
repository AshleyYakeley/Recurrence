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
    
    data Start a = Ongoing | Starts (Cut a);

    instance (Eq a) => Eq (Start a) where
    {
        Ongoing == Ongoing = True;
        (Starts a1) == (Starts a2) = a1 == a2;
        _ == _ = False;
    };

    instance (Ord a) => Ord (Start a) where
    {
        compare Ongoing Ongoing = EQ;
        compare Ongoing (Starts _) = LT;
        compare (Starts _) Ongoing = GT;
        compare (Starts a1) (Starts a2) = compare a1 a2;
    };

    instance (Show a) => Show (Start a) where
    {
        show Ongoing = "ongoing";
        show (Starts (MkCut a False)) = show a;
        show (Starts (MkCut a True)) = "just after " ++ (show a); 
    };
    
    data End a = Whenever | Ends (Cut a);

    instance (Eq a) => Eq (End a) where
    {
        Whenever == Whenever = True;
        (Ends a1) == (Ends a2) = a1 == a2;
        _ == _ = False;
    };

    instance (Ord a) => Ord (End a) where
    {
        compare Whenever Whenever = EQ;
        compare Whenever (Ends _) = GT;
        compare (Ends _) Whenever = LT;
        compare (Ends a1) (Ends a2) = compare a1 a2;
    };
    
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
    
    nextEvent :: forall a. (DeltaSmaller a) => Item a -> Cut a -> a -> Maybe (Event a);
    nextEvent (MkItem name phase) cut limit = if cutAfterMember phase cut
     then Just (MkEvent name
        (if cutCurrent phase cut then Starts cut else Ongoing)
        (getEnd cut)
     )
     else do
    {
        startcut <- cutFirstAfterUntil phase cut limit;
        return (MkEvent name (Starts startcut) (getEnd startcut));
    } where
    {
        getEnd c = case cutFirstAfterUntil phase c limit of
        {
            Just end -> Ends end;
            Nothing -> Whenever;
        };
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
