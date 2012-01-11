module Data.TimePhase.Item where
{
    import Data.Char;
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
    
    showItems :: T -> T -> [Item] -> IO ();
    showItems t limit items = mapM_ ff items where
    {
        getNext (IntervalsPhase ints) = (ssFirstAfterUntil (intervalsStartOf ints) t limit,"");
        getNext (PointSetPhase (PointPCPSet set)) = (ssFirstAfterUntil set t limit,"");
        getNext (PointSetPhase (CoPointPCPSet set)) = (ssFirstAfterUntil set t limit,"");
    
        ff (MkItem name phase) = let
        {
            (mt,desc) = getNext phase;
        } in do
        {
            case mt of
            {
                Just t -> putStrLn ((show t) ++ ": " ++ name);
                Nothing -> return ();
            };
        };
    };
}
