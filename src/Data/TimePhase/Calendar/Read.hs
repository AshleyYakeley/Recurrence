module Data.TimePhase.Calendar.Read(calendarFromString,calendarFromFile) where
{
    import System.IO;
    import Text.ParserCombinators.ReadPrec;
    import Data.TimePhase.Time;
    import Data.SExpression;
    import Data.SExpression.Read;
    import Data.TimePhase.Atom;
    import Data.TimePhase.Read;
    import Data.TimePhase.Value;
    import Data.TimePhase.Eval;
    import Data.TimePhase.Calendar.Item;

    readPhasesFile :: ReadPrec [SExpression Atom];
    readPhasesFile = do
    {
        exps <- readZeroOrMore readExpression;
        readAnyWhiteSpace;
        return exps;
    };

    interpretItem :: (?now :: T) => SExpression Atom -> M Item;
    interpretItem (ListSExpression [AtomSExpression (IdentifierAtom name),defn]) = do
    {
        value <- evalWithDict defn;
        phase <- fromValue value;
        return (MkItem name phase);
    };
    interpretItem _ = reportError "S-expression not in correct format";

    readItems :: (?now :: T) => ReadPrec (M [Item]);
    readItems = fmap (mapM interpretItem) readPhasesFile;

    calendarFromString :: (?now :: T,Monad m) => String -> m Calendar;
    calendarFromString text = case readPrec_to_S readItems 0 text of
    {
        [(mitems,"")] -> case mitems of
        {
            Left err -> fail err;
            Right items -> return items;
        };
        [(_,rest)] -> fail ("unreadable: " ++ (show rest));
        _ -> fail ("unreadable: " ++ (show text));
    };

    calendarFromFile :: (?now :: T) => FilePath -> IO Calendar;
    calendarFromFile filepath = withFile filepath ReadMode $ \h -> hGetContents h >>= calendarFromString;
}
