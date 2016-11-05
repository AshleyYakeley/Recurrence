module Data.Recurrence.Calendar.Read(calendarFromString,calendarFromFile) where
{
    import System.IO;
    import Text.Read(readPrec);
    import Text.ParserCombinators.ReadPrec;
    import Data.Recurrence.Time;
    import Data.SExpression;
    import Data.SExpression.Read;
    import Data.Recurrence.SExpression;
    import Data.Recurrence.Calendar.Item;

    readRecurFile :: ReadPrec [SExpression Atom];
    readRecurFile = do
    {
        exps <- readZeroOrMore readPrec;
        readAnyWhiteSpace;
        return exps;
    };

    interpretItem :: (?now :: T) => SExpression Atom -> M Item;
    interpretItem (ListSExpression [AtomSExpression (IdentifierAtom name),defn]) = do
    {
        value <- evalWithDict defn;
        rc <- fromValue value;
        return (MkItem name rc);
    };
    interpretItem _ = reportError "S-expression not in correct format";

    calendarFromString :: (?now :: T,Monad m) => String -> m Calendar;
    calendarFromString text = case readPrec_to_S readRecurFile 0 text of
    {
        [(exprs,"")] -> case mapM interpretItem exprs of
        {
            Left err -> fail err;
            Right calendar -> return calendar;
        };
        [(_,rest)] -> fail ("unreadable: " ++ (show rest));
        _ -> fail ("unreadable: " ++ (show text));
    };

    calendarFromFile :: (?now :: T) => FilePath -> IO Calendar;
    calendarFromFile filepath = withFile filepath ReadMode $ \h -> hGetContents h >>= calendarFromString;
}
