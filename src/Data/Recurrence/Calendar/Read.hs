module Data.Recurrence.Calendar.Read(calendarFromString,calendarFromFile) where
{
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

    interpretItem :: Dict -> SExpression Atom -> M (Either Dict Item);
    interpretItem dict (ListSExpression [IdentifierSExpression "let",key,value]) = do
    {
        newdict <- evalBinding dict key value;
        return $ Left newdict;
    };
    interpretItem dict (ListSExpression [IdentifierSExpression name,defn]) = do
    {
        value <- eval dict defn;
        rc <- fromValue value;
        return $ Right $ MkItem name rc;
    };
    interpretItem _ _ = reportError "S-expression not in correct format";

    calendarFromExpressions :: Dict -> [SExpression Atom] -> M Calendar;
    calendarFromExpressions _dict [] = return [];
    calendarFromExpressions dict (expr:exprs) = do
    {
        edi <- interpretItem dict expr;
        case edi of
        {
            Left newdict -> calendarFromExpressions newdict exprs;
            Right item -> do
            {
                items <- calendarFromExpressions dict exprs;
                return $ item:items;
            }
        }
    };

    calendarFromString :: (?now :: T,Monad m) => String -> m Calendar;
    calendarFromString text = case readPrec_to_S readRecurFile 0 text of
    {
        [(exprs,"")] -> case calendarFromExpressions stdDict exprs of
        {
            Left err -> fail err;
            Right calendar -> return calendar;
        };
        [(_,rest)] -> fail ("unreadable: " ++ (show rest));
        _ -> fail ("unreadable: " ++ (show text));
    };

    calendarFromFile :: (?now :: T) => FilePath -> IO Calendar;
    calendarFromFile filepath = readFile filepath >>= calendarFromString;
}
