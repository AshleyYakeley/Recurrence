module Data.TimePhase.Read (readExpression) where
{
    import Data.Char;
    import Language.SExpression;
    import Text.ParserCombinators.ReadPrec;

    matchChar :: (Char -> Bool) -> ReadPrec Char;
    matchChar match = do
    {
        found <- get;
        if match found then return found else pfail;
    };
    
    isChar :: Char -> ReadPrec ();
    isChar expected = matchChar (\found -> expected == found) >> (return ());
    
    readAll :: forall b. ReadPrec b -> ReadPrec [b];
    readAll reader = (do
        {
            b <- reader;
            bs <- readAll reader;
            return (b:bs);
        }) <++ (return []);
    
    isGoodChar :: Char -> Bool;
    isGoodChar '"' = False;
    isGoodChar c = not (isSpace c);
    
    bsChar :: ReadPrec Char;
    bsChar = do
    {
        isChar '\\';
        get;
    };
    
    isInQuoteChar :: Char -> Bool;
    isInQuoteChar '"' = False;
    isInQuoteChar '\\' = False;
    isInQuoteChar _ = True;
    
    readQuoted :: ReadPrec String;
    readQuoted = do
    {
        isChar '"';
        s <- readAll (bsChar <++ (matchChar isInQuoteChar));
        isChar '"';
        return s;
    };
    
    readAtom :: ReadPrec String;
    readAtom = readQuoted <++ (readAll (matchChar isGoodChar));

    readExpression :: ReadPrec (SExpression String);
    readExpression = readSExpression readAtom;
}
