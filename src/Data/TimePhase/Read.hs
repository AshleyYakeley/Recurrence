module Data.TimePhase.Read (readExpression) where
{
    import Data.Char;
    import Language.SExpression;
    import Text.ParserCombinators.ReadPrec;
    
    isGoodChar :: Char -> Bool;
    isGoodChar '"' = False;
    isGoodChar c = not (isSpace c);
    
    bsChar :: ReadPrec Char;
    bsChar = do
    {
        readThis '\\';
        get;
    };
    
    isInQuoteChar :: Char -> Bool;
    isInQuoteChar '"' = False;
    isInQuoteChar '\\' = False;
    isInQuoteChar _ = True;
    
    readQuoted :: ReadPrec String;
    readQuoted = do
    {
        readThis '"';
        s <- readZeroOrMore (bsChar <++ (readMatching isInQuoteChar));
        readThis '"';
        return s;
    };
    
    readAtom :: ReadPrec String;
    readAtom = readQuoted <++ (readOneOrMore (readMatching isGoodChar));

    readExpression :: ReadPrec (SExpression String);
    readExpression = readSExpression readAtom;
}
