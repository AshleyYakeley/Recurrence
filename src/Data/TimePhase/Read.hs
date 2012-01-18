module Data.TimePhase.Read (readExpression) where
{
    import Data.Char;
    import Data.Time;
    import Text.ParserCombinators.ReadPrec;
    import Data.SetSearch;
    import Data.TimePhase.Time;
    import Data.TimePhase.SExpression;
    import Data.TimePhase.SExpression.Read;
    import Data.TimePhase.Value;
    import Data.TimePhase.Atom;
    
    isGoodFirstChar :: Char -> Bool;
    isGoodFirstChar '_' = True;
    isGoodFirstChar c = isLetter c;
    
    isGoodRestChar :: Char -> Bool;
    isGoodRestChar '"' = False;
    isGoodRestChar '(' = False;
    isGoodRestChar ')' = False;
    isGoodRestChar '[' = False;
    isGoodRestChar ']' = False;
    isGoodRestChar c = not (isSpace c);

    readUnquotedIdentifier :: ReadPrec String;
    readUnquotedIdentifier = do
    {
        first <- (readMatching isGoodFirstChar);
        rest <- readZeroOrMore (readMatching isGoodRestChar);
        return (first:rest);
    };
    
    escapedChar :: ReadPrec Char;
    escapedChar = do
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
        s <- readZeroOrMore (escapedChar <++ (readMatching isInQuoteChar));
        readThis '"';
        return s;
    };

    readIdentifier :: ReadPrec String;
    readIdentifier = readQuoted <++ readUnquotedIdentifier;
    
    toNumber :: Int -> [Int] -> Int;
    toNumber n [] = n;
    toNumber n (d:ds) = toNumber (n * 10 + d) ds;
    
    readNumerals :: ReadPrec Int;
    readNumerals = fmap (toNumber 0) (readOneOrMore readDigit);

    {-
    -d
    -}
    readD :: ReadPrec (Intervals T);
    readD = do
    {
        readThis '-';
        day <- readNumerals;
        pfail; -- day;
    };
    
    {-
    -m-d
    -m-
    -}
    readM :: ReadPrec (Intervals T);
    readM = do
    {
        readThis '-';
        month <- readNumerals;
        readThis '-';
        mday <- readMaybe readNumerals;
        return (case mday of
        {
            Nothing -> isMonth month;
            Just day -> specialDays (maybeDayEachYear (\year -> fromGregorianValid year month day));
        });
    };
    
    {-
    y-m-d
    y-m-
    y-
    -}
    readY :: ReadPrec (Intervals T);
    readY = do
    {
        (year :: Integer) <- fmap fromIntegral readNumerals;
        readThis '-';
        mmonthmday <- readMaybe (do
        {
            month <- readNumerals;
            readThis '-';
            mday <- readMaybe readNumerals;
            return (month,mday);
        });
        return (case mmonthmday of
        {
            Nothing -> isYear year;
            Just (month,Nothing) -> fmap ((==) (year,month)) theYearAndMonth;
            Just (month,Just d) -> case fromGregorianValid year month d of
            {
                Just day -> fmap ((==) day) theDay;
                Nothing -> empty;
            };
        });
    };
    
    readBracketed :: ReadPrec TimePhase;
    readBracketed = do
    {
        readThis '[';
        readZeroOrMore_ (readMatching isSpace);
        dayIntervals <- readY <++ readM <++ readD;        
        readZeroOrMore_ (readMatching isSpace);
        readThis ']';
        return (toPhaseSet dayIntervals);
    };
    
    allowedName :: String -> Bool;
    allowedName ('_':_) = True;
    allowedName (c:_) | isLetter c = True;
    allowedName _ = False;
    
    durationType :: Char -> Maybe NominalDiffTime;
    durationType 's' = Just 1;
    durationType 'm' = Just 60;
    durationType 'h' = Just 3600;
    durationType 'd' = Just 86400;
    durationType 'w' = Just 604800;
    durationType _ = Nothing;
    
    readDurationPiece :: ReadPrec NominalDiffTime;
    readDurationPiece = do
    {
        n <- readNumerals;
        m <- readCollect durationType;
        return ((realToFrac n) * m);
    };
    
    readOptionalMinus :: ReadPrec Bool;
    readOptionalMinus = (readThis '-' >> return True) <++ (return False);
    
    readDuration :: ReadPrec NominalDiffTime;
    readDuration = do
    {
        minus <- readOptionalMinus;
        d <- fmap sum (readOneOrMore readDurationPiece);
        return (if minus then negate d else d);
    };

    readLiteral :: ReadPrec Value;
    readLiteral = (fmap toValue readDuration) <++ (fmap toValue readNumerals);

    readAtom :: ReadPrec Atom;
    readAtom = (fmap IdentifierAtom readIdentifier) <++ (fmap (LiteralAtom . toValue) readBracketed) <++ (fmap LiteralAtom readLiteral);

    readExpression :: ReadPrec (SExpression Atom);
    readExpression = readSExpression readAtom;
}
