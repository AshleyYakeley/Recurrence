module Data.Recurrence.SExpression.Atom(Atom(..)) where
{
    import Data.Char;
    import Data.Fixed;
    import Data.Time;
    import Text.Read(Read(..));
    import Text.ParserCombinators.ReadPrec;
    import Data.SetSearch;
    import Data.SExpression.Read;
    import Data.Recurrence.Time;
    import Data.Recurrence.SExpression.Value;


    data Atom = IdentifierAtom String | LiteralAtom Value;
    instance Show Atom where
    {
        show (IdentifierAtom s) = show s;
        show (LiteralAtom v) = show v;
    };

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

    toNumber :: Integer -> [Int] -> Integer;
    toNumber n [] = n;
    toNumber n (d:ds) = toNumber (n * 10 + (fromIntegral d)) ds;

    readNatural :: ReadPrec Integer;
    readNatural = fmap (toNumber 0) (readOneOrMore readDigit);

    toDecimalPart :: [Int] -> Pico;
    toDecimalPart [] = 0;
    toDecimalPart (d:ds) = ((realToFrac d) + (toDecimalPart ds)) / 10;

    readDecimal :: ReadPrec Pico;
    readDecimal = do
    {
        nat  <- readNatural;
        mdec <- readMaybe (do
        {
            readThis '.';
            fmap toDecimalPart (readOneOrMore readDigit);
        });
        return $ case mdec of
        {
            Just dec -> (realToFrac nat) + dec;
            Nothing -> realToFrac nat;
        };
    };

    {-
    -d
    -}
    readD :: ReadPrec (PieceSet T);
    readD = do
    {
        readThis '-';
        d <- readNatural;
        return $ isDayOfMonth $ fromIntegral d;
    };

    {-
    -m-d
    -m-
    -}
    readM :: ReadPrec (PieceSet T);
    readM = do
    {
        readThis '-';
        month <- fmap fromIntegral readNatural;
        readThis '-';
        mday <- readMaybe $ fmap fromIntegral readNatural;
        return $ case mday of
        {
            Nothing -> isMonthOfYear month;
            Just day -> isMonthAndDayOfYear month day;
        };
    };

    {-
    y-m-d
    y-m-
    y-
    -}
    readY :: ReadPrec (PieceSet T);
    readY = do
    {
        year <- readNatural;
        readThis '-';
        mmonthmday <- readMaybe (do
        {
            month <- fmap fromIntegral readNatural;
            readThis '-';
            mday <- readMaybe (fmap fromIntegral readNatural);
            return (month,mday);
        });
        return (case mmonthmday of
        {
            Nothing -> isSingleYear year;
            Just (month,Nothing) -> isSingleMonth year month;
            Just (month,Just d) -> case fromGregorianValid year month d of
            {
                Just day -> isSingleDay day;
                Nothing -> empty;
            };
        });
    };

    readDayFormat :: ReadPrec (PieceSet T);
    readDayFormat = readY <++ readM <++ readD;

    readTimeOfDayFormat :: ReadPrec (PointSet T);
    readTimeOfDayFormat = do
    {
        h <- fmap fromIntegral readNatural;
        readThis ':';
        m <- fmap fromIntegral readNatural;
        s <- (do
        {
            readThis ':';
            readDecimal;
        }) <++ (return 0);
        return (isTimeOfDay (TimeOfDay h m s));
    };

    readBracketed :: ReadPrec Recurrence;
    readBracketed = do
    {
        readThis '[';
        readZeroOrMore_ (readMatching isSpace);
        rc <- (do
        {
            df <- readDayFormat;
            mtf <- readMaybe (do
            {
                (readOneOrMore_ (readMatching isSpace)) <++ (readThis 'T');
                readTimeOfDayFormat;
            });
            return (case mtf of
            {
                Just tf -> InstantRecurrence $ pointIntersectPiece df tf;
                Nothing -> PeriodRecurrence $ let {?first=firstTime} in piecePartialIdentifySet df;
            });
        }) <++ fmap InstantRecurrence readTimeOfDayFormat;
        readZeroOrMore_ (readMatching isSpace);
        readThis ']';
        return rc;
    };
{-
    allowedName :: String -> Bool;
    allowedName ('_':_) = True;
    allowedName (c:_) | isLetter c = True;
    allowedName _ = False;
-}
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
        n <- readNatural;
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
    readLiteral = (fmap toValue readDuration) <++ (fmap toValue readNatural);

    readAtom :: ReadPrec Atom;
    readAtom = (fmap IdentifierAtom readIdentifier) <++ (fmap (LiteralAtom . toValue) readBracketed) <++ (fmap LiteralAtom readLiteral);

    instance Read Atom where
    {
        readPrec = readAtom;
    };
}
