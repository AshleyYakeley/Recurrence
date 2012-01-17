module Data.TimePhase.SExpression.Read where
{
    import Data.Maybe;
    import Data.Char;
    import Data.List;
    import Text.Read;
    import Text.ParserCombinators.ReadPrec;

    readMatching :: (Char -> Bool) -> ReadPrec Char;
    readMatching match = do
    {
        found <- get;
        if match found then return found else pfail;
    };
    
    readCollect :: (Char -> Maybe a) -> ReadPrec a;
    readCollect f = do
    {
        found <- get;
        case f found of
        {
            Just a -> return a;
            Nothing -> pfail;
        };
    };
    
    readIsEnd :: ReadPrec Bool;
    readIsEnd = (get >> return False) <++ (return True);
    
    readEnd :: ReadPrec ();
    readEnd = do
    {
        end <- readIsEnd;
        if end then return () else pfail;
    };
    
    readThis :: Char -> ReadPrec ();
    readThis expected = readMatching (\found -> expected == found) >> return ();
    
    readOneOrMore :: forall b. ReadPrec b -> ReadPrec [b];
    readOneOrMore reader = do
    {
        b <- reader;
        bs <- readZeroOrMore reader;
        return (b:bs);
    };
    
    readZeroOrMore :: forall b. ReadPrec b -> ReadPrec [b];
    readZeroOrMore reader = (readOneOrMore reader) <++ (return []);
    
    readZeroOrMore_ :: forall b. ReadPrec b -> ReadPrec ();
    readZeroOrMore_ reader = (do
        {
            _ <- reader;
            readZeroOrMore_ reader;
        }) <++ (return ());
    
    getDigit :: Char -> Maybe Int;
    getDigit c = if isDigit c then Just ((fromEnum c) - (fromEnum '0')) else Nothing;
    
    readDigit :: ReadPrec Int;
    readDigit = readCollect getDigit;
    
    isLineBreak :: Char -> Bool;
    isLineBreak '\r' = True;
    isLineBreak '\n' = True;
    isLineBreak c = case generalCategory c of
    {
        LineSeparator -> True;
        ParagraphSeparator -> True;
        _ -> False;
    };

    runRead :: ReadPrec a -> String -> Maybe a;
    runRead rp s = case mapMaybe (\(a,rest) -> case rest of
    {
        "" -> Just a;
        _ -> Nothing;
    }) (readPrec_to_S rp 0 s) of
    {
        [a] -> Just a;
        _ -> Nothing;
    };
}
