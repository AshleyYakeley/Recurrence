module Data.TimePhase.Atom(allowedName,evalAtom) where
{
    import Data.Char;
    import Data.Time;
    import Text.ParserCombinators.ReadPrec;
    import Data.TimePhase.SExpression.Read;
    import Data.TimePhase.Value;
    
    allowedName :: String -> Bool;
    allowedName ('_':_) = True;
    allowedName (c:_) | isLetter c = True;
    allowedName _ = False;
    
    toNumber :: Int -> [Int] -> Int;
    toNumber n [] = n;
    toNumber n (d:ds) = toNumber (n * 10 + d) ds;
    
    readNumerals :: ReadPrec Int;
    readNumerals = fmap (toNumber 0) (readOneOrMore readDigit);
    
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
    
    readAtom :: ReadPrec Value;
    readAtom = (fmap toValue readNumerals) +++ (fmap toValue readDuration);
    
    evalAtom :: String -> Maybe Value;
    evalAtom = runRead readAtom;
}

